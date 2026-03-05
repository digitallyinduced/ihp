module Test.JobQueueSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Job.Queue (watchForJobWithPollerTriggerRepair)
import IHP.ModelSupport (ModelContext, createModelContext, releaseModelContext, HasqlError (..))
import qualified IHP.Log as Log
import IHP.Log.Types (Logger, LogLevel (..), LoggerSettings (..))
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.STM (atomically, newTBQueue)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import Data.Functor.Contravariant (contramap)

data TestContext = TestContext
    { logger :: Logger
    }

tests :: Spec
tests = do
    describe "IHP.Job.Queue" do
        it "recreates missing triggers when poller repair is enabled" do
            withJobWatcher True \pool -> do
                dropNotificationTriggers pool
                didRecover <- waitUntil 6_000_000 (notificationTriggersHealthy pool)
                didRecover `shouldBe` True

        it "does not recreate missing triggers when poller repair is disabled" do
            withJobWatcher False \pool -> do
                dropNotificationTriggers pool
                Concurrent.threadDelay 3_000_000
                healthy <- notificationTriggersHealthy pool
                healthy `shouldBe` False

        it "keeps polling after trigger repair errors and recovers once table exists again" do
            withJobWatcher True \pool -> do
                dropTestTable pool
                Concurrent.threadDelay 1_500_000
                createTestTable pool
                didRecover <- waitUntil 6_000_000 (notificationTriggersHealthy pool)
                didRecover `shouldBe` True

withJobWatcher :: Bool -> (HasqlPool.Pool -> IO ()) -> IO ()
withJobWatcher enablePollerTriggerRepair action = do
    withDB \modelContext logger databaseUrl -> do
        let ?context = TestContext { logger = logger }
        let pool = modelContext.hasqlPool

        Exception.finally
            (do
                dropTestArtifacts pool
                createTestTable pool

                PGListener.withPGListener databaseUrl logger \pgListener -> do
                    runResourceT do
                        queue <- liftIO (atomically (newTBQueue 32))
                        (subscription, _) <- watchForJobWithPollerTriggerRepair enablePollerTriggerRepair pool pgListener testTableName 100000 queue
                        liftIO (action pool `Exception.finally` PGListener.unsubscribe subscription pgListener))
            (dropTestArtifacts pool)

withDB :: (ModelContext -> Logger -> ByteString -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    logger <- Log.newLogger def { level = Warn }
    modelContext <- createModelContext databaseUrl logger
    result <- Exception.try (action modelContext logger databaseUrl `Exception.finally` releaseModelContext modelContext)
    case result of
        Right () -> pure ()
        Left (HasqlError (HasqlPool.ConnectionUsageError _)) ->
            pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
        Left e -> Exception.throwIO e

testTableName :: Text
testTableName = "job_queue_spec_jobs"

insertTriggerName :: Text
insertTriggerName = "did_insert_job_" <> testTableName

updateTriggerName :: Text
updateTriggerName = "did_update_job_" <> testTableName

triggerFunctionName :: Text
triggerFunctionName = "notify_job_queued_" <> testTableName

createTestTable :: HasqlPool.Pool -> IO ()
createTestTable pool = do
    runScript pool $
        "CREATE TABLE IF NOT EXISTS \"" <> testTableName <> "\" ("
        <> " id UUID PRIMARY KEY,"
        <> " status TEXT DEFAULT 'job_status_not_started' NOT NULL,"
        <> " locked_by UUID DEFAULT NULL,"
        <> " run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,"
        <> " created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL"
        <> " );"

dropTestTable :: HasqlPool.Pool -> IO ()
dropTestTable pool =
    runScript pool ("DROP TABLE IF EXISTS \"" <> testTableName <> "\" CASCADE;")

dropNotificationTriggers :: HasqlPool.Pool -> IO ()
dropNotificationTriggers pool =
    runScript pool $
        "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON \"" <> testTableName <> "\";"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON \"" <> testTableName <> "\";"

dropTestArtifacts :: HasqlPool.Pool -> IO ()
dropTestArtifacts pool =
    runScript pool $
        "DROP TABLE IF EXISTS \"" <> testTableName <> "\" CASCADE;"
        <> "DROP FUNCTION IF EXISTS " <> triggerFunctionName <> "() CASCADE;"

notificationTriggersHealthy :: HasqlPool.Pool -> IO Bool
notificationTriggersHealthy pool = do
    let sql = "SELECT COUNT(*) FROM pg_trigger t"
            <> " JOIN pg_class c ON t.tgrelid = c.oid"
            <> " JOIN pg_namespace n ON c.relnamespace = n.oid"
            <> " WHERE n.nspname = current_schema()"
            <> " AND c.relname = $1"
            <> " AND NOT t.tgisinternal"
            <> " AND (t.tgname = $2 OR t.tgname = $3)"
    let encoder =
            contramap (\(tableName, _, _) -> tableName) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_, insertTriggerName, _) -> insertTriggerName) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_, _, updateTriggerName) -> updateTriggerName) (Encoders.param (Encoders.nonNullable Encoders.text))
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    let statement = Hasql.unpreparable sql encoder decoder
    count :: Int <- fromIntegral <$> runStatement pool (testTableName, insertTriggerName, updateTriggerName) statement
    pure (count == 2)

runScript :: HasqlPool.Pool -> Text -> IO ()
runScript pool sql = do
    result <- HasqlPool.use pool (HasqlSession.script sql)
    case result of
        Left err -> Exception.throwIO (HasqlError err)
        Right () -> pure ()

runStatement :: HasqlPool.Pool -> params -> Hasql.Statement params result -> IO result
runStatement pool params statement = do
    result <- HasqlPool.use pool (HasqlSession.statement params statement)
    case result of
        Left err -> Exception.throwIO (HasqlError err)
        Right value -> pure value

waitUntil :: Int -> IO Bool -> IO Bool
waitUntil timeoutInMicroseconds predicate = loop 0
    where
        interval = 100000
        loop elapsed
            | elapsed >= timeoutInMicroseconds = pure False
            | otherwise = do
                value <- predicate
                if value
                    then pure True
                    else do
                        Concurrent.threadDelay interval
                        loop (elapsed + interval)
