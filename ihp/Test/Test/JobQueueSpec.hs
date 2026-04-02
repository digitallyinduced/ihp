module Test.JobQueueSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.Job.Queue as JobQueue
import IHP.ModelSupport (createModelContext, releaseModelContext, HasqlError (..))
import System.Log.FastLogger (FastLogger)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.STM (atomically, newTBQueue)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)

data TestContext = TestContext
    { logger :: FastLogger
    }

tests :: Spec
tests = do
    describe "IHP.Job.Queue" do
        it "recreates missing triggers when poller repair is enabled" do
            withJobWatcher True \pool -> do
                dropNotificationTriggers pool testTableName
                didRecover <- waitUntil 6_000_000 (JobQueue.notificationTriggersHealthy pool testTableName)
                didRecover `shouldBe` True

        it "does not recreate missing triggers when poller repair is disabled" do
            withJobWatcher False \pool -> do
                dropNotificationTriggers pool testTableName
                Concurrent.threadDelay 3_000_000
                healthy <- JobQueue.notificationTriggersHealthy pool testTableName
                healthy `shouldBe` False

        it "keeps polling after trigger repair errors and recovers once table exists again" do
            withJobWatcher True \pool -> do
                dropTestTable pool testTableName
                Concurrent.threadDelay 1_500_000
                createTestTable pool testTableName
                didRecover <- waitUntil 6_000_000 (JobQueue.notificationTriggersHealthy pool testTableName)
                didRecover `shouldBe` True

        it "recreates missing triggers for long table names when poller repair is enabled" do
            withJobWatcherForTable True longTestTableName \pool -> do
                dropNotificationTriggers pool longTestTableName
                didRecover <- waitUntil 6_000_000 (JobQueue.notificationTriggersHealthy pool longTestTableName)
                didRecover `shouldBe` True

withJobWatcher :: Bool -> (HasqlPool.Pool -> IO ()) -> IO ()
withJobWatcher enablePollerTriggerRepair =
    withJobWatcherForTable enablePollerTriggerRepair testTableName

withJobWatcherForTable :: Bool -> Text -> (HasqlPool.Pool -> IO ()) -> IO ()
withJobWatcherForTable enablePollerTriggerRepair tableName action = do
    withDB \modelContext logger databaseUrl -> do
        let ?context = TestContext { logger = logger }
        let pool = modelContext.hasqlPool

        Exception.finally
            (do
                dropTestArtifacts pool tableName
                createTestTable pool tableName

                PGListener.withPGListener databaseUrl logger \pgListener -> do
                    runResourceT do
                        queue <- liftIO (atomically (newTBQueue 32))
                        (subscription, _) <- JobQueue.watchForJobWithPollerTriggerRepair enablePollerTriggerRepair pool pgListener tableName 100000 queue
                        liftIO (action pool `Exception.finally` PGListener.unsubscribe subscription pgListener))
            (dropTestArtifacts pool tableName)

withDB :: (ModelContext -> FastLogger -> ByteString -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    let logger = (\_ -> pure ()) :: FastLogger
    modelContext <- createModelContext databaseUrl logger
    result <- Exception.try (action modelContext logger databaseUrl `Exception.finally` releaseModelContext modelContext)
    case result of
        Right () -> pure ()
        Left (HasqlError (HasqlPool.ConnectionUsageError _)) ->
            pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
        Left e -> Exception.throwIO e

testTableName :: Text
testTableName = "job_queue_spec_jobs"

longTestTableName :: Text
longTestTableName = "job_queue_spec_jobs_with_a_very_long_table_name_for_trigger_truncation_regression_1234567890"

insertTriggerName :: Text -> Text
insertTriggerName tableName = "did_insert_job_" <> tableName

updateTriggerName :: Text -> Text
updateTriggerName tableName = "did_update_job_" <> tableName

triggerFunctionName :: Text -> Text
triggerFunctionName tableName = "notify_job_queued_" <> tableName

createTestTable :: HasqlPool.Pool -> Text -> IO ()
createTestTable pool tableName = do
    runScript pool $
        "CREATE TABLE IF NOT EXISTS \"" <> tableName <> "\" ("
        <> " id UUID PRIMARY KEY,"
        <> " status TEXT DEFAULT 'job_status_not_started' NOT NULL,"
        <> " locked_by UUID DEFAULT NULL,"
        <> " run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,"
        <> " created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL"
        <> " );"

dropTestTable :: HasqlPool.Pool -> Text -> IO ()
dropTestTable pool tableName =
    runScript pool ("DROP TABLE IF EXISTS \"" <> tableName <> "\" CASCADE;")

dropNotificationTriggers :: HasqlPool.Pool -> Text -> IO ()
dropNotificationTriggers pool tableName =
    runScript pool $
        "DROP TRIGGER IF EXISTS " <> insertTriggerName tableName <> " ON \"" <> tableName <> "\";"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName tableName <> " ON \"" <> tableName <> "\";"

dropTestArtifacts :: HasqlPool.Pool -> Text -> IO ()
dropTestArtifacts pool tableName =
    runScript pool $
        "DROP TABLE IF EXISTS \"" <> tableName <> "\" CASCADE;"
        <> "DROP FUNCTION IF EXISTS " <> triggerFunctionName tableName <> "() CASCADE;"

runScript :: HasqlPool.Pool -> Text -> IO ()
runScript pool sql = do
    result <- HasqlPool.use pool (HasqlSession.script sql)
    case result of
        Left err -> Exception.throwIO (HasqlError err)
        Right () -> pure ()

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
