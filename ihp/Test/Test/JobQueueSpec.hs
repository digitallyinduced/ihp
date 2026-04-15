module Test.JobQueueSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.Job.Queue as JobQueue
import IHP.ModelSupport (createModelContext, releaseModelContext, HasqlError (..))
import qualified IHP.Log as Log
import IHP.Log.Types (Logger, LogLevel (..), LoggerSettings (..))
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.STM (atomically, newTBQueue)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.UUID (UUID)
import Data.List (sort)

data TestContext = TestContext
    { logger :: Logger
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

    describe "IHP.Job.Queue.recoverStaleJobsForTable" do
        it "resets a stuck running job and returns its previous worker uuid" do
            withStaleRecoveryTable \pool -> do
                workerUuid <- UUID.nextRandom
                jobId <- UUID.nextRandom
                insertRunningJob pool staleRecoveryTableName jobId workerUuid

                (count, recoveredUuids) <- JobQueue.recoverStaleJobsForTable pool staleRecoveryTableName 0

                count `shouldBe` 1
                recoveredUuids `shouldBe` [workerUuid]

                -- A second sweep should be a no-op since the row is no longer running
                (count2, uuids2) <- JobQueue.recoverStaleJobsForTable pool staleRecoveryTableName 0
                count2 `shouldBe` 0
                uuids2 `shouldBe` []

        it "returns previous worker uuids for multiple stale rows" do
            withStaleRecoveryTable \pool -> do
                workerA <- UUID.nextRandom
                workerB <- UUID.nextRandom
                jobA <- UUID.nextRandom
                jobB <- UUID.nextRandom
                insertRunningJob pool staleRecoveryTableName jobA workerA
                insertRunningJob pool staleRecoveryTableName jobB workerB

                (count, recoveredUuids) <- JobQueue.recoverStaleJobsForTable pool staleRecoveryTableName 0

                count `shouldBe` 2
                sort recoveredUuids `shouldBe` sort [workerA, workerB]

        it "leaves rows alone when their locked_at is younger than the threshold" do
            withStaleRecoveryTable \pool -> do
                workerUuid <- UUID.nextRandom
                jobId <- UUID.nextRandom
                insertRunningJob pool staleRecoveryTableName jobId workerUuid

                -- Threshold of 1 hour: the row we just inserted is much younger,
                -- so nothing should be recovered.
                (count, recoveredUuids) <- JobQueue.recoverStaleJobsForTable pool staleRecoveryTableName 3600

                count `shouldBe` 0
                recoveredUuids `shouldBe` []

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

staleRecoveryTableName :: Text
staleRecoveryTableName = "stale_job_recovery_spec_jobs"

withStaleRecoveryTable :: (HasqlPool.Pool -> IO ()) -> IO ()
withStaleRecoveryTable action =
    withDB \modelContext _ _ -> do
        let pool = modelContext.hasqlPool
        Exception.finally
            (do
                dropTestTable pool staleRecoveryTableName
                createTestTable pool staleRecoveryTableName
                action pool)
            (dropTestTable pool staleRecoveryTableName)

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
        <> " locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,"
        <> " last_error TEXT DEFAULT NULL,"
        <> " run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,"
        <> " created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL"
        <> " );"

insertRunningJob :: HasqlPool.Pool -> Text -> UUID -> UUID -> IO ()
insertRunningJob pool tableName jobId workerUuid =
    runScript pool $
        "INSERT INTO \"" <> tableName <> "\""
        <> " (id, status, locked_by, locked_at)"
        <> " VALUES ('" <> UUID.toText jobId <> "',"
        <> " 'job_status_running',"
        <> " '" <> UUID.toText workerUuid <> "',"
        <> " NOW());"

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
