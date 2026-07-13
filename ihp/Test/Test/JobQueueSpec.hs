module Test.JobQueueSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.Job.Queue as JobQueue
import IHP.Job.Queue.Pool (runPool)
import IHP.ModelSupport (createModelContext, releaseModelContext, HasqlError (..), noopLogger)
import System.Log.FastLogger (FastLogger)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent.STM (atomically, newTBQueue)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import System.Timeout (timeout)

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

        it "retries a failed startup install with the default watcher" do
            withJobWatcherForMissingTable testTableName \pool -> do
                createTestTable pool testTableName
                didRecover <- waitUntil 6_000_000 (JobQueue.notificationTriggersHealthy pool testTableName)
                didRecover `shouldBe` True

        it "creates only the missing trigger while an ACCESS SHARE lock is held" do
            withJobWatcher False \pool -> do
                dropUpdateNotificationTrigger pool testTableName
                lock <- Async.async (holdAccessShareLock pool testTableName)
                Exception.finally
                    (do
                        didAcquireLock <- waitUntil 2_000_000 (accessShareLockHeld pool testTableName)
                        didAcquireLock `shouldBe` True

                        repairResult <- timeout 2_000_000 (ensureTestNotificationTriggers pool testTableName)
                        repairResult `shouldBe` Just ()
                        lockState <- Async.poll lock
                        case lockState of
                            Nothing -> pure ()
                            Just _ -> expectationFailure "ACCESS SHARE lock was released before trigger repair completed"
                        JobQueue.notificationTriggersHealthy pool testTableName `shouldReturn` True)
                    (Async.cancel lock)

        it "does not wait for another trigger installer" do
            withJobWatcher False \pool -> do
                lock <- Async.async (holdTriggerInstallLock pool testTableName)
                Exception.finally
                    (do
                        didAcquireLock <- waitUntil 2_000_000 (triggerInstallLockHeld pool)
                        didAcquireLock `shouldBe` True

                        let install = runScript pool (JobQueue.createNotificationTriggerSQL (cs testTableName))
                        installResult <- timeout 1_000_000 (Exception.try install :: IO (Either HasqlError ()))
                        case installResult of
                            Just (Left _) -> pure ()
                            Just (Right _) -> expectationFailure "concurrent trigger installation unexpectedly succeeded"
                            Nothing -> expectationFailure "concurrent trigger installation waited for the advisory lock")
                    (Async.cancel lock)

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

withJobWatcherForMissingTable :: Text -> (HasqlPool.Pool -> IO ()) -> IO ()
withJobWatcherForMissingTable tableName action = do
    withDB \modelContext logger databaseUrl -> do
        let ?context = TestContext { logger = logger }
        let pool = modelContext.hasqlPool

        Exception.finally
            (do
                dropTestArtifacts pool tableName

                PGListener.withPGListener databaseUrl logger \pgListener -> do
                    runResourceT do
                        queue <- liftIO (atomically (newTBQueue 32))
                        (subscription, _) <- JobQueue.watchForJob pool pgListener tableName 100000 queue
                        liftIO (action pool `Exception.finally` PGListener.unsubscribe subscription pgListener))
            (dropTestArtifacts pool tableName)

withDB :: (ModelContext -> FastLogger -> ByteString -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    let logger = noopLogger
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

dropUpdateNotificationTrigger :: HasqlPool.Pool -> Text -> IO ()
dropUpdateNotificationTrigger pool tableName =
    runScript pool ("DROP TRIGGER IF EXISTS " <> updateTriggerName tableName <> " ON \"" <> tableName <> "\";")

holdAccessShareLock :: HasqlPool.Pool -> Text -> IO ()
holdAccessShareLock pool tableName =
    runScript pool $
        "BEGIN;"
        <> "SET LOCAL application_name = 'ihp_job_queue_access_share_test';"
        <> "LOCK TABLE \"" <> tableName <> "\" IN ACCESS SHARE MODE;"
        <> "SELECT pg_sleep(3);"
        <> "ROLLBACK;"

accessShareLockHeld :: HasqlPool.Pool -> Text -> IO Bool
accessShareLockHeld pool tableName = do
    let sql = "SELECT EXISTS ("
            <> " SELECT 1 FROM pg_locks l"
            <> " JOIN pg_stat_activity a ON a.pid = l.pid"
            <> " WHERE l.relation = $1::regclass"
            <> " AND l.mode = 'AccessShareLock'"
            <> " AND l.granted"
            <> " AND a.application_name = 'ihp_job_queue_access_share_test')"
    let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    let statement = Hasql.unpreparable sql encoder decoder
    runPool pool (HasqlSession.statement tableName statement)

holdTriggerInstallLock :: HasqlPool.Pool -> Text -> IO ()
holdTriggerInstallLock pool tableName = do
    let functionName = triggerFunctionName tableName
    runScript pool $
        "BEGIN;"
        <> "SET LOCAL application_name = 'ihp_job_queue_trigger_install_test';"
        <> "SELECT pg_advisory_xact_lock(hashtext(current_schema()), hashtext('" <> functionName <> "'::name::text));"
        <> "SELECT pg_sleep(3);"
        <> "ROLLBACK;"

triggerInstallLockHeld :: HasqlPool.Pool -> IO Bool
triggerInstallLockHeld pool = do
    let sql = "SELECT EXISTS ("
            <> " SELECT 1 FROM pg_locks l"
            <> " JOIN pg_stat_activity a ON a.pid = l.pid"
            <> " WHERE l.locktype = 'advisory'"
            <> " AND l.granted"
            <> " AND a.application_name = 'ihp_job_queue_trigger_install_test')"
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    let statement = Hasql.unpreparable sql Encoders.noParams decoder
    runPool pool (HasqlSession.statement () statement)

ensureTestNotificationTriggers :: HasqlPool.Pool -> Text -> IO ()
ensureTestNotificationTriggers pool tableName = do
    let ?context = TestContext { logger = noopLogger }
    JobQueue.ensureNotificationTriggers pool tableName

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
