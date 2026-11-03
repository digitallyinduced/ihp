module Test.JobQueueSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.Job.Queue as JobQueue
import IHP.Job.Queue.Pool (runPool)
import qualified IHP.Job.Queue.Worker as Worker
import qualified Data.UUID.V4 as UUID
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
    describe "IHP.Job.Queue.Worker" do
        it "releases interrupted jobs immediately without consuming a retry attempt" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                Worker.unregisterWorker pool workerId
                queryBool pool ("SELECT status = 'job_status_retry' AND locked_by IS NULL"
                    <> " AND locked_at IS NULL AND run_at <= clock_timestamp() AND attempts_count = 2"
                    <> " FROM worker_registry_spec_jobs") `shouldReturn` True

        it "does not recover a long-running job belonging to a live worker" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                Worker.reapExpiredWorkers pool
                Worker.heartbeatWorker pool workerId `shouldReturn` True
                queryBool pool "SELECT status = 'job_status_running' AND locked_by IS NOT NULL FROM worker_registry_spec_jobs"
                    `shouldReturn` True

        it "reaps expired workers and releases their jobs" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                expireTestWorker pool workerId
                Worker.reapExpiredWorkers pool
                queryBool pool "SELECT status = 'job_status_retry' AND locked_by IS NULL FROM worker_registry_spec_jobs"
                    `shouldReturn` True
                Worker.heartbeatWorker pool workerId `shouldReturn` False

        it "bounds recovery lock waits and retries safely after a blocked foreign-key action" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                expireTestWorker pool workerId
                let holdJobTable = runScript pool
                        ("BEGIN; LOCK TABLE worker_registry_spec_jobs IN SHARE MODE;"
                            <> "SELECT pg_sleep(3); ROLLBACK;")
                Async.withAsync holdJobTable \_ -> do
                    waitUntil 2_000_000 (queryBool pool
                        ("SELECT EXISTS (SELECT 1 FROM pg_locks WHERE relation = 'worker_registry_spec_jobs'::regclass"
                            <> " AND mode = 'ShareLock' AND granted)")) `shouldReturn` True
                    result <- timeout 2_000_000 (Exception.try (Worker.reapExpiredWorkers pool) :: IO (Either HasqlError ()))
                    case result of
                        Just (Left _) -> pure ()
                        _ -> expectationFailure "reaper did not abort its blocked FK action within the lock timeout"
                    queryBool pool "SELECT status = 'job_status_running' AND locked_by IS NOT NULL FROM worker_registry_spec_jobs"
                        `shouldReturn` True
                Worker.reapExpiredWorkers pool
                queryBool pool "SELECT status = 'job_status_retry' AND locked_by IS NULL FROM worker_registry_spec_jobs"
                    `shouldReturn` True

        it "does not resurrect an expired lease before it is reaped" do
            withRegisteredWorker \pool workerId -> do
                expireTestWorker pool workerId
                Worker.heartbeatWorker pool workerId `shouldReturn` False

        it "does not turn normal completion into a retry" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                runScript pool "UPDATE worker_registry_spec_jobs SET status = 'job_status_succeeded', locked_by = NULL"
                Worker.unregisterWorker pool workerId
                queryBool pool "SELECT status = 'job_status_succeeded' AND attempts_count = 3 FROM worker_registry_spec_jobs"
                    `shouldReturn` True

        it "rejects claims by deleted workers" do
            withRegisteredWorker \pool workerId -> do
                Worker.unregisterWorker pool workerId
                insertRunningWorkerJob pool workerId `shouldThrow` (\(_ :: HasqlError) -> True)

        it "refuses to silently release legacy worker locks during upgrade" do
            withRegisteredWorker \pool _ -> do
                runScript pool "ALTER TABLE worker_registry_spec_jobs DROP CONSTRAINT ihp_job_worker_fk"
                legacyWorkerId <- UUID.nextRandom
                insertRunningWorkerJob pool legacyWorkerId
                Worker.ensureJobWorkerForeignKey pool "worker_registry_spec_jobs"
                    `shouldThrow` (\(_ :: HasqlError) -> True)
                queryBool pool "SELECT status = 'job_status_running' AND locked_by IS NOT NULL FROM worker_registry_spec_jobs"
                    `shouldReturn` True

        it "does not take DDL locks when ownership infrastructure already exists" do
            withRegisteredWorker \pool _ -> do
                Async.withAsync (holdRowExclusiveLock pool "worker_registry_spec_jobs") \_ -> do
                    waitUntil 2_000_000 (rowExclusiveLockHeld pool "worker_registry_spec_jobs") `shouldReturn` True
                    timeout 1_000_000 (Worker.ensureJobWorkerForeignKey pool "worker_registry_spec_jobs")
                        `shouldReturn` Just ()

        it "installs an ownership index without duplicating it on restart" do
            withRegisteredWorker \pool _ -> do
                Worker.ensureJobWorkerForeignKey pool "worker_registry_spec_jobs"
                queryBool pool ("SELECT count(*) = 1 FROM pg_index i JOIN pg_attribute a"
                    <> " ON a.attrelid = i.indrelid AND a.attnum = i.indkey[0]"
                    <> " WHERE i.indrelid = 'worker_registry_spec_jobs'::regclass AND a.attname = 'locked_by'")
                    `shouldReturn` True

        it "repairs a missing release trigger" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                runScript pool "DROP TRIGGER ihp_release_worker_job ON worker_registry_spec_jobs"
                Worker.ensureJobWorkerForeignKey pool "worker_registry_spec_jobs"
                runScript pool ("DELETE FROM public.job_workers WHERE id = '" <> tshow workerId <> "'")
                queryBool pool "SELECT status = 'job_status_retry' FROM worker_registry_spec_jobs" `shouldReturn` True

        it "fails a released job instead of duplicating a pending job for the same work" do
            withRegisteredWorker \pool workerId -> do
                insertRunningWorkerJob pool workerId
                insertPendingDuplicateJob pool
                Worker.unregisterWorker pool workerId
                queryBool pool ("SELECT status = 'job_status_failed' AND locked_by IS NULL AND attempts_count = 3"
                    <> " AND last_error LIKE 'Not retried:%'"
                    <> " FROM worker_registry_spec_jobs WHERE id = '00000000-0000-0000-0000-000000000001'")
                    `shouldReturn` True
                queryBool pool ("SELECT status = 'job_status_not_started'"
                    <> " FROM worker_registry_spec_jobs WHERE id = '00000000-0000-0000-0000-000000000002'")
                    `shouldReturn` True
                queryBool pool ("SELECT NOT EXISTS (SELECT 1 FROM public.job_workers WHERE id = '" <> tshow workerId <> "')")
                    `shouldReturn` True

        it "reaps every expired worker even when one of their jobs conflicts with a pending job" do
            withRegisteredWorker \pool workerId -> do
                otherWorkerId <- UUID.nextRandom
                Worker.registerWorker pool otherWorkerId
                insertRunningWorkerJob pool workerId
                insertPendingDuplicateJob pool
                runScript pool $
                    "INSERT INTO worker_registry_spec_jobs (id, status, locked_by, locked_at, attempts_count, job_key)"
                    <> " VALUES ('00000000-0000-0000-0000-000000000003', 'job_status_running', '"
                    <> tshow otherWorkerId <> "', now(), 1, 2)"
                expireTestWorker pool workerId
                expireTestWorker pool otherWorkerId
                Worker.reapExpiredWorkers pool
                queryBool pool ("SELECT bool_and(CASE id"
                    <> " WHEN '00000000-0000-0000-0000-000000000001' THEN status = 'job_status_failed'"
                    <> " WHEN '00000000-0000-0000-0000-000000000002' THEN status = 'job_status_not_started'"
                    <> " ELSE status = 'job_status_retry' AND attempts_count = 0 END)"
                    <> " FROM worker_registry_spec_jobs")
                    `shouldReturn` True
                queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM public.job_workers WHERE heartbeat_at < now() - interval '120 seconds')"
                    `shouldReturn` True

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

        it "fails immediately without blocking job writes when the trigger table lock is unavailable" do
            withJobWatcher False \pool -> do
                dropUpdateNotificationTrigger pool testTableName
                writer <- Async.async (holdRowExclusiveLock pool testTableName)
                Exception.finally
                    (do
                        didAcquireLock <- waitUntil 2_000_000 (rowExclusiveLockHeld pool testTableName)
                        didAcquireLock `shouldBe` True

                        repairResult <- timeout 1_000_000 (Exception.try (ensureTestNotificationTriggers pool testTableName) :: IO (Either HasqlError ()))
                        case repairResult of
                            Just (Left _) -> pure ()
                            Just (Right _) -> expectationFailure "trigger repair unexpectedly succeeded while a writer held the table lock"
                            Nothing -> expectationFailure "trigger repair waited for the table lock"

                        writeResult <- timeout 1_000_000 (insertTestJob pool testTableName)
                        writeResult `shouldBe` Just ()
                        writerState <- Async.poll writer
                        case writerState of
                            Nothing -> pure ()
                            Just _ -> expectationFailure "writer lock was released before the job insert completed")
                    (Async.cancel writer)

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

withRegisteredWorker :: (HasqlPool.Pool -> UUID -> IO ()) -> IO ()
withRegisteredWorker action = withDB \modelContext _ _ -> do
    let pool = modelContext.hasqlPool
    workerId <- UUID.nextRandom
    Worker.ensureWorkerRegistry pool
    Exception.finally
        (do
            runScript pool ("DROP TABLE IF EXISTS worker_registry_spec_jobs;"
                <> "CREATE TABLE worker_registry_spec_jobs (id UUID PRIMARY KEY,"
                <> "status TEXT NOT NULL, locked_by UUID, locked_at TIMESTAMPTZ,"
                <> "run_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),"
                <> "attempts_count INT NOT NULL DEFAULT 0, last_error TEXT, job_key INT NOT NULL DEFAULT 1);"
                -- One pending job per key, as applications use to deduplicate queued work.
                <> "CREATE UNIQUE INDEX worker_registry_spec_jobs_pending_key ON worker_registry_spec_jobs (job_key)"
                <> " WHERE status IN ('job_status_not_started', 'job_status_retry')")
            Worker.ensureJobWorkerForeignKey pool "worker_registry_spec_jobs"
            Worker.registerWorker pool workerId
            action pool workerId)
        (do
            runScript pool "DROP TABLE IF EXISTS worker_registry_spec_jobs"
            Worker.unregisterWorker pool workerId)

insertRunningWorkerJob :: HasqlPool.Pool -> UUID -> IO ()
insertRunningWorkerJob pool workerId = runScript pool $
    "INSERT INTO worker_registry_spec_jobs (id, status, locked_by, locked_at, attempts_count)"
    <> " VALUES ('00000000-0000-0000-0000-000000000001', 'job_status_running', '"
    <> tshow workerId <> "', now() - interval '1 day', 3)"

insertPendingDuplicateJob :: HasqlPool.Pool -> IO ()
insertPendingDuplicateJob pool = runScript pool $
    "INSERT INTO worker_registry_spec_jobs (id, status) VALUES ('00000000-0000-0000-0000-000000000002', 'job_status_not_started')"

expireTestWorker :: HasqlPool.Pool -> UUID -> IO ()
expireTestWorker pool workerId = runScript pool $
    "UPDATE public.job_workers SET heartbeat_at = now() - interval '121 seconds' WHERE id = '"
    <> tshow workerId <> "'"

queryBool :: HasqlPool.Pool -> Text -> IO Bool
queryBool pool sql = runPool pool $ HasqlSession.statement () $
    Hasql.unpreparable sql Encoders.noParams (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

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

holdRowExclusiveLock :: HasqlPool.Pool -> Text -> IO ()
holdRowExclusiveLock pool tableName =
    runScript pool $
        "BEGIN;"
        <> "SET LOCAL application_name = 'ihp_job_queue_row_exclusive_test';"
        <> "LOCK TABLE \"" <> tableName <> "\" IN ROW EXCLUSIVE MODE;"
        <> "SELECT pg_sleep(3);"
        <> "ROLLBACK;"

rowExclusiveLockHeld :: HasqlPool.Pool -> Text -> IO Bool
rowExclusiveLockHeld pool tableName = do
    let sql = "SELECT EXISTS ("
            <> " SELECT 1 FROM pg_locks l"
            <> " JOIN pg_stat_activity a ON a.pid = l.pid"
            <> " WHERE l.relation = $1::regclass"
            <> " AND l.mode = 'RowExclusiveLock'"
            <> " AND l.granted"
            <> " AND a.application_name = 'ihp_job_queue_row_exclusive_test')"
    let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    let statement = Hasql.unpreparable sql encoder decoder
    runPool pool (HasqlSession.statement tableName statement)

insertTestJob :: HasqlPool.Pool -> Text -> IO ()
insertTestJob pool tableName =
    runScript pool ("INSERT INTO \"" <> tableName <> "\" (id) VALUES ('00000000-0000-0000-0000-000000000001');")

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
