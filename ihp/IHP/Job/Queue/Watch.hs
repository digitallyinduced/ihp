module IHP.Job.Queue.Watch
( watchForJob
, watchForJobWithPollerTriggerRepair
, pollForJob
, notificationTriggersHealthy
, ensureNotificationTriggers
, createNotificationTriggerSQL
, channelName
) where

import IHP.Prelude
import IHP.Job.Queue.Pool (runPool)
import IHP.Job.Queue.Fetch (pendingJobConditionSQL)
import IHP.Job.Queue.STM (tryWriteTBQueue)
import IHP.Job.Types (JobWorkerProcessMessage (..))
import qualified IHP.PGListener as PGListener
import System.Log.FastLogger (FastLogger, toLogStr)
import Control.Monad.Trans.Resource
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified System.Random as Random
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Connection as HasqlConnection
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Control.Concurrent.STM (TBQueue, TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.Functor.Contravariant (contramap)

-- | Calls a callback every time something is inserted, updated or deleted in a given database table.
--
-- In the background this function creates and validates database triggers that notify this function about
-- table changes using @pg_notify@. If installing the triggers fails temporarily, jobs are still found by the
-- poller and trigger installation is retried.
--
-- This function returns a Async. Call 'cancel' on the async to stop watching the database.
--
-- __Example:__
--
-- > watchInsertOrUpdateTable "projects" do
-- >     putStrLn "Something changed in the projects table"
--
-- Now insert something into the @projects@ table. E.g. by running @make psql@ and then running @INSERT INTO projects (id, name) VALUES (DEFAULT, 'New project');@
-- You will see that @"Something changed in the projects table"@ is printed onto the screen.
watchForJob :: (?context :: context, HasField "logger" context FastLogger) => HasqlPool.Pool -> PGListener.PGListener -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO (PGListener.Subscription, ReleaseKey)
watchForJob pool pgListener tableName pollInterval onNewJob =
    watchForJobWithPollerTriggerRepair False pool pgListener tableName pollInterval onNewJob

-- | Like 'watchForJob' but allows enabling a poller-side trigger integrity check.
-- Useful in development to recover from missing triggers after `make db`.
watchForJobWithPollerTriggerRepair :: (?context :: context, HasField "logger" context FastLogger) => Bool -> HasqlPool.Pool -> PGListener.PGListener -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO (PGListener.Subscription, ReleaseKey)
watchForJobWithPollerTriggerRepair enablePollerTriggerRepair pool pgListener tableName pollInterval onNewJob = do
    let tableNameBS = cs tableName
    triggerRepairNeeded <- liftIO (newTVarIO False)
    liftIO do
        result <- Exception.tryAny (runPool pool (HasqlSession.script (createNotificationTriggerSQL tableNameBS)))
        case result of
            Left err -> do
                atomically (writeTVar triggerRepairNeeded True)
                ?context.logger (toLogStr ("Failed to install notification triggers for " <> tableName <> ": " <> tshow err <> ". Falling back to poller and retrying later."))
            Right _ -> atomically (writeTVar triggerRepairNeeded False)

        -- Recreate notification triggers when PGListener reconnects (e.g. after `make db` drops the database)
        PGListener.onReconnect (\connection -> do
            result <- HasqlConnection.use connection (HasqlSession.script (createNotificationTriggerSQL tableNameBS))
            case result of
                Left err -> do
                    atomically (writeTVar triggerRepairNeeded True)
                    ?context.logger (toLogStr ("Failed to recreate notification triggers for " <> tableName <> ": " <> tshow err <> ". Falling back to poller and retrying later."))
                Right _ -> do
                    atomically (writeTVar triggerRepairNeeded False)
                    ?context.logger (toLogStr ("Recreated notification triggers for " <> tableName))
            ) pgListener

    poller <- pollForJobWithTriggerRepairState enablePollerTriggerRepair triggerRepairNeeded pool tableName pollInterval onNewJob
    subscription <- liftIO $ pgListener |> PGListener.subscribe (channelName tableNameBS) (const (do
            didWrite <- atomically $ tryWriteTBQueue onNewJob JobAvailable
            unless didWrite (?context.logger (toLogStr ("Job queue full for " <> tableName)))
            ))

    pure (subscription, poller)

-- | Periodically checks the queue table for open jobs. Calls the callback if there are any.
--
-- 'watchForJob' only catches jobs when something is changed on the table. When a job is scheduled
-- with a 'runAt' in the future, and no other operation is happening on the queue, the database triggers
-- will not run, and so 'watchForJob' cannot pick up the job even when 'runAt' is now in the past.
--
-- This function returns a Async. Call 'cancel' on the async to stop polling the database.
pollForJob :: (?context :: context, HasField "logger" context FastLogger) => Bool -> HasqlPool.Pool -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO ReleaseKey
pollForJob enablePollerTriggerRepair pool tableName pollInterval onNewJob = do
    triggerRepairNeeded <- liftIO (newTVarIO False)
    pollForJobWithTriggerRepairState enablePollerTriggerRepair triggerRepairNeeded pool tableName pollInterval onNewJob

pollForJobWithTriggerRepairState :: (?context :: context, HasField "logger" context FastLogger) => Bool -> TVar Bool -> HasqlPool.Pool -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO ReleaseKey
pollForJobWithTriggerRepairState enablePollerTriggerRepair triggerRepairNeeded pool tableName pollInterval onNewJob = do
    let sql = "SELECT COUNT(*) FROM " <> tableName
            <> " WHERE " <> pendingJobConditionSQL
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    let statement = Hasql.unpreparable sql Encoders.noParams decoder
    let handler isFirstPoll = do
            pollResult <- Exception.tryAny do
                count :: Int <- fromIntegral <$> runPool pool (HasqlSession.statement () statement)

                -- For every job we send one signal to the job workers
                -- This way we use full concurrency when we find multiple jobs
                -- that haven't been picked up by the PGListener
                forEach [1..count] \_ -> do
                    _ <- atomically $ tryWriteTBQueue onNewJob JobAvailable
                    pure ()
            case pollResult of
                Left exception -> ?context.logger (toLogStr ("Job poller: " <> tshow exception))
                Right _ -> pure ()

            repairNeeded <- readTVarIO triggerRepairNeeded
            -- After a startup failure, first poll once before retrying so the
            -- fallback remains useful and we don't immediately repeat a timed-out DDL request.
            when (enablePollerTriggerRepair || (repairNeeded && not isFirstPoll)) do
                repairResult <- Exception.tryAny (ensureNotificationTriggers pool tableName)
                case repairResult of
                    Left exception -> ?context.logger (toLogStr ("Job poller: Failed to repair notification triggers for " <> tableName <> ": " <> tshow exception))
                    Right _ -> atomically (writeTVar triggerRepairNeeded False)

            -- Add up to 2 seconds of jitter to avoid all job queues polling at the same time
            jitter <- Random.randomRIO (0, 2000000)
            let pollIntervalWithJitter = pollInterval + jitter

            Concurrent.threadDelay pollIntervalWithJitter
            handler False

    fst <$> allocate (Async.async (handler True)) Async.cancel

notificationTriggersHealthy :: HasqlPool.Pool -> Text -> IO Bool
notificationTriggersHealthy pool tableName = do
    let insertTriggerName = "did_insert_job_" <> tableName
    let updateTriggerName = "did_update_job_" <> tableName
    let functionName = "notify_job_queued_" <> tableName
    let sql = "SELECT COUNT(*) FROM pg_trigger t"
            <> " JOIN pg_class c ON t.tgrelid = c.oid"
            <> " JOIN pg_namespace n ON c.relnamespace = n.oid"
            <> " WHERE n.nspname = current_schema()"
            <> " AND c.relname = $1::name"
            <> " AND NOT t.tgisinternal"
            <> " AND t.tgenabled = 'O'"
            <> " AND t.tgfoid = to_regproc($4)"
            <> " AND t.tgqual IS NOT NULL"
            <> " AND position('job_status_not_started' IN pg_get_triggerdef(t.oid)) > 0"
            <> " AND position('job_status_retry' IN pg_get_triggerdef(t.oid)) > 0"
            <> " AND ((t.tgname = $2::name AND t.tgtype = 5)"
            <> "   OR (t.tgname = $3::name AND t.tgtype = 17))"
    let encoder =
            contramap (\(tableNameParam, _, _, _) -> tableNameParam) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_, insertTriggerNameParam, _, _) -> insertTriggerNameParam) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_, _, updateTriggerNameParam, _) -> updateTriggerNameParam) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_, _, _, functionNameParam) -> functionNameParam) (Encoders.param (Encoders.nonNullable Encoders.text))
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    let statement = Hasql.unpreparable sql encoder decoder
    count :: Int <- fromIntegral <$> runPool pool (HasqlSession.statement (tableName, insertTriggerName, updateTriggerName, functionName) statement)
    pure (count == 2)

ensureNotificationTriggers :: (?context :: context, HasField "logger" context FastLogger) => HasqlPool.Pool -> Text -> IO ()
ensureNotificationTriggers pool tableName = do
    healthy <- notificationTriggersHealthy pool tableName
    unless healthy do
        let insertTriggerName = "did_insert_job_" <> tableName
        let updateTriggerName = "did_update_job_" <> tableName
        ?context.logger (toLogStr ("Job poller: Missing or invalid notification triggers for " <> tableName <> " (" <> insertTriggerName <> ", " <> updateTriggerName <> "). Repairing."))
        runPool pool (HasqlSession.script (createNotificationTriggerSQL (cs tableName)))
        ?context.logger (toLogStr ("Job poller: Repaired notification triggers for " <> tableName))

-- | Returns a SQL script to create the notification trigger.
--
-- The function body is always updated via @CREATE OR REPLACE FUNCTION@, which only
-- locks the function's row in @pg_proc@, not the job table.
--
-- The trigger DDL is only executed when a trigger is missing or invalid:
-- @DROP TRIGGER@ takes an @AccessExclusiveLock@ on the job table, which conflicts with
-- every other lock — even the @AccessShareLock@s held by a running @pg_dump@. Running
-- DROP + CREATE TRIGGER unconditionally on every start meant a job worker (re)started
-- while a backup was in flight would block on the trigger DDL, and all subsequent
-- INSERTs/UPDATEs on the job table would queue up behind that pending lock request,
-- stalling job processing until the backup finished.
--
-- When a trigger is missing or invalid (first install, after @make db@, or after manual
-- changes), a short @lock_timeout@ makes the DDL fail fast instead of blocking; the
-- caller falls back to the poller and retries later. Healthy triggers are never dropped.
--
-- The non-blocking advisory lock ensures only one process attempts installation at a
-- time. Other processes immediately fall back to polling instead of queuing behind it.
createNotificationTriggerSQL :: ByteString -> Text
createNotificationTriggerSQL tableName =
        cs $
        "DO $$\n"
        <> "BEGIN\n"
        <> "    SET LOCAL lock_timeout = '5s';\n"
        <> "    IF NOT pg_try_advisory_xact_lock(hashtext(current_schema()), hashtext('" <> functionName <> "'::name::text)) THEN\n"
        <> "        RAISE EXCEPTION 'Notification trigger installation already in progress' USING ERRCODE = '55P03';\n"
        <> "    END IF;\n"
        <> "    CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> channelName tableName <> "', '');\n"
            <> "    RETURN new;"
            <> "\nEND;\n"
            <> "$BODY$ language plpgsql;\n"
        -- PostgreSQL encodes AFTER ROW INSERT as 5 and AFTER ROW UPDATE as 17 in pg_trigger.tgtype.
        <> repairTriggerSQL insertTriggerName "5" insertTriggerDefinition
        <> repairTriggerSQL updateTriggerName "17" updateTriggerDefinition
        <> "END; $$"
    where
        functionName = "notify_job_queued_" <> tableName
        insertTriggerName = "did_insert_job_" <> tableName
        updateTriggerName = "did_update_job_" <> tableName
        insertTriggerDefinition = "AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "()"
        updateTriggerDefinition = "AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "()"
        triggerExistsSQL triggerName = "EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> triggerName <> "'::name AND tgrelid = '" <> tableName <> "'::regclass)"
        triggerHealthySQL triggerName triggerType =
            "EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = '" <> triggerName <> "'::name"
            <> " AND tgrelid = '" <> tableName <> "'::regclass"
            <> " AND tgenabled = 'O'"
            <> " AND tgfoid = to_regproc('" <> functionName <> "')"
            <> " AND tgqual IS NOT NULL"
            <> " AND position('job_status_not_started' IN pg_get_triggerdef(oid)) > 0"
            <> " AND position('job_status_retry' IN pg_get_triggerdef(oid)) > 0"
            <> " AND tgtype = " <> triggerType <> ")"
        repairTriggerSQL triggerName triggerType triggerDefinition =
            "    IF " <> triggerExistsSQL triggerName <> " AND NOT " <> triggerHealthySQL triggerName triggerType <> " THEN\n"
            <> "        DROP TRIGGER " <> triggerName <> " ON \"" <> tableName <> "\";\n"
            <> "    END IF;\n"
            <> "    IF NOT " <> triggerHealthySQL triggerName triggerType <> " THEN\n"
            <> "        CREATE TRIGGER " <> triggerName <> " " <> triggerDefinition <> ";\n"
            <> "    END IF;\n"

-- | Retuns the event name of the event that the pg notify trigger dispatches
channelName :: ByteString -> ByteString
channelName tableName = "job_available_" <> tableName
