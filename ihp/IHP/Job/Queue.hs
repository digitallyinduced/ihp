{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.Job.Queue
Description: Functions to operate on the Job Queue Database
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Job.Queue where

import IHP.Prelude
import IHP.Job.Types
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exception
import IHP.ModelSupport
import IHP.Controller.Param
import qualified System.Random as Random
import qualified IHP.PGListener as PGListener
import qualified IHP.Log as Log
import Control.Monad.Trans.Resource
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import IHP.Hasql.Encoders ()
import qualified Data.Text as Text
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue, STM)
import Control.Concurrent.STM.TBQueue (isFullTBQueue)

-- | Lock and fetch the next available job. In case no job is available returns Nothing.
--
-- The lock is set on the job row in an atomic way.
--
-- The job status is set to JobStatusRunning, lockedBy will be set to the worker id and the attemptsCount is incremented.
--
-- __Example:__ Locking a SendMailJob
--
-- > let workerId :: UUID = "faa5ba30-1d76-4adf-bf01-2d1f95cddc04"
-- > job <- fetchNextJob @SendMailJob workerId
--
-- After you're done with the job, call 'jobDidFail' or 'jobDidSucceed' to make it available to the queue again.
fetchNextJob :: forall job.
    ( ?modelContext :: ModelContext
    , job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , Table job
    ) => UUID -> IO (Maybe job)
fetchNextJob workerId = do
    let tableNameText = tableName @job
    let returningColumns = Text.intercalate ", " (columnNames @job)
    let snippet =
            Snippet.sql "UPDATE " <> Snippet.sql tableNameText
            <> Snippet.sql " SET status = " <> Snippet.param JobStatusRunning
            <> Snippet.sql ", locked_at = NOW(), locked_by = " <> Snippet.param workerId
            <> Snippet.sql ", attempts_count = attempts_count + 1"
            <> Snippet.sql " WHERE id IN (SELECT id FROM " <> Snippet.sql tableNameText
            <> Snippet.sql " WHERE " <> pendingJobCondition
            <> Snippet.sql " ORDER BY created_at LIMIT 1 FOR UPDATE SKIP LOCKED)"
            <> Snippet.sql " RETURNING " <> Snippet.sql returningColumns
    let decoder = Decoders.rowMaybe (hasqlRowDecoder @job)

    pool <- getHasqlPool
    withoutQueryLogging (sqlQueryHasql pool snippet decoder)

-- | Shared WHERE condition for fetching pending jobs.
-- Matches jobs that are either not started or in retry state,
-- not locked, and whose run_at time has passed.
pendingJobCondition :: Snippet.Snippet
pendingJobCondition =
    Snippet.sql "(status = " <> Snippet.param JobStatusNotStarted
    <> Snippet.sql " OR status = " <> Snippet.param JobStatusRetry
    <> Snippet.sql ") AND locked_by IS NULL AND run_at <= NOW()"

-- | Calls a callback every time something is inserted, updated or deleted in a given database table.
--
-- In the background this function creates a database trigger to notify this function about table changes
-- using pg_notify. When there are existing triggers, it will silently recreate them. So this will most likely
-- not fail.
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
--
watchForJob :: (?modelContext :: ModelContext) => PGListener.PGListener -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO (PGListener.Subscription, ReleaseKey)
watchForJob pgListener tableName pollInterval onNewJob = do
    let tableNameBS = cs tableName
    liftIO do
        pool <- getHasqlPool
        withoutQueryLogging (runSessionHasql pool (mapM_ HasqlSession.script (createNotificationTriggerStatements tableNameBS)))

    poller <- pollForJob tableName pollInterval onNewJob
    subscription <- liftIO $ pgListener |> PGListener.subscribe (channelName tableNameBS) (const (do _ <- atomically $ tryWriteTBQueue onNewJob JobAvailable; pure ()))

    pure (subscription, poller)

-- | Periodically checks the queue table for open jobs. Calls the callback if there are any.
--
-- 'watchForJob' only catches jobs when something is changed on the table. When a job is scheduled
-- with a 'runAt' in the future, and no other operation is happening on the queue, the database triggers
-- will not run, and so 'watchForJob' cannot pick up the job even when 'runAt' is now in the past.
--
-- This function returns a Async. Call 'cancel' on the async to stop polling the database.
--
pollForJob :: (?modelContext :: ModelContext) => Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO ReleaseKey
pollForJob tableName pollInterval onNewJob = do
    let snippet =
            Snippet.sql "SELECT COUNT(*) FROM " <> Snippet.sql tableName
            <> Snippet.sql " WHERE " <> pendingJobCondition
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    let handler = do
            let ?context = ?modelContext
            pool <- getHasqlPool
            forever do
                result <- Exception.tryAny do
                    -- We don't log the queries to the console as it's filling up the log entries with noise
                    count :: Int <- fromIntegral <$> withoutQueryLogging (sqlQueryHasql pool snippet decoder)

                    -- For every job we send one signal to the job workers
                    -- This way we use full concurrency when we find multiple jobs
                    -- that haven't been picked up by the PGListener
                    forEach [1..count] \_ -> do
                        _ <- atomically $ tryWriteTBQueue onNewJob JobAvailable
                        pure ()
                case result of
                    Left exception -> Log.error ("Job poller: " <> tshow exception)
                    Right _ -> pure ()

                -- Add up to 2 seconds of jitter to avoid all job queues polling at the same time
                jitter <- Random.randomRIO (0, 2000000)
                let pollIntervalWithJitter = pollInterval + jitter

                Concurrent.threadDelay pollIntervalWithJitter

    fst <$> allocate (Async.async handler) Async.cancel

-- | Returns individual SQL statements to create the notification trigger.
-- Split into separate statements because hasql 1.10's 'script' expects
-- exactly one result per call (multi-statement scripts cause
-- "Got too many results in script" errors).
createNotificationTriggerStatements :: ByteString -> [Text]
createNotificationTriggerStatements tableName =
        [ "BEGIN"
        , cs $ "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> channelName tableName <> "', '');\n"
            <> "    RETURN new;"
            <> "END;\n"
            <> "$$ language plpgsql"
        , cs $ "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName
        , cs $ "CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "()"
        , cs $ "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName
        , cs $ "CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "()"
        , "COMMIT"
        ]
    where
        functionName = "notify_job_queued_" <> tableName
        insertTriggerName = "did_insert_job_" <> tableName
        updateTriggerName = "did_update_job_" <> tableName

-- | Retuns the event name of the event that the pg notify trigger dispatches
channelName :: ByteString -> ByteString
channelName tableName = "job_available_" <> tableName

-- | Called when a job failed. Sets the job status to 'JobStatusFailed' or 'JobStatusRetry' (if more attempts are possible) and resets 'lockedBy'
jobDidFail :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , SetField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> SomeException -> IO ()
jobDidFail job exception = do
    now <- getCurrentTime

    Log.warn ("Failed job with exception: " <> tshow exception)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusFailed
    let nextRunAt = addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt now
        |> set #lastError (Just (tshow exception))
        |> (if canRetry then set #runAt nextRunAt else id)
        |> updateRecord

    pure ()

jobDidTimeout :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , SetField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> IO ()
jobDidTimeout job = do
    now <- getCurrentTime

    Log.warn ("Job timed out" :: Text)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusTimedOut
    let nextRunAt = addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt now
        |> setJust #lastError "Timeout reached"
        |> (if canRetry then set #runAt nextRunAt else id)
        |> updateRecord

    pure ()


-- | Called when a job succeeded. Sets the job status to 'JobStatusSucceded' and resets 'lockedBy'
jobDidSucceed :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> IO ()
jobDidSucceed job = do
    Log.info ("Succeeded job" :: Text)
    updatedAt <- getCurrentTime
    job
        |> set #status JobStatusSucceeded
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> updateRecord

    pure ()

-- | Compute the delay before the next retry attempt.
--
-- For 'LinearBackoff', the delay is constant.
-- For 'ExponentialBackoff', the delay doubles each attempt, capped at 24 hours.
backoffDelay :: BackoffStrategy -> Int -> NominalDiffTime
backoffDelay (LinearBackoff {delayInSeconds}) _ = fromIntegral delayInSeconds
backoffDelay (ExponentialBackoff {delayInSeconds}) attempts =
    min 86400 (fromIntegral delayInSeconds * (2 ^ min attempts 20))

-- | Recover stale jobs that have been in 'JobStatusRunning' for too long,
-- likely due to a worker crash.
--
-- Two-tier recovery:
-- - Recently stale jobs (within 24h) are set back to retry
-- - Ancient stale jobs (older than 24h) are marked as failed
recoverStaleJobs :: forall job.
    ( ?modelContext :: ModelContext
    , Table job
    ) => NominalDiffTime -> IO ()
recoverStaleJobs staleThreshold = do
    let tableNameText = tableName @job
    -- Tier 1: Recently stale jobs (threshold..24h) → retry
    let retrySnippet =
            Snippet.sql "UPDATE " <> Snippet.sql tableNameText
            <> Snippet.sql " SET status = " <> Snippet.param JobStatusRetry
            <> Snippet.sql ", locked_by = NULL, locked_at = NULL, run_at = NOW()"
            <> Snippet.sql " WHERE status = " <> Snippet.param JobStatusRunning
            <> Snippet.sql " AND locked_at < NOW() - interval '1 second' * " <> Snippet.param (round staleThreshold :: Int)
            <> Snippet.sql " AND locked_at > NOW() - interval '1 day'"

    -- Tier 2: Ancient stale jobs (>24h) → mark failed
    let failSnippet =
            Snippet.sql "UPDATE " <> Snippet.sql tableNameText
            <> Snippet.sql " SET status = " <> Snippet.param JobStatusFailed
            <> Snippet.sql ", locked_by = NULL, locked_at = NULL"
            <> Snippet.sql ", last_error = 'Stale job: worker likely crashed'"
            <> Snippet.sql " WHERE status = " <> Snippet.param JobStatusRunning
            <> Snippet.sql " AND locked_at < NOW() - interval '1 day'"

    pool <- getHasqlPool
    withoutQueryLogging (sqlExecHasql pool retrySnippet)
    withoutQueryLogging (sqlExecHasql pool failSnippet)

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
instance PG.FromField JobStatus where
    fromField field (Just "job_status_not_started") = pure JobStatusNotStarted
    fromField field (Just "job_status_running") = pure JobStatusRunning
    fromField field (Just "job_status_failed") = pure JobStatusFailed
    fromField field (Just "job_status_timed_out") = pure JobStatusTimedOut
    fromField field (Just "job_status_succeeded") = pure JobStatusSucceeded
    fromField field (Just "job_status_retry") = pure JobStatusRetry
    fromField field (Just value) = PG.returnError PG.ConversionFailed field ("Unexpected value for enum value. Got: " <> cs value)
    fromField field Nothing = PG.returnError PG.UnexpectedNull field "Unexpected null for enum value"

-- The default state is @not started@
instance Default JobStatus where
    def = JobStatusNotStarted

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
instance PG.ToField JobStatus where
    toField JobStatusNotStarted = PG.toField ("job_status_not_started" :: Text)
    toField JobStatusRunning = PG.toField ("job_status_running" :: Text)
    toField JobStatusFailed = PG.toField ("job_status_failed" :: Text)
    toField JobStatusTimedOut = PG.toField ("job_status_timed_out" :: Text)
    toField JobStatusSucceeded = PG.toField ("job_status_succeeded" :: Text)
    toField JobStatusRetry = PG.toField ("job_status_retry" :: Text)

instance InputValue JobStatus where
    inputValue JobStatusNotStarted = "job_status_not_started" :: Text
    inputValue JobStatusRunning = "job_status_running" :: Text
    inputValue JobStatusFailed = "job_status_failed" :: Text
    inputValue JobStatusTimedOut = "job_status_timed_out" :: Text
    inputValue JobStatusSucceeded = "job_status_succeeded" :: Text
    inputValue JobStatusRetry = "job_status_retry" :: Text

instance IHP.Controller.Param.ParamReader JobStatus where
    readParameter = IHP.Controller.Param.enumParamReader

-- | Parses a Text value to a JobStatus. Used by hasql decoders.
-- Uses HashMap for O(1) lookup.
textToEnumJobStatusMap :: HashMap.HashMap Text JobStatus
textToEnumJobStatusMap = HashMap.fromList
    [ ("job_status_not_started", JobStatusNotStarted)
    , ("job_status_running", JobStatusRunning)
    , ("job_status_failed", JobStatusFailed)
    , ("job_status_timed_out", JobStatusTimedOut)
    , ("job_status_succeeded", JobStatusSucceeded)
    , ("job_status_retry", JobStatusRetry)
    ]

textToEnumJobStatus :: Text -> Maybe JobStatus
textToEnumJobStatus t = HashMap.lookup t textToEnumJobStatusMap

-- | DefaultParamEncoder for hasql queries using JobStatus in filterWhere
instance DefaultParamEncoder JobStatus where
    defaultParam = Encoders.nonNullable (Encoders.enum (Just "public") "job_status" inputValue)

getHasqlPool :: (?modelContext :: ModelContext) => IO HasqlPool.Pool
getHasqlPool = pure ?modelContext.hasqlPool

-- | Non-blocking write to a TBQueue. Returns True if the value was written,
-- False if the queue was full.
tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue queue value = do
    full <- isFullTBQueue queue
    if full
        then pure False
        else do
            writeTBQueue queue value
            pure True
