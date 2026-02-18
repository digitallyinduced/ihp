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
import IHP.ModelSupport (HasqlError(..), Table(..), isCachedPlanError, GetModelByTableName, GetTableName, InputValue(..))
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
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
import qualified Hasql.Statement as Hasql
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue, STM)
import Control.Concurrent.STM.TBQueue (isFullTBQueue)
import Data.Functor.Contravariant (contramap)

-- | Run a hasql session against the pool, retrying once on cached plan errors.
runPool :: HasqlPool.Pool -> HasqlSession.Session a -> IO a
runPool pool session = do
    result <- HasqlPool.use pool session
    case result of
        Left err
            | isCachedPlanError err -> do
                HasqlPool.release pool
                retryResult <- HasqlPool.use pool session
                case retryResult of
                    Left retryErr -> throwIO (HasqlError retryErr)
                    Right a -> pure a
            | otherwise -> throwIO (HasqlError err)
        Right a -> pure a

-- | Lock and fetch the next available job. In case no job is available returns Nothing.
--
-- The lock is set on the job row in an atomic way.
--
-- The job status is set to JobStatusRunning, lockedBy will be set to the worker id and the attemptsCount is incremented.
--
-- __Example:__ Locking a SendMailJob
--
-- > let workerId :: UUID = "faa5ba30-1d76-4adf-bf01-2d1f95cddc04"
-- > job <- fetchNextJob @SendMailJob pool workerId
--
-- After you're done with the job, call 'jobDidFail' or 'jobDidSucceed' to make it available to the queue again.
fetchNextJob :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , Table job
    ) => HasqlPool.Pool -> UUID -> IO (Maybe job)
fetchNextJob pool workerId = do
    let tableNameText = tableName @job
    let returningColumns = Text.intercalate ", " (columnNames @job)
    let sql = "UPDATE " <> tableNameText
            <> " SET status = 'job_status_running'"
            <> ", locked_at = NOW(), locked_by = $1"
            <> ", attempts_count = attempts_count + 1"
            <> " WHERE id IN (SELECT id FROM " <> tableNameText
            <> " WHERE " <> pendingJobConditionSQL
            <> " ORDER BY created_at LIMIT 1 FOR UPDATE SKIP LOCKED)"
            <> " RETURNING " <> returningColumns
    let encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
    let decoder = Decoders.rowMaybe (hasqlRowDecoder @job)
    let statement = Hasql.unpreparable sql encoder decoder
    runPool pool (HasqlSession.statement workerId statement)

-- | Shared WHERE condition for fetching pending jobs as a SQL text fragment.
-- Matches jobs that are either not started or in retry state,
-- not locked, and whose run_at time has passed.
-- Enum values are inlined as SQL string literals (PostgreSQL casts them to job_status).
pendingJobConditionSQL :: Text
pendingJobConditionSQL =
    "(status = 'job_status_not_started'"
    <> " OR status = 'job_status_retry'"
    <> ") AND locked_by IS NULL AND run_at <= NOW()"

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
watchForJob :: (?context :: context, HasField "logger" context Log.Logger) => HasqlPool.Pool -> PGListener.PGListener -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO (PGListener.Subscription, ReleaseKey)
watchForJob pool pgListener tableName pollInterval onNewJob = do
    let tableNameBS = cs tableName
    liftIO do
        runPool pool (HasqlSession.script (createNotificationTriggerSQL tableNameBS))

    poller <- pollForJob pool tableName pollInterval onNewJob
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
pollForJob :: (?context :: context, HasField "logger" context Log.Logger) => HasqlPool.Pool -> Text -> Int -> TBQueue JobWorkerProcessMessage -> ResourceT IO ReleaseKey
pollForJob pool tableName pollInterval onNewJob = do
    let sql = "SELECT COUNT(*) FROM " <> tableName
            <> " WHERE " <> pendingJobConditionSQL
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    let statement = Hasql.unpreparable sql Encoders.noParams decoder
    let handler = do
            forever do
                result <- Exception.tryAny do
                    count :: Int <- fromIntegral <$> runPool pool (HasqlSession.statement () statement)

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

-- | Returns a SQL script to create the notification trigger.
--
-- Wrapped in a DO $$ block with EXCEPTION handler because concurrent requests
-- can race to CREATE OR REPLACE the same function, causing PostgreSQL to throw
-- 'tuple concurrently updated' (SQLSTATE XX000). This is safe to ignore: the
-- other connection's CREATE OR REPLACE will have succeeded.
createNotificationTriggerSQL :: ByteString -> Text
createNotificationTriggerSQL tableName =
        cs $
        "DO $$\n"
        <> "BEGIN\n"
        <> "    CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> channelName tableName <> "', '');\n"
            <> "    RETURN new;"
            <> "END;\n"
            <> "$BODY$ language plpgsql;\n"
        <> "    DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null; -- 'tuple concurrently updated': another connection installed it first\n"
        <> "END; $$"
    where
        functionName = "notify_job_queued_" <> tableName
        insertTriggerName = "did_insert_job_" <> tableName
        updateTriggerName = "did_update_job_" <> tableName

-- | Retuns the event name of the event that the pg notify trigger dispatches
channelName :: ByteString -> ByteString
channelName tableName = "job_available_" <> tableName

-- | Called when a job failed. Sets the job status to 'JobStatusFailed' or 'JobStatusRetry' (if more attempts are possible) and resets 'lockedBy'
jobDidFail :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "attemptsCount" job Int
    , HasField "runAt" job UTCTime
    , Job job
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => HasqlPool.Pool -> job -> SomeException -> IO ()
jobDidFail pool job exception = do
    now <- getCurrentTime

    Log.warn ("Failed job with exception: " <> tshow exception)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusFailed
    let nextRunAt = if canRetry
            then addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
            else job.runAt
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = $1::public.job_status, locked_by = NULL, updated_at = $2, last_error = $3, run_at = $4 WHERE id = $5"
    let encoder =
            contramap (\(s,_,_,_,_) -> s) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,u,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,e,_,_) -> e) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,_,_,r,_) -> r) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,i) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (inputValue status, now, tshow exception, nextRunAt, jobId) statement)

jobDidTimeout :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "attemptsCount" job Int
    , HasField "runAt" job UTCTime
    , Job job
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => HasqlPool.Pool -> job -> IO ()
jobDidTimeout pool job = do
    now <- getCurrentTime

    Log.warn ("Job timed out" :: Text)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusTimedOut
    let nextRunAt = if canRetry
            then addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
            else job.runAt
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = $1::public.job_status, locked_by = NULL, updated_at = $2, last_error = $3, run_at = $4 WHERE id = $5"
    let encoder =
            contramap (\(s,_,_,_,_) -> s) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,u,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,e,_,_) -> e) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,_,_,r,_) -> r) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,i) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (inputValue status, now, "Timeout reached" :: Text, nextRunAt, jobId) statement)


-- | Called when a job succeeded. Sets the job status to 'JobStatusSucceded' and resets 'lockedBy'
jobDidSucceed :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => HasqlPool.Pool -> job -> IO ()
jobDidSucceed pool job = do
    Log.info ("Succeeded job" :: Text)
    updatedAt <- getCurrentTime
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = 'job_status_succeeded', locked_by = NULL, updated_at = $1 WHERE id = $2"
    let encoder =
            contramap fst (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.uuid))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (updatedAt, jobId) statement)

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
    ( Table job
    ) => HasqlPool.Pool -> NominalDiffTime -> IO ()
recoverStaleJobs pool staleThreshold = do
    let tableNameText = tableName @job
    -- Tier 1: Recently stale jobs (threshold..24h) -> retry
    let retrySql =
            "UPDATE " <> tableNameText
            <> " SET status = 'job_status_retry', locked_by = NULL, locked_at = NULL, run_at = NOW()"
            <> " WHERE status = 'job_status_running'"
            <> " AND locked_at < NOW() - interval '1 second' * $1"
            <> " AND locked_at > NOW() - interval '1 day'"
    let retryEncoder = Encoders.param (Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8))
    let retryStatement = Hasql.unpreparable retrySql retryEncoder Decoders.noResult

    -- Tier 2: Ancient stale jobs (>24h) -> mark failed
    let failSql =
            "UPDATE " <> tableNameText
            <> " SET status = 'job_status_failed', locked_by = NULL, locked_at = NULL"
            <> ", last_error = 'Stale job: worker likely crashed'"
            <> " WHERE status = 'job_status_running'"
            <> " AND locked_at < NOW() - interval '1 day'"
    let failStatement = Hasql.unpreparable failSql Encoders.noParams Decoders.noResult

    let thresholdSeconds = round staleThreshold :: Int
    runPool pool (HasqlSession.statement thresholdSeconds retryStatement)
    runPool pool (HasqlSession.statement () failStatement)

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
--
-- These instances are needed by the generated @FromRow@ instances in user apps
-- (see 'compileFromRowInstance' in "IHP.SchemaCompiler").
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

-- | See 'FromField' instance above.
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

-- | DefaultParamEncoder for lists of JobStatus, needed for filterWhereIn/filterWhereNotIn
instance DefaultParamEncoder [JobStatus] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (Encoders.enum (Just "public") "job_status" inputValue)

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
