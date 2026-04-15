{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Queue.Result
( jobDidFail
, jobDidTimeout
, jobDidSucceed
, backoffDelay
, recoverStaleJobs
, recoverStaleJobsForTable
) where

import IHP.Prelude
import IHP.Job.Types
import IHP.Job.Queue.Pool (runPool)
import IHP.Job.Queue.StatusInstances ()
import IHP.ModelSupport (Table (..), InputValue (..))
import IHP.ModelSupport.Types (Id' (..), PrimaryKey)
import qualified IHP.Log as Log
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Data.Functor.Contravariant (contramap)
import Data.UUID (UUID)

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
backoffDelay (LinearBackoff { delayInSeconds }) _ = fromIntegral delayInSeconds
backoffDelay (ExponentialBackoff { delayInSeconds }) attempts =
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
    _ <- recoverStaleJobsForTable pool (tableName @job) staleThreshold
    pure ()

-- | Like 'recoverStaleJobs' but takes the table name explicitly and reports
-- which rows were recovered, returning @(count, previousWorkerUuids)@.
--
-- The @previousWorkerUuids@ list contains the @locked_by@ values that were
-- attached to the recovered rows before they were reset, so callers can log
-- "we recovered N rows previously held by worker X". Workers with a NULL
-- @locked_by@ are filtered out of the list (but still counted).
--
-- Used both by the periodic recovery loop and by the boot-time sweep that
-- runs once at worker startup.
recoverStaleJobsForTable ::
    HasqlPool.Pool -> Text -> NominalDiffTime -> IO (Int, [UUID])
recoverStaleJobsForTable pool tableNameText staleThreshold = do
    -- Tier 1: Recently stale jobs (threshold..24h) -> retry.
    -- Capture the prior locked_by via a CTE so RETURNING can report it
    -- (RETURNING on the UPDATE alone would observe the post-update NULL).
    let retrySql =
            "WITH stale AS ("
            <> " SELECT id, locked_by AS prev_locked_by FROM \"" <> tableNameText <> "\""
            <> " WHERE status = 'job_status_running'"
            <> " AND locked_at < NOW() - interval '1 second' * $1"
            <> " AND locked_at > NOW() - interval '1 day'"
            <> " FOR UPDATE"
            <> " ) UPDATE \"" <> tableNameText <> "\" t"
            <> " SET status = 'job_status_retry', locked_by = NULL, locked_at = NULL, run_at = NOW()"
            <> " FROM stale WHERE t.id = stale.id"
            <> " RETURNING stale.prev_locked_by"
    let retryEncoder = Encoders.param (Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8))
    let uuidRowsDecoder = Decoders.rowList (Decoders.column (Decoders.nullable Decoders.uuid))
    let retryStatement = Hasql.unpreparable retrySql retryEncoder uuidRowsDecoder

    -- Tier 2: Ancient stale jobs (>24h) -> mark failed
    let failSql =
            "WITH stale AS ("
            <> " SELECT id, locked_by AS prev_locked_by FROM \"" <> tableNameText <> "\""
            <> " WHERE status = 'job_status_running'"
            <> " AND locked_at < NOW() - interval '1 day'"
            <> " FOR UPDATE"
            <> " ) UPDATE \"" <> tableNameText <> "\" t"
            <> " SET status = 'job_status_failed', locked_by = NULL, locked_at = NULL"
            <> ", last_error = 'Stale job: worker likely crashed'"
            <> " FROM stale WHERE t.id = stale.id"
            <> " RETURNING stale.prev_locked_by"
    let failStatement = Hasql.unpreparable failSql Encoders.noParams uuidRowsDecoder

    let thresholdSeconds = round staleThreshold :: Int
    retryRows <- runPool pool (HasqlSession.statement thresholdSeconds retryStatement)
    failRows <- runPool pool (HasqlSession.statement () failStatement)
    let allRows = retryRows ++ failRows
    pure (length allRows, catMaybes allRows)
