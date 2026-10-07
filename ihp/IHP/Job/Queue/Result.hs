{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Queue.Result
( jobDidFail
, jobDidTimeout
, jobDidSucceed
, jobDidInterrupt
, backoffDelay
, recoverStaleJobs
) where

import IHP.Prelude
import IHP.Job.Types
import IHP.Job.Queue.Pool (runPool)
import IHP.Job.Queue.StatusInstances ()
import IHP.Job.Queue.Worker (reapExpiredWorkers)
import IHP.ModelSupport (Table (..), InputValue (..))
import IHP.ModelSupport.Types (Id' (..), PrimaryKey)
import System.Log.FastLogger (FastLogger, toLogStr)
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import Data.Functor.Contravariant (contramap)

-- | Called when a job failed. Sets the job status to 'JobStatusFailed' or 'JobStatusRetry' (if more attempts are possible) and resets 'lockedBy'
jobDidFail :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    , HasField "attemptsCount" job Int
    , HasField "runAt" job UTCTime
    , Job job
    , ?context :: context
    , HasField "logger" context FastLogger
    ) => HasqlPool.Pool -> job -> SomeException -> IO ()
jobDidFail pool job exception = do
    now <- getCurrentTime

    ?context.logger (toLogStr ("Failed job with exception: " <> tshow exception))

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusFailed
    let nextRunAt = if canRetry
            then addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
            else job.runAt
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = $1::public.job_status, locked_by = NULL, locked_at = NULL, updated_at = $2, last_error = $3, run_at = $4 WHERE id = $5"
            <> " AND status = 'job_status_running' AND locked_by = $6 AND locked_at = $7"
    let encoder =
            contramap (\(s,_,_,_,_,_,_) -> s) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,u,_,_,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,e,_,_,_,_) -> e) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,_,_,r,_,_,_) -> r) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,i,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
            <> contramap (\(_,_,_,_,_,w,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
            <> contramap (\(_,_,_,_,_,_,l) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (inputValue status, now, tshow exception, nextRunAt, jobId, job.lockedBy, job.lockedAt) statement)

jobDidTimeout :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    , HasField "attemptsCount" job Int
    , HasField "runAt" job UTCTime
    , Job job
    , ?context :: context
    , HasField "logger" context FastLogger
    ) => HasqlPool.Pool -> job -> IO ()
jobDidTimeout pool job = do
    now <- getCurrentTime

    ?context.logger (toLogStr ("Job timed out" :: Text))

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusTimedOut
    let nextRunAt = if canRetry
            then addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
            else job.runAt
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = $1::public.job_status, locked_by = NULL, locked_at = NULL, updated_at = $2, last_error = $3, run_at = $4 WHERE id = $5"
            <> " AND status = 'job_status_running' AND locked_by = $6 AND locked_at = $7"
    let encoder =
            contramap (\(s,_,_,_,_,_,_) -> s) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,u,_,_,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,e,_,_,_,_) -> e) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,_,_,r,_,_,_) -> r) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,i,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
            <> contramap (\(_,_,_,_,_,w,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
            <> contramap (\(_,_,_,_,_,_,l) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (inputValue status, now, "Timeout reached" :: Text, nextRunAt, jobId, job.lockedBy, job.lockedAt) statement)


-- | Complete only the execution represented by the fetched job.
-- A late result must not overwrite a job released or claimed by another execution.
jobDidSucceed :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    , ?context :: context
    , HasField "logger" context FastLogger
    ) => HasqlPool.Pool -> job -> IO ()
jobDidSucceed pool job = do
    ?context.logger (toLogStr ("Succeeded job" :: Text))
    updatedAt <- getCurrentTime
    let Id jobId = job.id
    let tableNameText = tableName @job
    let sql = "UPDATE " <> tableNameText
            <> " SET status = 'job_status_succeeded', locked_by = NULL, locked_at = NULL, updated_at = $1 WHERE id = $2"
            <> " AND status = 'job_status_running' AND locked_by = $3 AND locked_at = $4"
    let encoder =
            contramap (\(u,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
            <> contramap (\(_,i,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
            <> contramap (\(_,_,w,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
            <> contramap (\(_,_,_,l) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (updatedAt, jobId, job.lockedBy, job.lockedAt) statement)

-- | Release a cancelled execution after its action has stopped. Deployment
-- interruptions neither consume an attempt nor apply failure backoff.
jobDidInterrupt :: forall job.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    ) => HasqlPool.Pool -> job -> IO ()
jobDidInterrupt pool job = do
    let Id jobId = job.id
    let sql = "UPDATE " <> tableName @job
            <> " SET status = 'job_status_retry', locked_by = NULL, locked_at = NULL"
            <> ", updated_at = NOW(), run_at = NOW(), attempts_count = GREATEST(0, attempts_count - 1)"
            <> " WHERE id = $1 AND status = 'job_status_running' AND locked_by = $2 AND locked_at = $3"
    let encoder =
            contramap (\(i,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
            <> contramap (\(_,w,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
            <> contramap (\(_,_,l) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
    let statement = Hasql.unpreparable sql encoder Decoders.noResult
    runPool pool (HasqlSession.statement (jobId, job.lockedBy, job.lockedAt) statement)

-- | Compute the delay before the next retry attempt.
--
-- For 'LinearBackoff', the delay is constant.
-- For 'ExponentialBackoff', the delay doubles each attempt, capped at 24 hours.
backoffDelay :: BackoffStrategy -> Int -> NominalDiffTime
backoffDelay (LinearBackoff { delayInSeconds }) _ = fromIntegral delayInSeconds
backoffDelay (ExponentialBackoff { delayInSeconds }) attempts =
    min 86400 (fromIntegral delayInSeconds * (2 ^ min attempts 20))

-- | Compatibility wrapper for worker lease recovery. Recovery now depends on
-- worker heartbeats, never the age of a running job. The threshold is ignored.
-- Expired workers release jobs in all registered queues through foreign keys.
recoverStaleJobs :: forall job.
    ( Table job
    ) => HasqlPool.Pool -> NominalDiffTime -> IO ()
recoverStaleJobs pool _ = reapExpiredWorkers pool
