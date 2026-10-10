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
    ?context.logger (toLogStr ("Failed job with exception: " <> tshow exception))
    finishFailedAttempt pool job JobStatusFailed (tshow exception)

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
    ?context.logger (toLogStr ("Job timed out" :: Text))
    finishFailedAttempt pool job JobStatusTimedOut "Timeout reached"

-- | Retry a failed attempt after its backoff, or record the final status once
-- all attempts are used up.
finishFailedAttempt :: forall job context.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    , HasField "attemptsCount" job Int
    , Job job
    , ?context :: context
    , HasField "logger" context FastLogger
    ) => HasqlPool.Pool -> job -> JobStatus -> Text -> IO ()
finishFailedAttempt pool job finalStatus lastError = do
    now <- getCurrentTime
    let ?job = job
    if job.attemptsCount < maxAttempts
        then do
            let nextRunAt = addUTCTime (backoffDelay (backoffStrategy @job) job.attemptsCount) now
            requeued <- requeueJob pool job (Just nextRunAt) (Just lastError) False
            unless requeued $
                ?context.logger (toLogStr ("Job not retried: a pending job for the same work already exists" :: Text))
        else do
            let Id jobId = job.id
            let sql = "UPDATE " <> tableName @job
                    <> " SET status = $1::public.job_status, locked_by = NULL, locked_at = NULL, updated_at = $2, last_error = $3 WHERE id = $4"
                    <> " AND status = 'job_status_running' AND locked_by = $5 AND locked_at = $6"
            let encoder =
                    contramap (\(s,_,_,_,_,_) -> s) (Encoders.param (Encoders.nonNullable Encoders.text))
                    <> contramap (\(_,u,_,_,_,_) -> u) (Encoders.param (Encoders.nonNullable Encoders.timestamptz))
                    <> contramap (\(_,_,e,_,_,_) -> e) (Encoders.param (Encoders.nonNullable Encoders.text))
                    <> contramap (\(_,_,_,i,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
                    <> contramap (\(_,_,_,_,w,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
                    <> contramap (\(_,_,_,_,_,l) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
            let statement = Hasql.unpreparable sql encoder Decoders.noResult
            runPool pool (HasqlSession.statement (inputValue finalStatus, now, lastError, jobId, job.lockedBy, job.lockedAt) statement)


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
    _ <- requeueJob pool job Nothing Nothing True
    pure ()

-- | Return the claimed execution to the queue. Returns False when a unique
-- index already holds a pending job for the same work: the execution is then
-- recorded as failed, because the pending job covers it.
requeueJob :: forall job.
    ( Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    ) => HasqlPool.Pool -> job -> Maybe UTCTime -> Maybe Text -> Bool -> IO Bool
requeueJob pool job nextRunAt lastError refundAttempt = do
    let Id jobId = job.id
    let sql = "SELECT public.ihp_requeue_job($1::regclass, $2, $3, $4, $5, $6, $7)"
    let encoder =
            contramap (\(t,_,_,_,_,_,_) -> t) (Encoders.param (Encoders.nonNullable Encoders.text))
            <> contramap (\(_,i,_,_,_,_,_) -> i) (Encoders.param (Encoders.nonNullable Encoders.uuid))
            <> contramap (\(_,_,w,_,_,_,_) -> w) (Encoders.param (Encoders.nullable Encoders.uuid))
            <> contramap (\(_,_,_,l,_,_,_) -> l) (Encoders.param (Encoders.nullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,r,_,_) -> r) (Encoders.param (Encoders.nullable Encoders.timestamptz))
            <> contramap (\(_,_,_,_,_,e,_) -> e) (Encoders.param (Encoders.nullable Encoders.text))
            <> contramap (\(_,_,_,_,_,_,a) -> a) (Encoders.param (Encoders.nonNullable Encoders.bool))
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    let statement = Hasql.unpreparable sql encoder decoder
    runPool pool (HasqlSession.statement (tableName @job, jobId, job.lockedBy, job.lockedAt, nextRunAt, lastError, refundAttempt) statement)

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
