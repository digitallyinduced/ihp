{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Queue.Fetch
( fetchNextJob
, pendingJobConditionSQL
) where

import IHP.Prelude
import IHP.Job.Queue.Pool (runPool)
import IHP.ModelSupport (Table (..), GetModelByTableName)
import IHP.ModelSupport.Types (PrimaryKey)
import IHP.Hasql.FromRow (FromRowHasql (..))
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text

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
