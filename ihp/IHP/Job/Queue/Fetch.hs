{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Queue.Fetch
( withNextJob
, pendingJobConditionSQL
, runConn
, advisoryLockKey
, advisoryUnlockStatement
) where

import IHP.Prelude
import IHP.ModelSupport (Table (..), GetModelByTableName)
import IHP.ModelSupport.Types (PrimaryKey, Id'(..), HasqlSessionError(..), HasqlConnectionError(..))
import IHP.Hasql.FromRow (FromRowHasql (..))
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Connection as HasqlConnection
import qualified Hasql.Connection.Settings as HasqlConnectionSettings
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text
import qualified Control.Exception.Safe as Exception
import Data.Functor.Contravariant (contramap)

-- | Fetch and lock a job on a dedicated connection with crash recovery.
--
-- Opens a raw connection (not from the pool), atomically fetches and locks the
-- next pending job (committed immediately so the running status is visible),
-- then acquires a session-level advisory lock on that connection.
--
-- The advisory lock is released instantly when the connection drops (e.g. worker
-- crash), which allows 'recoverStaleJobs' to detect and reclaim the job. The
-- row lock from the fetch UPDATE is released as soon as the fetch commits, so
-- the job's @perform@ is free to update the job row without deadlocking.
--
-- The callback receives the connection and the locked job. The callback is
-- responsible for updating the job status and releasing the advisory lock
-- (via 'jobDidSucceedConn' or 'jobDidFailConn').
--
-- Returns 'Nothing' if no pending job is available.
--
-- __Example:__
--
-- > withNextJob @SendMailJob databaseUrl workerId \conn job -> do
-- >     perform job
-- >     jobDidSucceedConn conn job
--
withNextJob :: forall job a.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , Table job
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    ) => ByteString -> UUID -> (HasqlConnection.Connection -> job -> IO a) -> IO (Maybe a)
withNextJob databaseUrl workerId callback = do
    connResult <- HasqlConnection.acquire (HasqlConnectionSettings.connectionString (cs databaseUrl))
    case connResult of
        Left err -> Exception.throwIO (HasqlConnectionError err)
        Right conn -> do
            let cleanup = HasqlConnection.release conn
            flip Exception.finally cleanup do
                -- Fetch the job inside a transaction and commit immediately.
                -- This makes status='running' visible to other connections
                -- (including recoverStaleJobs) and releases the row lock so
                -- perform can freely update the job row without deadlocking.
                runConn conn (HasqlSession.script "BEGIN")
                let statement = fetchNextJobStatement @job
                maybeJob <- runConn conn (HasqlSession.statement workerId statement)
                case maybeJob of
                    Nothing -> do
                        runConn conn (HasqlSession.script "ROLLBACK")
                        pure Nothing
                    Just job -> do
                        runConn conn (HasqlSession.script "COMMIT")

                        -- Acquire a session-level advisory lock. This lock is
                        -- released instantly when the connection drops (crash),
                        -- allowing recoverStaleJobs to detect dead workers.
                        let Id jobId = job.id
                        let lockKey = advisoryLockKey (tableName @job) jobId
                        runConn conn (HasqlSession.statement lockKey advisoryLockStatement)

                        result <- callback conn job `Exception.onException`
                            Exception.tryAny (runConn conn (HasqlSession.statement lockKey advisoryUnlockStatement))
                        pure (Just result)

-- | The SQL statement used by 'withNextJob'.
fetchNextJobStatement :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , Table job
    ) => Hasql.Statement UUID (Maybe job)
fetchNextJobStatement =
    let tableNameText = tableName @job
        returningColumns = Text.intercalate ", " (columnNames @job)
        sql = "UPDATE " <> tableNameText
            <> " SET status = 'job_status_running'"
            <> ", locked_at = NOW(), locked_by = $1"
            <> ", attempts_count = attempts_count + 1"
            <> " WHERE id IN (SELECT id FROM " <> tableNameText
            <> " WHERE " <> pendingJobConditionSQL
            <> " ORDER BY created_at LIMIT 1 FOR UPDATE SKIP LOCKED)"
            <> " RETURNING " <> returningColumns
        encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
        decoder = Decoders.rowMaybe (hasqlRowDecoder @job)
    in Hasql.unpreparable sql encoder decoder

-- | Run a hasql session on a raw connection, throwing on error.
runConn :: HasqlConnection.Connection -> HasqlSession.Session a -> IO a
runConn conn session = do
    result <- HasqlConnection.use conn session
    case result of
        Left err -> Exception.throwIO (HasqlSessionError err)
        Right a -> pure a

-- | Compute the advisory lock key for a job. Uses hashtext(table_name || job_id)
-- to produce a stable int8 key for pg_advisory_lock/pg_advisory_unlock.
advisoryLockKey :: Text -> UUID -> (Text, UUID)
advisoryLockKey tableNameText jobId = (tableNameText, jobId)

-- | Acquire a session-level advisory lock. Released when the connection drops.
advisoryLockStatement :: Hasql.Statement (Text, UUID) ()
advisoryLockStatement =
    let sql = "SELECT pg_advisory_lock(hashtext($1 || $2::text))"
        encoder = contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
                  <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.uuid))
    in Hasql.unpreparable sql encoder Decoders.noResult

-- | Release a session-level advisory lock.
advisoryUnlockStatement :: Hasql.Statement (Text, UUID) ()
advisoryUnlockStatement =
    let sql = "SELECT pg_advisory_unlock(hashtext($1 || $2::text))"
        encoder = contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
                  <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.uuid))
    in Hasql.unpreparable sql encoder Decoders.noResult

-- | Shared WHERE condition for fetching pending jobs as a SQL text fragment.
-- Matches jobs that are either not started or in retry state,
-- not locked, and whose run_at time has passed.
-- Enum values are inlined as SQL string literals (PostgreSQL casts them to job_status).
pendingJobConditionSQL :: Text
pendingJobConditionSQL =
    "(status = 'job_status_not_started'"
    <> " OR status = 'job_status_retry'"
    <> ") AND locked_by IS NULL AND run_at <= NOW()"
