{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Queue.Fetch
( withNextJob
, pendingJobConditionSQL
, runConn
) where

import IHP.Prelude
import IHP.ModelSupport (Table (..), GetModelByTableName)
import IHP.ModelSupport.Types (PrimaryKey, HasqlSessionError(..), HasqlConnectionError(..))
import IHP.Hasql.FromRow (FromRowHasql (..))
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Connection as HasqlConnection
import qualified Hasql.Connection.Settings as HasqlConnectionSettings
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text
import qualified Control.Exception.Safe as Exception

-- | Fetch and lock a job inside a database transaction on a dedicated connection.
--
-- Opens a raw connection (not from the pool), begins a transaction, and atomically
-- locks the next pending job. The callback receives the connection and the locked job.
-- The callback is responsible for committing the transaction (via 'jobDidSucceedConn'
-- or 'jobDidFailConn'). If the worker process crashes, PostgreSQL automatically rolls
-- back the transaction and the job reverts to its previous state, immediately available
-- for other workers.
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
    ) => ByteString -> UUID -> (HasqlConnection.Connection -> job -> IO a) -> IO (Maybe a)
withNextJob databaseUrl workerId callback = do
    connResult <- HasqlConnection.acquire (HasqlConnectionSettings.connectionString (cs databaseUrl))
    case connResult of
        Left err -> Exception.throwIO (HasqlConnectionError err)
        Right conn -> do
            let cleanup = HasqlConnection.release conn
            flip Exception.finally cleanup do
                runConn conn (HasqlSession.script "BEGIN")
                let statement = fetchNextJobStatement @job
                maybeJob <- runConn conn (HasqlSession.statement workerId statement)
                case maybeJob of
                    Nothing -> do
                        runConn conn (HasqlSession.script "ROLLBACK")
                        pure Nothing
                    Just job -> do
                        result <- callback conn job `Exception.onException`
                            -- On unhandled exception, rollback so the job reverts to not_started
                            Exception.tryAny (runConn conn (HasqlSession.script "ROLLBACK"))
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

-- | Shared WHERE condition for fetching pending jobs as a SQL text fragment.
-- Matches jobs that are either not started or in retry state,
-- not locked, and whose run_at time has passed.
-- Enum values are inlined as SQL string literals (PostgreSQL casts them to job_status).
pendingJobConditionSQL :: Text
pendingJobConditionSQL =
    "(status = 'job_status_not_started'"
    <> " OR status = 'job_status_retry'"
    <> ") AND locked_by IS NULL AND run_at <= NOW()"
