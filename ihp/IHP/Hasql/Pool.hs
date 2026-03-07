module IHP.Hasql.Pool (usePoolWithRetry, isCachedPlanError, isCachedPlanSessionError) where

import Prelude
import Control.Exception (throwIO)
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as Hasql
import qualified Hasql.Errors as HasqlErrors
import System.IO (hPutStrLn, stderr)
import IHP.ModelSupport.Types (HasqlError(..))

-- | Like 'HasqlPool.use' but retries once on cached-plan errors
-- (e.g. after @make db@ invalidates prepared statement type caches).
-- Releases the pool before retrying so fresh connections are used.
--
-- Follows the same pattern as hasql-transaction's @inRetryingTransaction@
-- which retries on serialization failures (40001) and deadlocks (40P01),
-- but operates at the pool level since cached plan errors (0A000) require
-- a fresh connection rather than just re-running the session.
usePoolWithRetry :: HasqlPool.Pool -> Hasql.Session a -> IO a
usePoolWithRetry pool session = do
    result <- HasqlPool.use pool session
    case result of
        Left err
            | isCachedPlanError err -> do
                hPutStrLn stderr "Resetting hasql connection pool due to stale prepared statements (e.g. after 'make db')"
                HasqlPool.release pool
                retryResult <- HasqlPool.use pool session
                case retryResult of
                    Left retryErr -> throwIO (HasqlError retryErr)
                    Right a -> pure a
            | otherwise -> throwIO (HasqlError err)
        Right a -> pure a

-- | Detects errors caused by stale schema after @make db@ recreates the database.
--
-- Matches four categories:
--
-- 1. PostgreSQL \"cached plan must not change result type\" (error code 0A000) —
--    the server rejects a prepared statement whose result columns changed.
--
-- 2. PostgreSQL \"cache lookup failed for type\" (error code XX000) —
--    a prepared statement references a type OID that no longer exists after
--    schema recreation (types get new OIDs).
--
-- 3. Hasql 'MissingTypesSessionError' — custom enum types (e.g. @JOB_STATUS@)
--    get new OIDs after schema recreation, and hasql's type registry can't find them.
--
-- 4. Hasql 'UnexpectedColumnTypeStatementError' — the column's type OID no longer
--    matches the OID cached in the prepared statement / decoder.
isCachedPlanError :: HasqlPool.UsageError -> Bool
isCachedPlanError (HasqlPool.SessionUsageError sessionError) = isCachedPlanSessionError sessionError
isCachedPlanError _ = False

isCachedPlanSessionError :: HasqlErrors.SessionError -> Bool
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.ServerStatementError (HasqlErrors.ServerError "0A000" _ _ _ _))) = True
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.ServerStatementError (HasqlErrors.ServerError "XX000" _ _ _ _))) = True
isCachedPlanSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError "0A000" _ _ _ _)) = True
isCachedPlanSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError "XX000" _ _ _ _)) = True
isCachedPlanSessionError (HasqlErrors.MissingTypesSessionError _) = True
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.UnexpectedColumnTypeStatementError _ _ _)) = True
isCachedPlanSessionError _ = False
