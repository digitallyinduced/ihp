module IHP.Hasql.Pool (usePoolWithRetry) where

import Prelude
import Control.Exception (throwIO)
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as Hasql
import qualified Hasql.Errors as HasqlErrors
import IHP.ModelSupport.Types (HasqlError(..))

-- | Run a session on the pool, retrying on stale prepared-statement errors.
--
-- After schema changes (e.g. @make db@), pooled connections have stale caches.
-- hasql-pool auto-discards these connections, so retrying cycles through the
-- pool until a fresh connection is created.
usePoolWithRetry :: HasqlPool.Pool -> Hasql.Session a -> IO a
usePoolWithRetry pool session = do
    result <- HasqlPool.use pool session
    case result of
        Left err
            | isCachedPlanError err -> usePoolWithRetry pool session
            | otherwise -> throwIO (HasqlError err)
        Right a -> pure a

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
