module IHP.Job.Queue.Pool
( runPool
) where

import IHP.Prelude
import IHP.ModelSupport (HasqlError (..), isCachedPlanError)
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession

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
