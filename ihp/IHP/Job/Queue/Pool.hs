module IHP.Job.Queue.Pool
( runPool
) where

import IHP.Prelude
import IHP.Hasql.Pool (usePoolWithRetry)
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession

-- | Run a hasql session against the pool, retrying once on cached plan errors.
runPool :: HasqlPool.Pool -> HasqlSession.Session a -> IO a
runPool = usePoolWithRetry
