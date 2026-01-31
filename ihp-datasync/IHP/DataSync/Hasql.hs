module IHP.DataSync.Hasql
( runSession
, runSessionOnConnection
, withPoolConnection
) where

import IHP.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Pool
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Session
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

-- | Run a composed Session against the pool. Throws 'Hasql.Pool.UsageError' on failure.
runSession :: Hasql.Pool.Pool -> Session.Session a -> IO a
runSession pool session = do
    result <- Hasql.Pool.use pool session
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runSession #-}

-- | Run a composed Session on a bare connection. Throws 'Session.QueryError' on failure.
runSessionOnConnection :: Hasql.Connection -> Session.Session a -> IO a
runSessionOnConnection conn session = do
    result <- Session.run session conn
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runSessionOnConnection #-}

-- | Borrow a connection from the pool for the duration of an IO action.
--
-- The connection is returned to the pool when the action completes normally.
-- If the action throws, the connection is destroyed (rolling back any pending transaction).
withPoolConnection :: Hasql.Pool.Pool -> (Hasql.Connection -> IO a) -> IO a
withPoolConnection pool action = do
    result <- Hasql.Pool.use pool $ do
        conn <- ask
        liftIO (action conn)
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
