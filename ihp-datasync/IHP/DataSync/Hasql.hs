module IHP.DataSync.Hasql
( runHasql
, runHasqlOnConnection
, runStatement
, withPoolConnection
) where

import IHP.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Pool
import qualified Hasql.Connection as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

-- | Run a hasql snippet using a connection from the given pool.
--
-- Throws 'Hasql.Pool.UsageError' on failure.
runHasql :: Hasql.Pool.Pool -> Snippet -> Decoders.Result a -> IO a
runHasql pool snippet decoder = do
    let session = DynSession.dynamicallyParameterizedStatement snippet decoder True
    result <- Hasql.Pool.use pool session
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runHasql #-}

-- | Run a snippet on an existing hasql connection. Used for transaction-scoped queries.
--
-- Throws 'Hasql.Pool.UsageError' on failure.
runHasqlOnConnection :: Hasql.Connection -> Snippet -> Decoders.Result a -> IO a
runHasqlOnConnection conn snippet decoder = do
    result <- Session.run (DynSession.dynamicallyParameterizedStatement snippet decoder True) conn
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runHasqlOnConnection #-}

-- | Run a typed 'Statement.Statement' using a connection from the given pool.
--
-- Throws 'Hasql.Pool.UsageError' on failure.
runStatement :: Hasql.Pool.Pool -> params -> Statement.Statement params result -> IO result
runStatement pool params stmt = do
    result <- Hasql.Pool.use pool (Session.statement params stmt)
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runStatement #-}

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
