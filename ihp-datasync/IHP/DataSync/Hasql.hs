module IHP.DataSync.Hasql
( runHasql
, runHasqlOnConnection
) where

import IHP.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Pool
import qualified Hasql.Connection as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Hasql

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
-- Throws 'Hasql.SessionError' on failure.
runHasqlOnConnection :: Hasql.Connection -> Snippet -> Decoders.Result a -> IO a
runHasqlOnConnection conn snippet decoder = do
    result <- Hasql.run (DynSession.dynamicallyParameterizedStatement snippet decoder True) conn
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runHasqlOnConnection #-}
