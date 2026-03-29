module IHP.DataSync.Hasql
( runSession
, runSessionOnConnection
, withDedicatedConnection
) where

import IHP.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Pool
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import qualified Hasql.Errors as Hasql

-- | Run a composed Session against the pool. Throws 'Hasql.Pool.UsageError' on failure.
runSession :: Hasql.Pool.Pool -> Session.Session a -> IO a
runSession pool session = do
    result <- Hasql.Pool.use pool session
    case result of
        Left err -> Exception.throwIO err
        Right val -> pure val
{-# INLINE runSession #-}

-- | Run a composed Session on a bare connection. Throws on failure.
runSessionOnConnection :: Hasql.Connection -> Session.Session a -> IO a
runSessionOnConnection conn session = do
    result <- Hasql.use conn session
    case result of
        Left err -> error (Hasql.toDetailedText err)
        Right val -> pure val
{-# INLINE runSessionOnConnection #-}

-- | Acquire a dedicated connection for the duration of an IO action.
--
-- Used for long-lived transactions in DataSync where a connection must be held
-- open across multiple message handlers. The connection is released when the
-- action completes (normally or via exception).
withDedicatedConnection :: ByteString -> (Hasql.Connection -> IO a) -> IO a
withDedicatedConnection databaseUrl action = do
    connResult <- Hasql.acquire (HasqlSettings.connectionString (cs databaseUrl))
    case connResult of
        Right conn -> action conn `Exception.finally` Hasql.release conn
        Left err -> error (Hasql.toDetailedText err)
