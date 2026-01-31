module IHP.DataSync.Pool
( hasqlPoolVaultKey
, hasqlPoolMiddleware
, requestHasqlPool
, initHasqlPoolMiddleware
) where

import IHP.Prelude
import Network.Wai
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection

hasqlPoolVaultKey :: Vault.Key Hasql.Pool.Pool
hasqlPoolVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE hasqlPoolVaultKey #-}

hasqlPoolMiddleware :: Hasql.Pool.Pool -> Middleware
hasqlPoolMiddleware pool app req respond =
    let req' = req { vault = Vault.insert hasqlPoolVaultKey pool req.vault }
    in app req' respond

requestHasqlPool :: Request -> Hasql.Pool.Pool
requestHasqlPool req = case Vault.lookup hasqlPoolVaultKey req.vault of
    Just pool -> pool
    Nothing -> error "requestHasqlPool: No hasql pool in request vault. Add hasqlPoolMiddleware to your Config.hs."

-- | Convenience: creates pool and returns middleware. Pool lives for app lifetime.
initHasqlPoolMiddleware :: ByteString -> Int -> IO Middleware
initHasqlPoolMiddleware databaseUrl poolSize = do
    let poolConfig = Hasql.Pool.Config.settings
            [ Hasql.Pool.Config.size poolSize
            , Hasql.Pool.Config.staticConnectionSettings
                [HasqlSetting.connection (HasqlConnection.string (cs databaseUrl))]
            ]
    pool <- Hasql.Pool.acquire poolConfig
    pure (hasqlPoolMiddleware pool)
