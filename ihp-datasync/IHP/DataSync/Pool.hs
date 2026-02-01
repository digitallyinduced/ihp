module IHP.DataSync.Pool
( hasqlPoolVaultKey
, hasqlPoolMiddleware
, requestHasqlPool
, initHasqlPoolMiddleware
, initHasqlPool
, initHasqlPoolWithRLS
) where

import IHP.Prelude
import Network.Wai
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import IHP.FrameworkConfig (findOptionOrNothing, configIO, defaultDatabaseUrl)
import IHP.FrameworkConfig.Types (DatabaseUrl(..), DBPoolMaxConnections(..), CustomMiddleware(..), RLSAuthenticatedRole(..))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified Control.Concurrent as Concurrent
import qualified IHP.DataSync.Role as Role

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

-- | Reads 'DatabaseUrl' and 'DBPoolMaxConnections' from the framework config,
-- creates a hasql connection pool, and composes 'hasqlPoolMiddleware' into the
-- existing 'CustomMiddleware' stack.
--
-- Use this in your @Config.hs@:
--
-- > config :: ConfigBuilder
-- > config = do
-- >     initHasqlPool
initHasqlPool :: State.StateT TMap.TMap IO ()
initHasqlPool = do
    initHasqlPoolImpl
    pure ()

-- | Like 'initHasqlPool', but also ensures the RLS authenticated role exists
-- in postgres. Reads 'RLSAuthenticatedRole' from the config (default: @ihp_authenticated@).
--
-- Use this in your @Config.hs@:
--
-- > config :: ConfigBuilder
-- > config = do
-- >     initHasqlPoolWithRLS
initHasqlPoolWithRLS :: State.StateT TMap.TMap IO ()
initHasqlPoolWithRLS = do
    pool <- initHasqlPoolImpl
    rlsRole <- findOptionOrNothing @RLSAuthenticatedRole >>= \case
        Just (RLSAuthenticatedRole role) -> pure role
        Nothing -> pure "ihp_authenticated"
    configIO $ Role.ensureAuthenticatedRoleExistsWithRole pool rlsRole

-- | Shared implementation: creates the pool, installs the middleware, returns the pool.
initHasqlPoolImpl :: State.StateT TMap.TMap IO Hasql.Pool.Pool
initHasqlPoolImpl = do
    databaseUrl <- findOptionOrNothing @DatabaseUrl >>= \case
        Just (DatabaseUrl url) -> pure url
        Nothing -> configIO defaultDatabaseUrl
    maxConnections <- findOptionOrNothing @DBPoolMaxConnections >>= \case
        Just (DBPoolMaxConnections n) -> pure n
        Nothing -> configIO $ max 20 <$> Concurrent.getNumCapabilities
    pool <- configIO $ do
        let poolConfig = Hasql.Pool.Config.settings
                [ Hasql.Pool.Config.size maxConnections
                , Hasql.Pool.Config.staticConnectionSettings
                    [HasqlSetting.connection (HasqlConnection.string (cs databaseUrl))]
                ]
        Hasql.Pool.acquire poolConfig
    existingMiddleware <- findOptionOrNothing @CustomMiddleware >>= \case
        Just (CustomMiddleware mw) -> pure mw
        Nothing -> pure id
    State.modify (\map -> map
            |> TMap.delete @CustomMiddleware
            |> TMap.insert (CustomMiddleware (existingMiddleware . hasqlPoolMiddleware pool))
        )
    pure pool
