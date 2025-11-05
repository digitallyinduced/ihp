module IHP.RequestVault where

import IHP.Prelude
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import Data.Proxy
import Data.Typeable
import IHP.FrameworkConfig
import IHP.PGListener

insertVaultMiddleware :: Vault.Key value -> value -> Middleware
insertVaultMiddleware key value app req respond = do
    let req' = req { vault = Vault.insert key value req.vault }
    app req' respond

lookupRequestVault :: forall value. Typeable value => Vault.Key value -> Request -> value
lookupRequestVault key req =
    case Vault.lookup key req.vault of
        Just modelContext -> modelContext
        Nothing -> error $ "lookupRequestVault: Could not find " <> show (typeRep (Proxy @value) ) <> " in request.vault. Did you forget to add the middleware to your application?"

insertVaultMiddlewareAndGetter :: Typeable value => Vault.Key value -> (value -> Middleware, Request -> value)
insertVaultMiddlewareAndGetter key = (insertVaultMiddleware key, lookupRequestVault key)

-- request.modelContext
modelContextVaultKey :: Vault.Key ModelContext
modelContextVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE modelContextVaultKey #-}

(modelContextMiddleware, requestModelContext) = insertVaultMiddlewareAndGetter modelContextVaultKey

-- request.frameworkConfig
frameworkConfigVaultKey :: Vault.Key FrameworkConfig
frameworkConfigVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE frameworkConfigVaultKey #-}

(frameworkConfigMiddleware, requestFrameworkConfig) = insertVaultMiddlewareAndGetter frameworkConfigVaultKey

-- request.pgListener
pgListenerVaultKey :: Vault.Key PGListener
pgListenerVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pgListenerVaultKey #-}

(pgListenerMiddleware, requestPGListener) = insertVaultMiddlewareAndGetter pgListenerVaultKey

-- Field access helpers
instance HasField "frameworkConfig" Request FrameworkConfig where getField request = requestFrameworkConfig request
instance HasField "modelContext" Request ModelContext where getField request = requestModelContext request
instance HasField "pgListener" Request PGListener where getField request = requestPGListener request