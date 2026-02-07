module IHP.RequestVault.Helper
( insertVaultMiddleware
, lookupRequestVault
, insertVaultMiddlewareAndGetter
) where

import Prelude
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import Data.Proxy
import Data.Typeable

insertVaultMiddleware :: Vault.Key value -> value -> Middleware
insertVaultMiddleware key value app req respond = do
    let req' = req { vault = Vault.insert key value req.vault }
    app req' respond

{-# INLINE lookupRequestVault #-}
lookupRequestVault :: forall value. Typeable value => Vault.Key value -> Request -> value
lookupRequestVault key req =
    case Vault.lookup key req.vault of
        Just modelContext -> modelContext
        Nothing -> error $ "lookupRequestVault: Could not find " <> show (typeRep (Proxy @value) ) <> " in request.vault. Did you forget to add the middleware to your application?"

insertVaultMiddlewareAndGetter :: Typeable value => Vault.Key value -> (value -> Middleware, Request -> value)
insertVaultMiddlewareAndGetter key = (insertVaultMiddleware key, lookupRequestVault key)
