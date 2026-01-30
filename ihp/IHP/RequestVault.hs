module IHP.RequestVault
( -- * Vault infrastructure (re-exported from Helper)
  module IHP.RequestVault.Helper
  -- * ModelContext (re-exported from ModelContext)
, module IHP.RequestVault.ModelContext
  -- * FrameworkConfig
, frameworkConfigVaultKey
, frameworkConfigMiddleware
, requestFrameworkConfig
  -- * PGListener
, pgListenerVaultKey
, pgListenerMiddleware
, requestPGListener
) where

import IHP.Prelude
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import Data.Proxy
import Data.Typeable
import IHP.FrameworkConfig
import IHP.PGListener
import IHP.RequestVault.Helper
import IHP.RequestVault.ModelContext

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
instance HasField "pgListener" Request PGListener where getField request = requestPGListener request
