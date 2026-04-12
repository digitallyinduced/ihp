module IHP.RequestVault
( -- * Vault infrastructure (re-exported from Helper)
  module IHP.RequestVault.Helper
  -- * ModelContext (re-exported from ModelContext)
, module IHP.RequestVault.ModelContext
  -- * FrameworkConfig
, frameworkConfigVaultKey
, frameworkConfigMiddleware
, requestFrameworkConfig
  -- * Logger
, loggerVaultKey
, loggerMiddleware
, requestLogger
  -- * PGListener
, pgListenerVaultKey
, pgListenerMiddleware
, requestPGListener
) where

import IHP.Prelude
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import IHP.FrameworkConfig
import IHP.Log.Types (Logger)
import IHP.PGListener
import IHP.RequestVault.Helper
import IHP.RequestVault.ModelContext

-- request.frameworkConfig
frameworkConfigVaultKey :: Vault.Key FrameworkConfig
frameworkConfigVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE frameworkConfigVaultKey #-}

{-# INLINE frameworkConfigMiddleware #-}
frameworkConfigMiddleware :: FrameworkConfig -> Middleware
frameworkConfigMiddleware = insertVaultMiddleware frameworkConfigVaultKey

{-# INLINE requestFrameworkConfig #-}
requestFrameworkConfig :: Request -> FrameworkConfig
requestFrameworkConfig = lookupRequestVault frameworkConfigVaultKey

-- request.pgListener
pgListenerVaultKey :: Vault.Key PGListener
pgListenerVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pgListenerVaultKey #-}

{-# INLINE pgListenerMiddleware #-}
pgListenerMiddleware :: PGListener -> Middleware
pgListenerMiddleware = insertVaultMiddleware pgListenerVaultKey

{-# INLINE requestPGListener #-}
requestPGListener :: Request -> PGListener
requestPGListener = lookupRequestVault pgListenerVaultKey

-- request.logger
loggerVaultKey :: Vault.Key Logger
loggerVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE loggerVaultKey #-}

{-# INLINE loggerMiddleware #-}
loggerMiddleware :: Logger -> Middleware
loggerMiddleware = insertVaultMiddleware loggerVaultKey

{-# INLINE requestLogger #-}
requestLogger :: Request -> Logger
requestLogger = lookupRequestVault loggerVaultKey

-- Field access helpers
instance HasField "frameworkConfig" Request FrameworkConfig where
    {-# INLINE getField #-}
    getField request = requestFrameworkConfig request
instance HasField "pgListener" Request PGListener where
    {-# INLINE getField #-}
    getField request = requestPGListener request
instance HasField "logger" Request Logger where
    {-# INLINE getField #-}
    getField request = requestLogger request
