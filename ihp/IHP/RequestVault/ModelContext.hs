module IHP.RequestVault.ModelContext
( -- * ModelContext
  modelContextVaultKey
, modelContextMiddleware
, requestModelContext
  -- * RequestBody (re-exported from RequestBodyMiddleware)
, RequestBody (..)
, requestBodyVaultKey
) where

import GHC.Records (HasField(..))
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import IHP.ModelSupport.Types (ModelContext)
import IHP.RequestVault.Helper
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)

-- request.modelContext
modelContextVaultKey :: Vault.Key ModelContext
modelContextVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE modelContextVaultKey #-}

{-# INLINE modelContextMiddleware #-}
modelContextMiddleware :: ModelContext -> Middleware
modelContextMiddleware = insertVaultMiddleware modelContextVaultKey

{-# INLINE requestModelContext #-}
requestModelContext :: Request -> ModelContext
requestModelContext = lookupRequestVault modelContextVaultKey

-- request.parsedBody
{-# INLINE requestParsedBody #-}
requestParsedBody :: Request -> RequestBody
requestParsedBody = lookupRequestVault requestBodyVaultKey

-- Field access helpers
instance HasField "modelContext" Request ModelContext where
    {-# INLINE getField #-}
    getField request = requestModelContext request
instance HasField "parsedBody" Request RequestBody where
    {-# INLINE getField #-}
    getField request = requestParsedBody request
