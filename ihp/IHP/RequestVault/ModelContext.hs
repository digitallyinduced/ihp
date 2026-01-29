module IHP.RequestVault.ModelContext
( -- * ModelContext
  modelContextVaultKey
, modelContextMiddleware
, requestModelContext
  -- * RequestBody (re-exported from RequestBodyMiddleware)
, RequestBody (..)
, requestBodyVaultKey
) where

import Prelude
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

(modelContextMiddleware, requestModelContext) = insertVaultMiddlewareAndGetter modelContextVaultKey

-- request.parsedBody
requestParsedBody :: Request -> RequestBody
requestParsedBody = lookupRequestVault requestBodyVaultKey

-- Field access helpers
instance HasField "modelContext" Request ModelContext where getField request = requestModelContext request
instance HasField "parsedBody" Request RequestBody where getField request = requestParsedBody request
