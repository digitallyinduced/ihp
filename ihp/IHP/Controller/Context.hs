{-|
Module: IHP.Controller.Context
Copyright: (c) digitally induced GmbH, 2020

Re-exports from ihp-context and adds IHP-specific HasField instances
for accessing the WAI Request and FrameworkConfig.
-}
module IHP.Controller.Context
    ( ControllerContext(..)
    , newControllerContext
    , freeze
    , unfreeze
    , putContext
    , fromContext
    , maybeFromContext
    , fromFrozenContext
    , maybeFromFrozenContext
    , ActionType(..)
    , loggerOverrideVaultKey
    ) where

import Prelude
import Data.IORef (newIORef, readIORef)
import GHC.Records (HasField(..))
import Data.Maybe (fromMaybe)
import qualified Data.TMap as TypeMap
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Log.Types
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai (Request, vault)
import qualified Data.Vault.Lazy as Vault
import IHP.RequestVault (requestFrameworkConfig)
import IHP.ActionType (ActionType(..))

-- Re-export from ihp-context, but we shadow newControllerContext
import IHP.ControllerContext (ControllerContext(..), freeze, unfreeze, putContext, fromContext, maybeFromContext, fromFrozenContext, maybeFromFrozenContext)

-- | Creates a new controller context with the WAI Request stored in the TMap
--
-- This version stores the Request in the TMap so it can be retrieved
-- via the HasField instance.
newControllerContext :: (?request :: Request) => IO ControllerContext
newControllerContext = do
    customFieldsRef <- newIORef (TypeMap.insert ?request TypeMap.empty)
    pure ControllerContext { customFieldsRef }
{-# INLINE newControllerContext #-}

-- | Access request from the TMap
--
-- This allows @controllerContext.request@ to work by retrieving
-- the WAI Request stored in the TMap.
instance HasField "request" ControllerContext Request where
    getField (FrozenControllerContext { customFields }) =
        case TypeMap.lookup @Request customFields of
            Just req -> req
            Nothing -> error "request: Request not found in controller context. Did you forget to call newControllerContext?"
    getField (ControllerContext { customFieldsRef }) =
        -- Hacky but necessary - we need to read the IORef in a pure context
        unsafePerformIO $ do
            customFields <- readIORef customFieldsRef
            case TypeMap.lookup @Request customFields of
                Just req -> pure req
                Nothing -> error "request: Request not found in controller context. Did you forget to call newControllerContext?"
    {-# INLINABLE getField #-}

-- | Access frameworkConfig via the request vault
instance HasField "frameworkConfig" ControllerContext FrameworkConfig where
    getField controllerContext = requestFrameworkConfig controllerContext.request
    {-# INLINABLE getField #-}

-- | Vault key for per-request logger overrides.
--
-- To override the logger, install a middleware via 'CustomMiddleware' in Config.hs:
--
-- > import IHP.Controller.Context (loggerOverrideVaultKey)
-- > import IHP.RequestVault.Helper (insertVaultMiddleware)
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option $ CustomMiddleware (insertVaultMiddleware loggerOverrideVaultKey myCustomLogger)
--
loggerOverrideVaultKey :: Vault.Key Logger
loggerOverrideVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE loggerOverrideVaultKey #-}

-- | Access logger, checking the vault override first, then falling back to frameworkConfig.logger
instance HasField "logger" ControllerContext Logger where
    getField context =
        let request = context.request
        in fromMaybe (requestFrameworkConfig request).logger (Vault.lookup loggerOverrideVaultKey (vault request))
    {-# INLINABLE getField #-}
