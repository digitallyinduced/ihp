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
    , setLogger
    ) where

import Prelude
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Records (HasField(..))
import Data.Maybe (fromMaybe)
import qualified Data.TMap as TypeMap
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Log.Types
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai (Request)
import qualified Data.Vault.Lazy as Vault
import IHP.RequestVault (requestFrameworkConfig)
import IHP.RequestVault.Helper (lookupRequestVault)
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
-- Middleware creates an @IORef (Maybe Logger)@ in the vault. 'setLogger' writes to it.
-- The @HasField "logger"@ instance on 'ControllerContext' reads from it.
loggerOverrideVaultKey :: Vault.Key (IORef (Maybe Logger))
loggerOverrideVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE loggerOverrideVaultKey #-}

-- | Override the logger for the current request.
--
-- This can be useful to customize the log formatter for all actions of an app:
--
-- > -- Web/FrontController.hs
-- >
-- > import IHP.Log.Types as Log
-- > import IHP.Controller.Context
-- >
-- > instance InitControllerContext WebApplication where
-- >     initContext = do
-- >         -- ... your other initContext code
-- >         setLogger myCustomLogger
-- >
-- > myCustomLogger :: (?request :: Request) => Logger
-- > myCustomLogger =
-- >     defaultLogger { Log.formatter = myFormatter defaultLogger.formatter }
-- >     where
-- >         defaultLogger = ?request.frameworkConfig.logger
--
setLogger :: (?request :: Request) => Logger -> IO ()
setLogger logger = writeIORef (lookupRequestVault loggerOverrideVaultKey ?request) (Just logger)
{-# INLINE setLogger #-}

-- | Access logger, checking the vault override first, then falling back to frameworkConfig.logger
instance HasField "logger" ControllerContext Logger where
    getField context =
        let request = context.request
            override = unsafePerformIO $ readIORef (lookupRequestVault loggerOverrideVaultKey request)
        in fromMaybe (requestFrameworkConfig request).logger override
    {-# INLINABLE getField #-}
