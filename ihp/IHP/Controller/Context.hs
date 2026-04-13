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
    ) where

import Prelude
import Data.IORef (newIORef, readIORef)
import GHC.Records (HasField(..))
import qualified Data.TMap as TypeMap
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Log.Types
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai (Request)
import IHP.RequestVault (requestFrameworkConfig, requestLogger)
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

-- | Access logger from the request vault
instance HasField "logger" ControllerContext Logger where
    getField context = requestLogger context.request
    {-# INLINABLE getField #-}
