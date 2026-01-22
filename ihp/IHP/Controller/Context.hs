{-|
Module: IHP.Controller.Context
Copyright: (c) digitally induced GmbH, 2020

Re-exports from ihp-context and adds IHP-specific HasField instances
for backward compatibility.
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

import IHP.Prelude
import qualified Data.TMap as TypeMap
import qualified Data.Typeable as Typeable
import IHP.Controller.RequestContext (RequestContext)
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Log.Types
import System.IO.Unsafe (unsafePerformIO)

-- Re-export from ihp-context, but we shadow newControllerContext
import IHP.ControllerContext (ControllerContext(..), freeze, unfreeze, putContext, fromContext, maybeFromContext, fromFrozenContext, maybeFromFrozenContext)
import qualified IHP.ControllerContext as Context

-- | Creates a new controller context with RequestContext stored in the TMap
--
-- This version stores the RequestContext in the TMap so it can be retrieved
-- via the HasField instance.
newControllerContext :: (?requestContext :: RequestContext) => IO ControllerContext
newControllerContext = do
    customFieldsRef <- newIORef (TypeMap.insert ?requestContext TypeMap.empty)
    pure ControllerContext { customFieldsRef }
{-# INLINABLE newControllerContext #-}

-- | IHP-specific: used to track the current action type
newtype ActionType = ActionType Typeable.TypeRep

-- | Access requestContext from the TMap
--
-- This allows @controllerContext.requestContext@ to work even though
-- requestContext is not a direct field of ControllerContext.
instance HasField "requestContext" ControllerContext RequestContext where
    getField (FrozenControllerContext { customFields }) =
        case TypeMap.lookup @RequestContext customFields of
            Just rc -> rc
            Nothing -> error "requestContext: RequestContext not found in controller context. Did you forget to call newControllerContext?"
    getField (ControllerContext { customFieldsRef }) =
        -- Hacky but necessary - we need to read the IORef in a pure context
        unsafePerformIO $ do
            customFields <- readIORef customFieldsRef
            case TypeMap.lookup @RequestContext customFields of
                Just rc -> pure rc
                Nothing -> error "requestContext: RequestContext not found in controller context. Did you forget to call newControllerContext?"
    {-# INLINABLE getField #-}

-- | Access frameworkConfig via requestContext
instance HasField "frameworkConfig" ControllerContext FrameworkConfig where
    getField controllerContext = controllerContext.requestContext.frameworkConfig
    {-# INLINABLE getField #-}

-- The following hack is bad, but allows us to override the logger using 'putContext'
-- The alternative would be https://github.com/digitallyinduced/ihp/pull/1921 which is also not very nice
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
-- >     -- ... your other initContext code
-- >
-- >     putContext userIdLogger
-- >
-- > userIdLogger :: (?context :: ControllerContext) => Logger
-- > userIdLogger =
-- >     defaultLogger { Log.formatter = userIdFormatter defaultLogger.formatter }
-- >     where
-- >         defaultLogger = ?context.frameworkConfig.logger
-- >
-- >
-- > userIdFormatter :: (?context :: ControllerContext) => Log.LogFormatter -> Log.LogFormatter
-- > userIdFormatter existingFormatter time level string =
-- >     existingFormatter time level (prependUserId string)
-- >
-- > prependUserId :: (?context :: ControllerContext) => LogStr -> LogStr
-- > prependUserId string =
-- >     toLogStr $ userInfo <> show string
-- >     where
-- >         userInfo =
-- >             case currentUserOrNothing of
-- >                 Just currentUser -> "Authenticated user ID: " <> show currentUser.id <> " "
-- >                 Nothing -> "Anonymous user: "
--
-- This design mistake should be fixed in IHP v2
instance HasField "logger" ControllerContext Logger where
    getField context@(FrozenControllerContext { customFields }) = fromMaybe context.frameworkConfig.logger (TypeMap.lookup @Logger customFields)
    getField context = (unsafePerformIO (freeze context)).logger -- Hacky, but there's no better way. The only way to retrieve the logger here, is by reading from the IORef in an unsafe way
