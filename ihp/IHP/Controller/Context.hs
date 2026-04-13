{-|
Module: IHP.Controller.Context
Copyright: (c) digitally induced GmbH, 2020

A thin wrapper around the WAI 'Request' that's threaded through controllers
and views as the @?context@ implicit parameter. All request-scoped state
lives in @request.vault@ now; see 'IHP.RequestVault'.
-}
module IHP.Controller.Context
    ( ControllerContext(..)
    , newControllerContext
    , ActionType(..)
    ) where

import Prelude
import GHC.Records (HasField(..))
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Log.Types
import Network.Wai (Request)
import IHP.RequestVault (requestFrameworkConfig, requestLogger)
import IHP.ActionType (ActionType(..))

-- | Wraps the WAI 'Request' that's threaded through controllers and views.
--
-- The @request@ field accessor lets you write @?context.request@. Other
-- common fields (@frameworkConfig@, @logger@) are provided via 'HasField'
-- instances that delegate to the underlying request vault.
data ControllerContext = ControllerContext { request :: Request }

-- | Creates a controller context wrapping the current request.
newControllerContext :: (?request :: Request) => IO ControllerContext
newControllerContext = pure ControllerContext { request = ?request }
{-# INLINE newControllerContext #-}

-- | @?context.frameworkConfig@ delegates to @?context.request.frameworkConfig@.
instance HasField "frameworkConfig" ControllerContext FrameworkConfig where
    getField context = requestFrameworkConfig context.request
    {-# INLINABLE getField #-}

-- | @?context.logger@ delegates to @?context.request.logger@.
instance HasField "logger" ControllerContext Logger where
    getField context = requestLogger context.request
    {-# INLINABLE getField #-}
