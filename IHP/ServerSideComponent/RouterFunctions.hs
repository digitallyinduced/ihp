{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.ServerSideComponent.RouterFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.RouterFunctions where

import IHP.Prelude
import IHP.ServerSideComponent.Types
import qualified Data.Typeable as Typeable
import qualified Data.ByteString.Char8 as ByteString
import IHP.RouterSupport
import qualified Prelude
import IHP.ServerSideComponent.Controller.ComponentsController ()
import Data.Aeson
import IHP.ControllerSupport
import IHP.ApplicationContext

routeComponent :: forall component controller application.
    ( Typeable component
    , FromJSON component
    , Component component controller
    , FromJSON controller
    , ToJSON controller
    , InitControllerContext application
    , Typeable application
    , ?application :: application
    , ?applicationContext :: IHP.ApplicationContext.ApplicationContext
    , ?context :: RequestContext
    ) => RouteParser
routeComponent = webSocketAppWithCustomPath @(ComponentsController component) @application ("SSC/" <> typeName)
    where
        typeName :: ByteString
        typeName = Typeable.typeOf (error "unreachable" :: component)
                |> Prelude.show
                |> ByteString.pack
