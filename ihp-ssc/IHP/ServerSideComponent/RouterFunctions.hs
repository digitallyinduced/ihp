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
import Network.Wai
import Data.Attoparsec.ByteString.Char8 (Parser)

routeComponent :: forall component controller application.
    ( Typeable component
    , Component component controller
    , FromJSON controller
    , InitControllerContext application
    , Typeable application
    , ?application :: application
    , ?context :: RequestContext
    ) => Parser Application
routeComponent = webSocketAppWithCustomPath @(ComponentsController component) @application ("SSC/" <> typeName)
    where
        typeName :: ByteString
        typeName = Typeable.typeOf (error "unreachable" :: component)
                |> Prelude.show
                |> ByteString.pack
