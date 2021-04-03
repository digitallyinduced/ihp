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
import Network.Wai
import Data.Attoparsec.ByteString.Char8 (Parser)
import IHP.ServerSideComponent.Controller.ComponentsController

routeComponent :: forall component controller. (Typeable component, Component component controller, Read controller, _) => Parser (IO ResponseReceived)
routeComponent = webSocketAppWithCustomPath @(ComponentsController component) ("SSC/" <> typeName)
    where
        typeName :: ByteString
        typeName = Typeable.typeOf (error "unreachable" :: component)
                |> Prelude.show
                |> ByteString.pack