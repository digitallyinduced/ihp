{-|
Module: IHP.IDE.ToolServer.Helper.Controller
Description: Provides helpers for controllers of the ToolServer
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.ToolServer.Helper.Controller (appPort) where

import IHP.Prelude
import IHP.ControllerSupport
import IHP.ModelSupport
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import qualified IHP.IDE.PortConfig as PortConfig
import IHP.IDE.Types
import qualified Network.Socket as Socket

-- | Returns the port used by the running app. Usually returns @8000@.
appPort :: (?controllerContext :: ControllerContext) => Socket.PortNumber
appPort = (fromControllerContext @ToolServerApplication)
        |> get #devServerContext
        |> get #portConfig
        |> get #appPort

