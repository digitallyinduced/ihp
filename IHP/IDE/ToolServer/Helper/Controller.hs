{-|
Module: IHP.IDE.ToolServer.Helper.Controller
Description: Provides helpers for controllers of the ToolServer
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.ToolServer.Helper.Controller (appPort, openEditor) where

import IHP.Prelude
import IHP.ControllerSupport
import IHP.ModelSupport
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import qualified IHP.IDE.PortConfig as PortConfig
import IHP.IDE.Types
import qualified Network.Socket as Socket
import qualified System.Process as Process

-- | Returns the port used by the running app. Usually returns @8000@.
appPort :: (?controllerContext :: ControllerContext) => Socket.PortNumber
appPort = (fromControllerContext @ToolServerApplication)
        |> get #devServerContext
        |> get #portConfig
        |> get #appPort

openEditor :: Text -> Int -> Int -> IO ()
openEditor path line col = do
    _ <- Process.system $ cs $ "sublime " <> path <> ":" <> tshow line <> ":" <> tshow col
    pure ()