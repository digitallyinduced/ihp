{-|
Module: IHP.ServerSideComponent.ControllerFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.ControllerFunctions where

import IHP.Prelude
import Web.Controller.Prelude hiding (render, setState, getState)
import IHP.ServerSideComponent.Types as SSC

import qualified Network.WebSockets as WebSocket
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

updateState :: (?stateRef :: IORef state, ?connection :: WebSocket.Connection, Component state action, ?context :: ControllerContext) => (state -> state) -> IO ()
updateState updateFn = do
    modifyIORef' ?stateRef updateFn
    state <- readIORef ?stateRef
    let result = SSC.render state |> Blaze.renderHtml

    sendTextData result

setState :: (?stateRef :: IORef state, ?connection :: WebSocket.Connection, Component state action, ?context :: ControllerContext) => state -> IO ()
setState state = do
    writeIORef ?stateRef state
    let result = SSC.render state |> Blaze.renderHtml

    sendTextData result


getState :: _ => _
getState = readIORef ?stateRef

callServerAction :: Show action => action -> Text
callServerAction action = "callServerAction('" <> tshow action <> "')"