{-|
Module: IHP.ServerSideComponent.Types
Description: Types & Data Structures for IHP SSC
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.Types where

import IHP.ViewPrelude hiding (render)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Web.Controller.Prelude hiding (render, setState)
import qualified Network.WebSockets as WebSocket

class Component state action | state -> action where
    initialState :: state
    render :: state -> Html
    action ::
        ( ?stateRef :: IORef state
        , ?connection :: WebSocket.Connection
        , ?context :: ControllerContext
        , ?modelContext :: ModelContext
        ) => state -> action -> IO state

    componentDidMount ::
        ( ?stateRef :: IORef state
        , ?connection :: WebSocket.Connection
        , ?context :: ControllerContext
        , ?modelContext :: ModelContext
        ) => state -> IO state
    componentDidMount state = pure state

data ComponentsController components
    = ComponentsController
    deriving (Eq, Show, Data)