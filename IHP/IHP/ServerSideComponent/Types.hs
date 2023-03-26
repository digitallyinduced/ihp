{-|
Module: IHP.ServerSideComponent.Types
Description: Types & Data Structures for IHP SSC
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.Types where

import IHP.ViewPrelude
import qualified Network.WebSockets as WebSocket

class Component state action | state -> action where
    initialState :: state
    render :: state -> Html
    action ::
        ( ?instanceRef :: IORef (ComponentInstance state)
        , ?connection :: WebSocket.Connection
        , ?context :: ControllerContext
        , ?modelContext :: ModelContext
        ) => state -> action -> IO state

    componentDidMount ::
        ( ?instanceRef :: IORef (ComponentInstance state)
        , ?connection :: WebSocket.Connection
        , ?context :: ControllerContext
        , ?modelContext :: ModelContext
        ) => state -> IO state
    componentDidMount state = pure state

data ComponentsController components
    = ComponentsController
    deriving (Eq, Show, Data)

data ComponentInstance state
    = ComponentInstance { state :: state } -- ^ If you wondered why the current rendered HTML doesn't need to be stored here for later diffing it: As our render functions are pure we can just re-render the HTML based on the state when we want to do our diffing

instance (SetField "state" (ComponentInstance state) state) where
    setField state componentInstance = componentInstance { state }
