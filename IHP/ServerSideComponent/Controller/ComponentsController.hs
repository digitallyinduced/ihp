module IHP.ServerSideComponent.Controller.ComponentsController where

import Web.Controller.Prelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC
import Prelude (read)

instance (Component component controller, Read controller) => WSApp (ComponentsController component) where
    initialState = ComponentsController

    run = do
        let state :: component = SSC.initialState
        stateRef <- newIORef state
        let ?stateRef = stateRef

        nextState <- componentDidMount state
        SSC.setState nextState

        forever do
            actionPayload :: Text <- receiveData

            let theAction :: controller = read (cs actionPayload)

            currentState <- SSC.getState

            nextState <- SSC.action currentState theAction
            SSC.setState nextState