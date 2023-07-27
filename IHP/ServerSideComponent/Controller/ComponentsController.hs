{-# LANGUAGE  UndecidableInstances #-}
module IHP.ServerSideComponent.Controller.ComponentsController where

import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC

import qualified Data.Aeson as Aeson

instance (Component component controller, Aeson.FromJSON component, FromJSON controller, ToJSON controller) => WSApp (ComponentsController component) where
    initialState = ComponentsController

    run = do
        let state :: component = SSC.initialState
        instanceRef <- newIORef (ComponentInstance { state })
        let ?instanceRef = instanceRef

        nextState <- componentDidMount state
        SSC.setState nextState

        forever do
            actionPayload :: LByteString <- receiveData

            let theAction = Aeson.eitherDecode @controller actionPayload

            case theAction of
                Right theAction -> do
                    currentState <- SSC.getState

                    nextState <- SSC.action currentState theAction
                    SSC.setState nextState
                Left error -> do
                    let theState = Aeson.eitherDecode @component actionPayload

                    case theState of
                        Right initialState -> do
                            SSC.setState initialState
                        Left error -> do
                            putStrLn "Failed Parsing Server Side Component Message As JSON"
                            putStrLn (cs actionPayload)
                            putStrLn (cs error)
