{-# LANGUAGE  UndecidableInstances #-}
module IHP.ServerSideComponent.Controller.ComponentsController where

import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC

import qualified Data.Aeson as Aeson
import qualified IHP.Log as Log

instance (Component component controller, FromJSON controller) => WSApp (ComponentsController component) where
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
                    Log.error ("Failed Parsing Server Side Component Message As JSON" :: Text)
                    Log.error actionPayload
                    Log.error error
