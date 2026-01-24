{-# LANGUAGE  UndecidableInstances #-}
module IHP.ServerSideComponent.Controller.ComponentsController where

import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC

import qualified Data.Aeson as Aeson
import qualified IHP.Log as Log
import qualified Control.Exception as Exception
import Data.Typeable (typeOf)

instance (Component component controller, FromJSON controller, Typeable component) => WSApp (ComponentsController component) where
    initialState = ComponentsController

    run = do
        let state :: component = SSC.initialState
        instanceRef <- newIORef (ComponentInstance { state })
        let ?instanceRef = instanceRef

        let componentName = tshow (typeOf state)
        Log.info ("SSC: Component " <> componentName <> " connected")

        -- Handle componentDidMount with exception handling
        mountResult <- Exception.try (componentDidMount state)
        case mountResult of
            Left (e :: SomeException) -> do
                let errorText = tshow e
                Log.error ("SSC: componentDidMount failed for " <> componentName <> ": " <> errorText)
                SSC.sendError (SSCActionError { errorMessage = "Component initialization failed: " <> errorText })
            Right nextState -> do
                Log.debug ("SSC: Component " <> componentName <> " mounted")
                SSC.setState nextState

        forever do
            actionPayload :: LByteString <- receiveData

            let theAction = Aeson.eitherDecode @controller actionPayload

            case theAction of
                Right theAction -> do
                    currentState <- SSC.getState

                    -- Execute action with exception handling
                    actionResult <- Exception.try (SSC.action currentState theAction)
                    case actionResult of
                        Left (e :: SomeException) -> do
                            let errorText = tshow e
                            Log.error ("SSC: Action failed for " <> componentName <> ": " <> errorText)
                            SSC.sendError (SSCActionError { errorMessage = errorText })
                        Right nextState -> do
                            SSC.setState nextState
                Left parseError -> do
                    let errorText = cs parseError
                    Log.error ("SSC: Failed parsing action for " <> componentName <> ": " <> errorText)
                    Log.debug ("SSC: Invalid payload: " <> tshow actionPayload)
                    SSC.sendError (SSCParseError { errorMessage = errorText })
