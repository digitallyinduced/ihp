{-# LANGUAGE  UndecidableInstances #-}
module IHP.ServerSideComponent.Controller.ComponentsController where

import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ControllerFunctions as SSC

import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html.Renderer.Text as Blaze

instance (Component component controller, FromJSON controller) => WSApp (ComponentsController component) where
    initialState = ComponentsController

    run = do
        let state :: component = SSC.initialState
        let initialHtml = state
                |> SSC.render
                |> Blaze.renderHtml
                |> cs
        instanceRef <- newIORef (ComponentInstance { state, renderedHtml = initialHtml })
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
                Left error -> putStrLn (cs error)