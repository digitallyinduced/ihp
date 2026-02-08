{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.ServerSideComponent.ControllerFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.ControllerFunctions where

import IHP.Prelude
import IHP.ControllerPrelude
import IHP.ServerSideComponent.Types as SSC

import qualified Network.WebSockets as WebSocket
import Network.Wai (Request)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import IHP.ServerSideComponent.HtmlParser
import IHP.ServerSideComponent.HtmlDiff

import qualified IHP.Log as Log

$(Aeson.deriveJSON Aeson.defaultOptions { sumEncoding = defaultTaggedObject { tagFieldName = "type" }} ''Attribute)
$(Aeson.deriveJSON Aeson.defaultOptions { sumEncoding = defaultTaggedObject { tagFieldName = "type" }} ''AttributeOperation)
$(Aeson.deriveJSON Aeson.defaultOptions { sumEncoding = defaultTaggedObject { tagFieldName = "type" }} ''Node)
$(Aeson.deriveJSON Aeson.defaultOptions { sumEncoding = defaultTaggedObject { tagFieldName = "type" }} ''NodeOperation)
$(Aeson.deriveJSON Aeson.defaultOptions { sumEncoding = defaultTaggedObject { tagFieldName = "type" }} ''SSCError)

setState :: (?instanceRef :: IORef (ComponentInstance state), ?connection :: WebSocket.Connection, Component state action, ?context :: ControllerContext, ?request :: Request) => state -> IO ()
setState state = do
    oldState <- (.state) <$> readIORef ?instanceRef
    let oldHtml = oldState
            |> SSC.render
            |> Blaze.renderHtml
            |> cs
    let newHtml = state
            |> SSC.render
            |> Blaze.renderHtml
            |> cs

    modifyIORef' ?instanceRef (\componentInstance -> componentInstance { state })

    case diffHtml oldHtml newHtml of
        Left parseError -> do
            let errorText = tshow parseError
            Log.error ("SSC HTML diff failed: " <> errorText)
            sendError (SSCDiffError { errorMessage = errorText })
        Right patches -> sendTextData (Aeson.encode patches)

-- | Send an error message to the client
sendError :: (?connection :: WebSocket.Connection) => SSCError -> IO ()
sendError error = sendTextData (Aeson.encode error)


getState :: (?instanceRef :: IORef (ComponentInstance state)) => IO state
getState = (.state) <$> readIORef ?instanceRef

deriveSSC = Aeson.deriveJSON Aeson.defaultOptions { allNullaryToStringTag = False, sumEncoding = defaultTaggedObject { tagFieldName = "action", contentsFieldName = "payload" }}
