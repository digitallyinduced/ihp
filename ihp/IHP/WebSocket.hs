{-|
Module: IHP.WebSocket
Description: Building blocks for websocket applications
Copyright: (c) digitally induced GmbH, 2020
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.WebSocket
( WSApp (..)
, startWSApp
, setState
, getState
, receiveData
, receiveDataMessage
, sendTextData
, sendJSON
)
where

import IHP.Prelude
import qualified Network.WebSockets as Websocket
import Network.WebSockets.Connection.PingPong (withPingPong, defaultPingPongOptions)
import IHP.Controller.RequestContext
import qualified Data.UUID as UUID
import qualified Data.Maybe as Maybe
import qualified Control.Exception.Safe as Exception
import IHP.Controller.Context
import qualified Data.Aeson as Aeson

import qualified IHP.Log as Log

import qualified Network.WebSockets.Connection as WebSocket

class WSApp state where
    initialState :: state

    run :: (?state :: IORef state, ?context :: ControllerContext, ?modelContext :: ModelContext, ?connection :: Websocket.Connection) => IO ()
    run = pure ()

    onPing :: (?state :: IORef state, ?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
    onPing = pure ()

    onClose :: (?state :: IORef state, ?context :: ControllerContext, ?modelContext :: ModelContext, ?connection :: Websocket.Connection) => IO ()
    onClose = pure ()

    -- | Provide WebSocket Connection Options
    --
    -- See All Config Options Here
    -- https://hackage.haskell.org/package/websockets/docs/Network-WebSockets-Connection.html#t:ConnectionOptions
    --
    -- __Example:__
    -- Enable default permessage-deflate compression
    --
    -- > connectionOptions =
    -- >     WebSocket.defaultConnectionOptions {
    -- >         WebSocket.connectionCompressionOptions =
    -- >             WebSocket.PermessageDeflateCompression WebSocket.defaultPermessageDeflate
    -- >     }
    --
    connectionOptions :: WebSocket.ConnectionOptions
    connectionOptions = WebSocket.defaultConnectionOptions

startWSApp :: forall state. (WSApp state, ?requestContext :: RequestContext, ?context :: ControllerContext, ?modelContext :: ModelContext) => state -> Websocket.Connection -> IO ()
startWSApp initialState connection = do
    state <- newIORef initialState
    let ?state = state

    result <- Exception.try ((withPingPong (defaultPingPongOptions { Websocket.pingAction = onPing @state }) connection (\connection -> let ?connection = connection in run @state)) `Exception.finally` (let ?connection = connection in onClose @state))
    case result of
        Left (e@Exception.SomeException{}) ->
            case Exception.fromException e of
                (Just Websocket.ConnectionClosed) -> pure ()
                (Just (Websocket.CloseRequest {})) -> pure ()
                (Just other) -> error ("Unhandled Websocket exception: " <> show other)
                Nothing -> Log.error (tshow e)
        Right _ -> pure ()

setState :: (?state :: IORef state) => state -> IO ()
setState newState = writeIORef ?state newState

getState :: (?state :: IORef state) => IO state
getState = readIORef ?state

receiveData :: (?connection :: Websocket.Connection, Websocket.WebSocketsData a) => IO a
receiveData = Websocket.receiveData ?connection

receiveDataMessage :: (?connection :: Websocket.Connection) => IO Websocket.DataMessage
receiveDataMessage = Websocket.receiveDataMessage ?connection

sendTextData :: (?connection :: Websocket.Connection, Websocket.WebSocketsData text) => text -> IO ()
sendTextData text = Websocket.sendTextData ?connection text

-- | Json encode a payload and send it over the websocket wire
--
-- __Example:__
--
-- > message <- Aeson.decode <$> receiveData @LByteString
-- >
-- > case message of
-- >     Just decodedMessage -> handleMessage decodedMessage
-- >     Nothing -> sendJSON FailedToDecodeMessageError
--
sendJSON :: (?connection :: Websocket.Connection, Aeson.ToJSON value) => value -> IO ()
sendJSON payload = sendTextData (Aeson.encode payload)

instance Websocket.WebSocketsData UUID where
    fromDataMessage (Websocket.Text byteString _) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromDataMessage (Websocket.Binary byteString) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromLazyByteString byteString = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    toLazyByteString = UUID.toLazyASCIIBytes

