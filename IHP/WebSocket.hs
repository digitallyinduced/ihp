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
import IHP.ApplicationContext
import IHP.Controller.RequestContext
import qualified Data.UUID as UUID
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exception
import IHP.Controller.Context
import qualified Data.Aeson as Aeson

import qualified IHP.Log as Log

import qualified Network.WebSockets.Connection as WebSocket

class WSApp state where
    initialState :: state

    run :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext, ?connection :: Websocket.Connection) => IO ()
    run = pure ()

    onPing :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext, ?connection :: Websocket.Connection) => IO ()
    onPing = pure ()

    onClose :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext, ?connection :: Websocket.Connection) => IO ()
    onClose = pure ()

startWSApp :: forall state. (WSApp state, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, ?context :: ControllerContext, ?modelContext :: ModelContext) => Websocket.Connection -> IO ()
startWSApp connection' = do
    state <- newIORef (initialState @state)
    lastPongAt <- getCurrentTime >>= newIORef


    let connection = installPongHandler lastPongAt connection'
    let ?state = state
    let ?connection = connection
    let pingHandler = do
            seconds <- secondsSinceLastPong lastPongAt
            when (seconds > pingWaitTime * 2) (throwIO PongTimeout)
            onPing @state

    result <- Exception.try ((WebSocket.withPingThread connection pingWaitTime pingHandler (run @state)) `Exception.finally` onClose @state)
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

data PongTimeout
    = PongTimeout
    deriving (Show)

instance Exception PongTimeout

pingWaitTime :: Int
pingWaitTime = 30

installPongHandler :: IORef UTCTime -> WebSocket.Connection -> WebSocket.Connection
installPongHandler lastPongAt connection =
    connection { WebSocket.connectionOptions = connection.connectionOptions { WebSocket.connectionOnPong = connectionOnPong lastPongAt }  }

connectionOnPong :: IORef UTCTime -> IO ()
connectionOnPong lastPongAt = do
    now <- getCurrentTime
    writeIORef lastPongAt now

secondsSinceLastPong :: IORef UTCTime -> IO Int
secondsSinceLastPong lastPongAt = do
    now <- getCurrentTime
    last <- readIORef lastPongAt
    pure $ ceiling $ nominalDiffTimeToSeconds $ diffUTCTime now last
