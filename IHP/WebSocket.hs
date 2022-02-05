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

import qualified IHP.Log.Types as Log
import qualified IHP.Log as Log

import Control.Concurrent.Chan
import Control.Concurrent
import System.Timeout
import Data.Function (fix)
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
startWSApp connection = do
    state <- newIORef (initialState @state)
    let ?state = state
    let ?connection = connection

    let runWithPongChan pongChan = do
        let connectionOnPong = writeChan pongChan ()
        let ?connection = connection
                { WebSocket.connectionOptions = (get #connectionOptions connection) { WebSocket.connectionOnPong } 
                }
            in
                run @state

    result <- Exception.try ((withPinger connection runWithPongChan) `Exception.finally` onClose @state)
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


-- | Pings the client every 30 seconds and expects a pong response within 10 secons. If no pong response
-- is received within 10 seconds, it will kill the connection.
--
-- We cannot use the withPingThread of the websockets package as this doesn't deal with pong messages. So
-- open connection will stay around forever.
--
-- This implementation is based on https://github.com/jaspervdj/websockets/issues/159#issuecomment-552776502
withPinger conn action = do
    pongChan <- newChan
    mainAsync <- async $ action pongChan
    pingerAsync <- async $ runPinger conn pongChan

    waitEitherCatch mainAsync pingerAsync >>= \case
        -- If the application async died for any reason, kill the pinger async
        Left result -> do
            cancel pingerAsync
            case result of
                Left exception -> throw exception
                Right result -> pure ()
        -- The pinger thread should never throw an exception. If it does, kill the app thread
        Right (Left exception) -> do
            cancel mainAsync
            throw exception
        -- The pinger thread exited due to a pong timeout. Tell the app thread about it.
        Right (Right ()) -> cancelWith mainAsync PongTimeout

runPinger conn pongChan = fix $ \loop -> do
    Websocket.sendPing conn (mempty :: ByteString)
    threadDelay pingWaitTime
    -- See if we got a pong in that time
    timeout 1000000 (readChan pongChan) >>= \case
        Just () -> loop
        Nothing -> return ()