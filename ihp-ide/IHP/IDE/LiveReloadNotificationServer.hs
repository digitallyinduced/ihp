module IHP.IDE.LiveReloadNotificationServer (app, notifyHaskellChange, notifyAssetChange, State) where

import IHP.Prelude
import qualified Network.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.IDE.Types
import qualified Control.Exception as Exception
import qualified Data.UUID.V4 as UUID
import qualified Data.Map as Map

type State = IORef (Map UUID Websocket.Connection)

notifyHaskellChange :: State -> IO ()
notifyHaskellChange clients = broadcast "reload" clients

notifyAssetChange :: State -> IO ()
notifyAssetChange clients = broadcast "reload_assets" clients

broadcast :: ByteString -> State -> IO ()
broadcast message clients = do
    clients' <- readIORef clients
    
    let removeClient connectionId = modifyIORef clients (Map.delete connectionId)
    let sendMessage (id, connection) = ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> removeClient id))
    let connections = clients' |> Map.toList

    forConcurrently connections sendMessage
    pure ()

app :: State -> Websocket.ServerApp
app clients pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    connectionId <- UUID.nextRandom

    modifyIORef clients (Map.insert connectionId connection)

    let removeClient = modifyIORef clients (Map.delete connectionId)

    let withPingThread = Websocket.withPingThread connection 30 (pure ())

    let keepalive = forever (Concurrent.threadDelay maxBound)
    withPingThread keepalive `Exception.finally` removeClient