module IHP.IDE.LiveReloadNotificationServer (app, notifyHaskellChange, notifyAssetChange) where

import IHP.Prelude
import qualified Network.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.IDE.Types
import qualified Control.Exception as Exception
import qualified Data.UUID.V4 as UUID
import qualified Data.Map as Map

notifyHaskellChange :: (?context :: Context) => IO ()
notifyHaskellChange = broadcast "reload"

notifyAssetChange :: (?context :: Context) => IO ()
notifyAssetChange = broadcast "reload_assets"

broadcast :: (?context :: Context) => ByteString -> IO ()
broadcast message = do
    let clients = ?context.liveReloadClients
    clients' <- readIORef clients
    
    let removeClient connectionId = modifyIORef clients (Map.delete connectionId)
    let sendMessage (id, connection) = ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> removeClient id))
    let connections = clients' |> Map.toList

    forConcurrently connections sendMessage
    pure ()

app :: (?context :: Context) => Websocket.ServerApp
app pendingConnection = do
    let clients = ?context.liveReloadClients
    
    connection <- Websocket.acceptRequest pendingConnection
    connectionId <- UUID.nextRandom

    modifyIORef clients (Map.insert connectionId connection)

    let removeClient = modifyIORef clients (Map.delete connectionId)

    let withPingThread = Websocket.withPingThread connection 30 (pure ())

    let keepalive = forever (Concurrent.threadDelay maxBound)
    withPingThread keepalive `Exception.finally` removeClient