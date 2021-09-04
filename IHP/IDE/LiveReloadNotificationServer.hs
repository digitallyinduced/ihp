module IHP.IDE.LiveReloadNotificationServer (app, notifyHaskellChange, notifyAssetChange) where

import IHP.Prelude
import qualified Network.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.IDE.Types
import IHP.HaskellSupport
import qualified Control.Exception as Exception
import qualified Data.UUID.V4 as UUID
import qualified Data.Map as Map

notifyHaskellChange :: (?context :: Context) => LiveReloadNotificationServerState -> IO ()
notifyHaskellChange = broadcast "reload"

notifyAssetChange :: (?context :: Context) => LiveReloadNotificationServerState -> IO ()
notifyAssetChange = broadcast "reload_assets"

broadcast :: (?context :: Context) => ByteString -> LiveReloadNotificationServerState -> IO ()
broadcast message LiveReloadNotificationServerState { clients } = do
    clients' <- readIORef clients
    
    let removeClient connectionId = modifyIORef clients (Map.delete connectionId)
    let sendMessage (id, connection) = ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> removeClient id))
    let connections = clients' |> Map.toList

    forConcurrently connections sendMessage
    pure ()

app :: LiveReloadNotificationServerState -> Websocket.ServerApp
app LiveReloadNotificationServerState { clients } pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    connectionId <- UUID.nextRandom

    modifyIORef clients (Map.insert connectionId connection)

    let removeClient = modifyIORef clients (Map.delete connectionId)

    let withPingThread = Websocket.withPingThread connection 30 (pure ())

    let keepalive = forever (Concurrent.threadDelay maxBound)
    withPingThread keepalive `Exception.finally` removeClient