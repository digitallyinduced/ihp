module IHP.IDE.LiveReloadNotificationServer (startLiveReloadNotificationServer, notifyHaskellChange, notifyAssetChange, stopLiveReloadNotification) where

import ClassyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.IDE.Types
import IHP.HaskellSupport
import IHP.IDE.PortConfig

startLiveReloadNotificationServer :: (?context :: Context) => IO ()
startLiveReloadNotificationServer = do
    clients <- newIORef []

    let port = ?context
            |> get #portConfig
            |> get #liveReloadNotificationPort
            |> fromIntegral
    
    server <- async $ Warp.run port $ Websocket.websocketsOr
        Websocket.defaultConnectionOptions
        (app clients)
        httpApp

    dispatch (UpdateLiveReloadNotificationServerState (LiveReloadNotificationServerStarted { server, clients }))

httpApp :: Wai.Application
httpApp request respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

notifyHaskellChange :: LiveReloadNotificationServerState -> IO ()
notifyHaskellChange = broadcast "reload"
notifyAssetChange = broadcast "reload_assets"

broadcast :: ByteString -> LiveReloadNotificationServerState -> IO ()
broadcast message LiveReloadNotificationServerStarted { server, clients } = do
    async do
        clients' <- readIORef clients
        let sendMessage connection = ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> putStrLn (tshow e)))
        mapM_ sendMessage clients'
    pure ()
broadcast message _ = putStrLn "LiveReloadNotificationServer: broadcast failed as not running"

stopLiveReloadNotification :: LiveReloadNotificationServerState -> IO ()
stopLiveReloadNotification LiveReloadNotificationServerStarted { .. } = uninterruptibleCancel server
stopLiveReloadNotification _ = pure ()

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 1
    forever do
        Websocket.sendTextData connection ("pong" :: Text)
        Concurrent.threadDelay (1000000)
        pure ()