module TurboHaskell.IDE.LiveReloadNotificationServer (startLiveReloadNotificationServer, notifyHaskellChange, notifyAssetChange, stopLiveReloadNotification) where

import ClassyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import TurboHaskell.IDE.Types

startLiveReloadNotificationServer :: (?context :: Context) => IO ()
startLiveReloadNotificationServer = do
    clients <- newIORef []

    server <- async $ Warp.run 8002 $ Websocket.websocketsOr
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
    async do readIORef clients >>= (mapM_ $ \connection -> ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> pure ())))
    pure ()
broadcast message _ = pure ()

stopLiveReloadNotification :: LiveReloadNotificationServerState -> IO ()
stopLiveReloadNotification LiveReloadNotificationServerStarted { .. } = uninterruptibleCancel server
stopLiveReloadNotification _ = putStrLn "stopLiveReloadNotification: LiveReloadNotificationServer not running"

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 1
    forever do
        Websocket.sendTextData connection ("pong" :: Text)
        Concurrent.threadDelay (1000000)
        pure ()