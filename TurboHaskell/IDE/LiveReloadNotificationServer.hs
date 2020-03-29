module TurboHaskell.IDE.LiveReloadNotificationServer where

import ClassyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent

startLiveReloadNotificationServer :: IO (Async (), IO (), IO ())
startLiveReloadNotificationServer = do
    state <- newIORef []

    reloadTask <- newIORef Nothing
    let handleAssetChange = notifyAssetChange state
    let handleHaskellChange = do
            readIORef reloadTask >>= maybe (pure ()) uninterruptibleCancel
            (async (notifyReload state)) >>= pure . Just >>= writeIORef reloadTask
    server <- async $ Warp.run 8002 $ Websocket.websocketsOr
        Websocket.defaultConnectionOptions
        (app state)
        httpApp
    pure (server, handleAssetChange, handleHaskellChange)

httpApp :: Wai.Application
httpApp request respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

notifyReload = broadcast "reload"
notifyAssetChange = broadcast "reload_assets"

broadcast :: ByteString -> IORef [Websocket.Connection] -> IO ()
broadcast message stateRef = do
    clients <- readIORef stateRef
    forM_ clients $ \connection -> ((Websocket.sendTextData connection message) `catch` (\(e :: SomeException) -> pure ()))

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 30
    forever do
        Concurrent.threadDelay (30 * 1000000)
        pure ()