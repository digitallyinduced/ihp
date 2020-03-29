module TurboHaskell.IDE.StatusServer (startStatusServer) where

import ClassyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import qualified System.Posix.Signals as Signals
import TurboHaskell.HaskellSupport
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import TurboHaskell.ViewPrelude
import qualified Data.ByteString.Char8 as ByteString
import TurboHaskell.IDE.Types


appPort :: Int
appPort = 8000


startStatusServer :: IO (IORef (Async ()), ByteString -> IO (), ByteString -> IO (), IO (), IO ())
startStatusServer = do
        standardOutput <- newIORef ""
        errorOutput <- newIORef ""
        websocketState <- newIORef []

        let warpApp = Websocket.websocketsOr
                Websocket.defaultConnectionOptions
                (app websocketState)
                (statusServerApp (standardOutput, errorOutput))

        let startServer' = async do
                (Warp.run appPort warpApp) `catch` (\(e :: SomeException) -> putStrLn (tshow e))
        
        serverRef <- startServer' >>= newIORef


        let applicationOnStandardOutput line = do
                modifyIORef standardOutput (\o -> o <> "\n" <> line)

                -- We don't want to slow down the compiling process
                -- therefore we notify our websockets in another thread
                _ <- async $ notifyOutput websocketState ("stdout" <> line)
                pure ()

        let stopStatusServer = do
                async do
                    server <- readIORef serverRef
                    cancel server
                    writeIORef standardOutput ""
                    writeIORef errorOutput ""
                    writeIORef websocketState []
                pure ()

        let startStatusServer = do
                putStrLn "START STATUS SERVER"
                async do
                    Concurrent.threadDelay 10000
                    readIORef serverRef >>= cancel
                    startServer' >>= writeIORef serverRef
                pure ()

        let applicationOnErrorOutput line = do
                modifyIORef errorOutput (\o -> o <> "\n" <> line)
                _ <- async $ notifyOutput websocketState ("stderr" <> line)
                pure ()

        pure (serverRef, applicationOnStandardOutput, applicationOnErrorOutput, startStatusServer, stopStatusServer)
    where
        statusServerApp :: (IORef ByteString, IORef ByteString) -> Wai.Application
        statusServerApp (standardOutput, errorOutput) req respond = do
            currentStandardOutput <- readIORef standardOutput
            currentErrorOutput <- readIORef errorOutput
            let responseBody = Blaze.renderHtmlBuilder (renderErrorView currentStandardOutput currentErrorOutput True)
            let responseHeaders = [(HTTP.hContentType, "text/html")]
            respond $ Wai.responseBuilder HTTP.status200 responseHeaders responseBody


renderErrorView :: ByteString -> ByteString -> Bool -> Html5.Html
renderErrorView standardOutput errorOutput isCompiling = [hsx|
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <title>Compilation Error</title>
                <style>{style}</style>
            </head>
            <body style="background-color: #002b36; color: #839496">
                <div class="m-2">{inner}</div>
                <div style="display: flex">
                    <pre style="font-family: Menlo, monospace; color: #268bd2; width: 50%" id="stderr">{errorOutput}</pre>
                    <pre style="font-family: Menlo, monospace; color: #586e75; width: 50%; text-align: right; font-size: 8px" id="stdout">{standardOutput}</pre>
                </div>
                <script>{websocketHandler}</script>
            </body>
        </html>
    |]
        where
            inner = if isCompiling
                then [hsx|<h1>Is compiling</h1>|]
                else [hsx|<h1>Error while compiling</h1>|]

            websocketHandler = preEscapedToHtml [plain|
                var socket = new WebSocket("ws://localhost:#{appPort}");
                socket.onclose = function () { window.location.reload(); }
                socket.onmessage = function (event) {
                    var c = (event.data.substr(0, 6) === 'stdout' ? stdout : stderr); c.innerText = c.innerText + "\\n" + event.data.substr(6);
                }
            |]

            style = preEscapedToHtml [plain|
                body {
                    font-size: 16px;
                    font-family: -apple-system, Roboto, "Helvetica Neue", Arial, sans-serif;
                }
            |]

notifyOutput :: IORef [Websocket.Connection] -> ByteString -> IO ()
notifyOutput stateRef output = do
    clients <- readIORef stateRef
    forM_ clients $ \connection -> ((Websocket.sendTextData connection output) `catch` (\(e :: SomeException) -> putStrLn $ tshow e))

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 30
    forever do
        -- (_ :: Text) <- Websocket.receiveData connection
        Websocket.sendTextData connection ("pong" :: Text)
        Concurrent.threadDelay (1000000)
        pure ()
