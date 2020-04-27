module TurboHaskell.IDE.StatusServer (startStatusServer, stopStatusServer, notifyBrowserOnApplicationOutput, continueStatusServer) where

import TurboHaskell.ViewPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import TurboHaskell.HaskellSupport
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString
import TurboHaskell.IDE.Types
import TurboHaskell.IDE.PortConfig
import ClassyPrelude (async, uninterruptibleCancel, catch, forever)

startStatusServer :: (?context :: Context) => IO ()
startStatusServer = do
        standardOutput <- newIORef ""
        errorOutput <- newIORef ""
        clients <- newIORef []
        serverRef <- async (pure ()) >>= newIORef

        continueStatusServer StatusServerPaused { .. }

        dispatch (UpdateStatusServerState (StatusServerStarted { serverRef, clients, standardOutput, errorOutput }))

continueStatusServer :: (?context :: Context) => StatusServerState -> IO ()
continueStatusServer StatusServerPaused { .. } = do
    
        let warpApp = Websocket.websocketsOr
                Websocket.defaultConnectionOptions
                (app clients)
                (statusServerApp (standardOutput, errorOutput))

        let port = ?context
                |> get #portConfig
                |> get #appPort
                |> fromIntegral

        server <- async $ Warp.run 8003 warpApp
        
        writeIORef serverRef server
    where
        statusServerApp :: (IORef ByteString, IORef ByteString) -> Wai.Application
        statusServerApp (standardOutput, errorOutput) req respond = do
            currentStandardOutput <- readIORef standardOutput
            currentErrorOutput <- readIORef errorOutput
            let responseBody = Blaze.renderHtmlBuilder (renderErrorView currentStandardOutput currentErrorOutput True)
            let responseHeaders = [(HTTP.hContentType, "text/html")]
            respond $ Wai.responseBuilder HTTP.status200 responseHeaders responseBody

stopStatusServer :: StatusServerState -> IO ()
stopStatusServer StatusServerStarted { serverRef } = do 
    async $ readIORef serverRef >>= uninterruptibleCancel
    pure ()
stopStatusServer _ = putStrLn "StatusServer: Cannot stop as not running"

notifyBrowserOnApplicationOutput :: StatusServerState -> OutputLine -> IO ()
notifyBrowserOnApplicationOutput StatusServerStarted { serverRef, clients, standardOutput, errorOutput } line = do
    let shouldIgnoreLine = (line == ErrorOutput "Warning: -debug, -threaded and -ticky are ignored by GHCi")
    unless shouldIgnoreLine do
        case line of
            StandardOutput line -> modifyIORef standardOutput (\o -> o <> "\n" <> line)
            ErrorOutput line -> modifyIORef errorOutput (\o -> o <> "\n" <> line)
        let payload = case line of
                StandardOutput line -> "stdout" <> line
                ErrorOutput line -> "stderr" <> line
        async (notifyOutput clients payload)
        pure ()
    
notifyBrowserOnApplicationOutput _ _ = putStrLn "StatusServer: Cannot notify clients as not in running state"

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
                var socket = new WebSocket("ws://localhost:" + window.location.port);
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
    forM_ clients $ \connection -> ((Websocket.sendTextData connection output) `catch` (\(e :: SomeException) -> pure ()))

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 1
    forever do
        Websocket.sendTextData connection ("pong" :: Text)
        Concurrent.threadDelay (1000000)
        pure ()
