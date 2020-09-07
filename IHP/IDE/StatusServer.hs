module IHP.IDE.StatusServer (startStatusServer, stopStatusServer, clearStatusServer, notifyBrowserOnApplicationOutput, continueStatusServer) where

import IHP.ViewPrelude hiding (catch)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.HaskellSupport
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.Types
import IHP.IDE.PortConfig
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes
import ClassyPrelude (async, uninterruptibleCancel, catch, forever)
import qualified Network.URI as URI

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

        server <- async $ Warp.run port warpApp

        writeIORef serverRef server
    where
        statusServerApp :: (IORef ByteString, IORef ByteString) -> Wai.Application
        statusServerApp (standardOutput, errorOutput) req respond = do
            isCompiling <- getCompilingStatus
            currentStandardOutput <- readIORef standardOutput
            currentErrorOutput <- readIORef errorOutput
            let responseBody = Blaze.renderHtmlBuilder (renderErrorView currentStandardOutput currentErrorOutput isCompiling)
            let responseHeaders = [(HTTP.hContentType, "text/html")]
            respond $ Wai.responseBuilder HTTP.status200 responseHeaders responseBody

stopStatusServer :: StatusServerState -> IO ()
stopStatusServer StatusServerStarted { serverRef } = do
    async $ readIORef serverRef >>= uninterruptibleCancel
    pure ()
stopStatusServer _ = putStrLn "StatusServer: Cannot stop as not running"

clearStatusServer :: (?context :: Context) => StatusServerState -> IO ()
clearStatusServer StatusServerStarted { .. } = do
    writeIORef standardOutput ""
    writeIORef errorOutput ""
    async (notifyOutput (standardOutput, errorOutput) clients)
    pure ()
clearStatusServer StatusServerPaused { .. } = do
    writeIORef standardOutput ""
    writeIORef errorOutput ""
clearStatusServer StatusServerNotStarted = pure ()

notifyBrowserOnApplicationOutput :: (?context :: Context) => StatusServerState -> OutputLine -> IO ()
notifyBrowserOnApplicationOutput StatusServerStarted { serverRef, clients, standardOutput, errorOutput } line = do
    let shouldIgnoreLine = (line == ErrorOutput "Warning: -debug, -threaded and -ticky are ignored by GHCi")
    unless shouldIgnoreLine do
        case line of
            StandardOutput line -> modifyIORef standardOutput (\o -> o <> "\n" <> line)
            ErrorOutput line -> modifyIORef errorOutput (\o -> o <> "\n" <> line)
        let payload = case line of
                StandardOutput line -> "stdout" <> line
                ErrorOutput line -> "stderr" <> line
        async (notifyOutput (standardOutput, errorOutput) clients)
        pure ()
notifyBrowserOnApplicationOutput StatusServerPaused { serverRef, clients, standardOutput, errorOutput } line = do
    case line of
        StandardOutput line -> modifyIORef standardOutput (\o -> o <> "\n" <> line)
        ErrorOutput line -> modifyIORef errorOutput (\o -> o <> "\n" <> line)
    pure ()
notifyBrowserOnApplicationOutput _ _ = putStrLn "StatusServer: Cannot notify clients as not in running state"


data CompilerError = CompilerError { errorMessage :: [ByteString], isWarning :: Bool } deriving (Show)

renderErrorView :: (?context :: Context) => ByteString -> ByteString -> Bool -> Html5.Html
renderErrorView standardOutput errorOutput isCompiling = [hsx|
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                {title}
                <style>
                    * { -webkit-font-smoothing: antialiased }
                    body {
                        font-size: 16px;
                        font-family: -apple-system, Roboto, "Helvetica Neue", Arial, sans-serif;
                        background-color: hsl(196 13% 30% / 1);
                        color: hsla(196, 13%, 96%, 1);
                    }
                    .compiler-error .file-name {
                        margin-bottom: 1rem;
                        font-weight: bold;
                        display: block;
                        color: inherit !important;
                        text-decoration: none;
                    }

                    .compiler-error {
                        margin-bottom: 3rem;
                        font-size: 0.7rem;
                    }

                    .ihp-error-other-solutions {
                        margin-top: 2rem;
                        padding-top: 0.5rem;
                        font-size: 0.8rem;
                        color: hsla(196, 13%, 80%, 1);
                        border-top: 1px solid hsla(196, 13%, 60%, 0.4);
                        margin-bottom: 4rem;
                    }

                    .ihp-error-other-solutions a {
                        color: hsla(196, 13%, 80%, 0.9);
                        text-decoration: none !important;
                        margin-right: 2rem;
                        font-size: 0.8rem;
                    }
                    .ihp-error-other-solutions a:hover {
                        color: hsla(196, 13%, 80%, 1);
                    }

                    .troubleshooting-suggestion {
                        margin-top: 1rem;
                    }

                    .troubleshooting-suggestion, .troubleshooting-suggestion a {
                        font-weight: bold;
                        color: #859900 !important;
                    }

                    #stderr .compiler-error:first-child { opacity: 1; font-size: 1rem; }
                    #stderr .compiler-error:first-child .file-name { font-size: 1.5rem; }
                    #stderr .compiler-error { opacity: 0.5; }

                    #ihp-error-container {
                        max-width: 800px;
                        margin-left: auto;
                        margin-right: auto;
                    }
                </style>
            </head>
            <body>
                {errorContainer}

                <script>
                    var socket = new WebSocket("ws://localhost:" + window.location.port);
                    var parser = new DOMParser();
                    socket.onclose = function () { setTimeout(() => window.location.reload(), 500); }
                    socket.onmessage = function (event) {
                        if (event.data !== 'pong') {
                            var responseBody = parser.parseFromString(event.data, 'text/html');
                            document.getElementById('ihp-error-container').outerHTML = responseBody.getElementById('ihp-error-container').outerHTML;
                        }
                    }
                </script>
            </body>
        </html>
    |]
        where
            errorContainer = [hsx|
                <div id="ihp-error-container">
                    <h1 style="margin-bottom: 2rem; margin-top: 20%; font-size: 1rem; font-weight: 400; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1); color: hsla(196, 13%, 80%, 1)">{inner}</h1>
                    <pre style="font-family: Menlo, monospace; width: 100%" id="stderr">{forEach (parseErrorOutput errorOutput) renderError}</pre>

                    <div class="ihp-error-other-solutions">
                        <a href="https://gitter.im/digitallyinduced/ihp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge" target="_blank">Ask the IHP Community on Gitter</a>
                        <a href="https://github.com/digitallyinduced/ihp/wiki/Troubleshooting" target="_blank">Check the Troubleshooting</a>
                        <a href={("https://github.com/digitallyinduced/ihp/issues/new?body=" :: Text) <> cs (URI.escapeURIString URI.isUnescapedInURI (cs errorOutput))} target="_blank">Open a GitHub Issue</a>
                    </div>

                    <pre style="font-family: Menlo, monospace; font-size: 10px" id="stdout">{standardOutput}</pre>
                </div>
            |]
            parseErrorOutput output =
                    splitToSections (ByteString.lines output) []
                    |> map identifySection
                where
                    splitToSections :: [ByteString] -> [[ByteString]] -> [[ByteString]]
                    splitToSections [] result = result
                    splitToSections ("":lines) result = splitToSections lines result
                    splitToSections lines result =
                        let (error :: [ByteString], rest) = span (\line -> line /= "") lines
                        in splitToSections rest ((error |> filter (/= "")):result)

                    identifySection :: [ByteString] -> CompilerError
                    identifySection lines | "warning" `ByteString.isInfixOf` (fromMaybe "" (headMay lines)) = CompilerError { errorMessage = lines, isWarning = True }
                    identifySection lines = CompilerError { errorMessage = lines, isWarning = False }

            title = if isCompiling
                then [hsx|<title>Compiling...</title>|]
                else [hsx|<title>Compilation Error</title>|]

            inner = if isCompiling
                then [hsx|Is compiling|]
                else [hsx|Problems found while compiling|]

            renderError CompilerError { errorMessage, isWarning } = [hsx|
                    <div class="compiler-error">
                        {forEachWithIndex errorMessage renderLine}
                        {mconcat (renderTroubleshooting errorMessage)}
                    </div>
                |]

            renderLine (0, line) = [hsx|
                    <a class="file-name" href={openEditor} target={line}>{filePath}</a>
                    <iframe name={line} src="about:blank" style="display: none"/>
                |]
                where
                    (filePath, rest) = ByteString.breakSubstring ": " line
                    openEditor = "http://localhost:" <> tshow toolServerPort <> (pathTo OpenEditorAction) <> "?path=" <> cs plainFilePath <> "&line=" <> cs fileLine <> "&col=" <> cs fileCol
                    (plainFilePath, fileLine, fileCol) = case ByteString.split ':' filePath of
                            [path, line, col] -> (path, line, col)
                            [path, line] -> (path, line, "0")
                            otherwise -> (filePath, "0", "0")
            renderLine (i, line) = [hsx|<div>{line}</div>|]

            renderTroubleshooting :: [ByteString] -> [Html5.Html]
            renderTroubleshooting lines = [ modelContextTroubleshooting ]
                    |> map (\f -> f lines)
                    |> catMaybes

            toolServerPort = ?context
                |> get #portConfig
                |> get #toolServerPort

notifyOutput :: (?context :: Context) => (IORef ByteString, IORef ByteString) -> IORef [Websocket.Connection] -> IO ()
notifyOutput (standardOutputRef, errorOutputRef) stateRef = do
    clients <- readIORef stateRef
    let ignoreException (e :: SomeException) = pure ()

    isCompiling <- getCompilingStatus
    standardOutput <- readIORef standardOutputRef
    errorOutput <- readIORef errorOutputRef

    forM_ clients $ \connection -> do
        let errorContainer = renderErrorView standardOutput errorOutput isCompiling
        let html = Blaze.renderHtml errorContainer
        (Websocket.sendTextData connection html) `ClassyPrelude.catch` ignoreException

app :: IORef [Websocket.Connection] -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    modifyIORef stateRef $ \state -> (connection : state)
    Websocket.forkPingThread connection 1
    forever do
        Websocket.sendTextData connection ("pong" :: Text)
        Concurrent.threadDelay (1000000)
        pure ()


modelContextTroubleshooting :: [ByteString] -> Maybe Html5.Html
modelContextTroubleshooting lines =
    lines
    |> map (\line -> "Unbound implicit parameter (?modelContext::" `ByteString.isInfixOf` line)
    |> or
    |> \case
        True -> Just [hsx|
            <div class="troubleshooting-suggestion">
                A detailed explanation for this error is available in the IHP Wiki:
                <a href="https://github.com/digitallyinduced/ihp/wiki/Troubleshooting#unbound-implicit-parameter-modelcontextmodelcontext" target="_blank">See Error Explanation</a>
            </div>
        |]
        False -> Nothing


getCompilingStatus :: (?context :: Context) => IO Bool
getCompilingStatus = do
    devServerState <- ?context
        |> get #appStateRef
        |> readIORef

    pure case (get #appGHCIState devServerState) of
            AppGHCILoading { } -> True
            _ -> False
