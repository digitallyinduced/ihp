module IHP.IDE.StatusServer (withStatusServer, consumeGhciOutput, clearStatusServer, Clients) where

import IHP.ViewPrelude hiding (catch)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.Types
import IHP.IDE.PortConfig
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes ()
import qualified Network.URI as URI
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Chan.Unagi as Queue
import Control.Concurrent.MVar

type Clients = IORef [(Websocket.Connection, Concurrent.MVar ())]

withStatusServer :: (?context :: Context) => IORef Bool -> (MVar () -> MVar (MVar ()) -> IORef [ByteString] -> IORef [ByteString] -> Clients -> IO a) -> IO a
withStatusServer ghciIsLoadingVar callback = do
    standardOutput <- newIORef []
    errorOutput <- newIORef []
    clients <- newIORef []

    startMVar :: MVar () <- newMVar ()
    stopMVar :: MVar (MVar ()) <- newEmptyMVar

    (a, _) <- concurrently (callback startMVar stopMVar standardOutput errorOutput clients) (runStatusServer ghciIsLoadingVar standardOutput errorOutput clients startMVar stopMVar)
    pure a

runStatusServer ghciIsLoadingVar standardOutput errorOutput clients startMVar stopMVar = do

    let port = ?context.portConfig.appPort |> fromIntegral

    forever do
        _ <- takeMVar startMVar

        race_
            (Warp.run port (waiApp ghciIsLoadingVar clients standardOutput errorOutput))
            (readMVar stopMVar)

        isStoppedVar <- takeMVar stopMVar
        putMVar isStoppedVar ()



waiApp :: (?context :: Context) => IORef Bool -> IORef [(Websocket.Connection, Concurrent.MVar ())] -> IORef [ByteString] -> IORef [ByteString] -> Wai.Application
waiApp ghciIsLoadingVar clients standardOutput errorOutput = do
    Websocket.websocketsOr
        Websocket.defaultConnectionOptions
        (wsApp ghciIsLoadingVar clients standardOutput errorOutput)
        (httpApp ghciIsLoadingVar standardOutput errorOutput)

httpApp :: (?context :: Context) => IORef Bool -> IORef [ByteString] -> IORef [ByteString] -> Wai.Application
httpApp ghciIsLoadingVar standardOutput errorOutput req respond = do
    isCompiling <- readIORef ghciIsLoadingVar
    currentStandardOutput <- readIORef standardOutput
    currentErrorOutput <- readIORef errorOutput
    lastSchemaCompilerError <- readIORef ?context.lastSchemaCompilerError
    let responseBody = Blaze.renderHtmlBuilder (renderErrorView currentStandardOutput currentErrorOutput isCompiling lastSchemaCompilerError)
    let responseHeaders = [(HTTP.hContentType, "text/html")]
    respond $ Wai.responseBuilder HTTP.status200 responseHeaders responseBody

clearStatusServer :: _ => IORef [ByteString] -> IORef [ByteString] -> Clients -> IO ()
clearStatusServer standardOutput errorOutput clients = do
    writeIORef standardOutput []
    writeIORef errorOutput []


    async (notifyOutput clients)
    pure ()

consumeGhciOutput :: (?context :: Context) => IORef [ByteString] -> IORef [ByteString] -> Clients -> IO ()
consumeGhciOutput statusServerStandardOutput statusServerErrorOutput statusServerClients = forever do
    line <- Queue.readChan ?context.ghciOutChan

    let shouldIgnoreLine = (line == ErrorOutput "Warning: -debug, -threaded and -ticky are ignored by GHCi")
    unless shouldIgnoreLine do
        let writeOutputLine standardOutput errorOutput = do
                case line of
                    StandardOutput line -> modifyIORef' standardOutput (line:)
                    ErrorOutput line -> modifyIORef' errorOutput (line:)

        writeOutputLine statusServerStandardOutput statusServerErrorOutput
        notifyOutput statusServerClients


notifyOutput :: IORef [(Websocket.Connection, Concurrent.MVar ())] -> IO ()
notifyOutput stateRef = do
    clients <- readIORef stateRef

    forM_ clients \(connection, didChangeMVar) -> do
        _ <- Concurrent.tryPutMVar didChangeMVar ()
        pure ()


data CompilerError = CompilerError { errorMessage :: [ByteString], isWarning :: Bool } deriving (Show)

renderErrorView :: (?context :: Context) => [ByteString] -> [ByteString] -> Bool -> Maybe SomeException -> Html5.Html
renderErrorView standardOutput errorOutput' isCompiling lastSchemaCompilerError = [hsx|
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
            errorOutput =
                case lastSchemaCompilerError of
                    Just lastSchemaCompilerError -> [cs (displayException lastSchemaCompilerError)]
                    Nothing -> errorOutput'
            errorContainer = [hsx|
                <div id="ihp-error-container">
                    <h1 style="margin-bottom: 2rem; margin-top: 20%; font-size: 1rem; font-weight: 400; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1); color: hsla(196, 13%, 80%, 1)">{inner}</h1>
                    <pre style="font-family: Menlo, monospace; width: 100%" id="stderr">{forEach errors renderError}</pre>

                    <div class="ihp-error-other-solutions">
                        <a href="https://ihp.digitallyinduced.com/Slack" target="_blank">Ask on Slack</a>
                        <a href="https://stackoverflow.com/questions/tagged/ihp" target="_blank">Ask on Stack Overflow</a>
                        <a href="https://github.com/digitallyinduced/ihp/wiki/Troubleshooting" target="_blank">Check the Troubleshooting</a>
                        <a href={("https://github.com/digitallyinduced/ihp/issues/new?body=" :: Text) <> cs (URI.escapeURIString URI.isUnescapedInURI (cs $ ByteString.unlines errorOutput))} target="_blank">Open a GitHub Issue</a>
                    </div>

                    <pre style="font-family: Menlo, monospace; font-size: 10px" id="stdout">{ByteString.unlines (reverse standardOutput)}</pre>
                </div>
            |]
                where
                    -- Errors are reordered here as we want to display the most important compile errors first
                    -- Warnings should come after the actual errors.
                    errors = errorOutput
                            |> parseErrorOutput
                            |> sortBy (comparing (.isWarning))
            parseErrorOutput :: [ByteString] -> [CompilerError]
            parseErrorOutput output =
                    splitToSections (reverse output) []
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
                            [path, line, col, rest] -> (path, line, col) -- This happens for parser errors from the IHP.SchemaCompiler.compile function
                            [path, line, col] -> (path, line, col)
                            [path, line] -> (path, line, "0")
                            otherwise -> (filePath, "0", "0")
            renderLine (i, line) = [hsx|<div>{line}</div>|]

            renderTroubleshooting :: [ByteString] -> [Html5.Html]
            renderTroubleshooting lines = [ modelContextTroubleshooting ]
                    |> map (\f -> f lines)
                    |> catMaybes

            toolServerPort = ?context.portConfig.toolServerPort

wsApp :: (?context :: Context) => IORef Bool -> Clients -> _ -> _ -> Websocket.ServerApp
wsApp ghciIsLoadingVar stateRef standardOutput errorOutput pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    didChangeMVar <- Concurrent.newEmptyMVar

    modifyIORef stateRef $ \state -> ((connection, didChangeMVar) : state)

    let notifyClient = do
            -- Blocks until a change happens
            Concurrent.takeMVar didChangeMVar

            -- Debounce
            Concurrent.threadDelay 100000 -- 100ms

            isCompiling <- readIORef ghciIsLoadingVar
            standardOutput' <- readIORef standardOutput
            errorOutput' <- readIORef errorOutput
            lastSchemaCompilerError <- readIORef ?context.lastSchemaCompilerError

            let errorContainer = renderErrorView standardOutput' errorOutput' isCompiling lastSchemaCompilerError
            let html = Blaze.renderHtml errorContainer

            result <- Exception.try (Websocket.sendTextData connection html)
            case result of
                Left (Exception.SomeException e) -> pure () -- Client was probably disconnected
                Right _ -> notifyClient

    notifyClient

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
