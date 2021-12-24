module IHP.IDE.Repl
( startRepl
, stopRepl
) where

import IHP.ViewPrelude hiding (catch)
import qualified System.Process as Process
import System.IO ( hSetBuffering, hPutStr, hPutStrLn, BufferMode(..) )
import Control.Concurrent
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
import qualified IHP.LibDir as LibDir
import qualified IHP.Log as Log
import qualified Network.URI as URI
import qualified Control.Exception as Exception

startRepl :: (?context :: Context) => IO ()
startRepl = do
    process <- startGHCI
    let ManagedProcess { inputHandle, outputHandle, errorHandle } = process
    hSetBuffering inputHandle LineBuffering

    libDirectory <- LibDir.findLibDirectory

    let loadAppCommands =
            [ ":script " <> cs libDirectory <> "/applicationGhciConfig"
            , ":set prompt \"\"" -- Disable the prompt as this caused output such as '[38;5;208mIHP>[m Ser[v3e8r; 5s;t2a0r8tmedI' instead of 'Server started'
            , "import qualified ClassyPrelude"
            ]

    outputLines <- newIORef []
    inputLine <- newEmptyMVar @ByteString
    clients <- newIORef []

    stdoutListener <- async $ forever do
        line <- ByteString.hGetLine outputHandle
        lines <- atomicModifyIORef'
            outputLines
            (\lines ->
                let l = lines ++ [line] in
                    (l, l))
        currentClients <- readIORef clients
        forConcurrently currentClients (\c -> putMVar c ())
        Log.info ("REPL INFO: " <> line)

    stderrListener <- async $ forever do
        line <- ByteString.hGetLine errorHandle
        atomicModifyIORef' outputLines (\lines -> (lines ++ [line], ()))
        currentClients <- readIORef clients
        forConcurrently currentClients (\c -> putMVar c ())
        Log.error ("REPL ERROR: " <> line)

    inputListener <- async $ forever do
        ByteString.hPutStrLn inputHandle =<< takeMVar inputLine
        pure ()

    forEach loadAppCommands (hPutStrLn inputHandle)
    let state = ReplStarted { process, clients, inputLine, outputLines, stdoutListener, stderrListener, inputListener }
    dispatch (UpdateReplState state)

stopRepl :: (?context :: Context) => ReplState -> IO ()
-- stopRepl ReplNotStarted = error "Unable to stop REPL as REPL is not started."
stopRepl ReplNotStarted = pure ()
stopRepl ReplStarted { process, inputListener, stdoutListener, stderrListener }
    = uninterruptibleCancel stdoutListener
    >> uninterruptibleCancel stderrListener
    >> uninterruptibleCancel inputListener
    >> cleanupManagedProcess process

startGHCI :: IO ManagedProcess
startGHCI = do
    let args =
            [ "-threaded"
            , "-fomit-interface-pragmas"
            , "-j"
            , "-O0"
            , "-package-env -" -- Do not load `~/.ghc/arch-os-version/environments/name file`, global packages interfere with our packages
            , "-ignore-dot-ghci" -- Ignore the global ~/.ghc/ghci.conf That file sometimes causes trouble (specifically `:set +c +s`)
            , "-ghci-script", ".ghci" -- Because the previous line ignored default ghci config file locations, we have to manual load our .ghci
            , "+RTS", "-A128m", "-n2m", "-H2m", "--nonmoving-gc", "-N"
            ]
    createManagedProcess (Process.proc "ghci" args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
