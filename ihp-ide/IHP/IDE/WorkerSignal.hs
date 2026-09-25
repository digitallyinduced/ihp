{-|
Tiny Unix-socket IPC between the dev-mode @web@ and @worker@ processes.

The @worker@ process listens on a socket; the @web@ process connects and sends
a reload message whenever the file watcher detects a Haskell change. The
worker debounces signals: rapid back-to-back signals collapse into a single
reload (the in-flight one will pick up the latest source state anyway).

@
worker:  withWorkerSignalServer path \\waitForReload -> ...
web:     sendReload path
@
-}
module IHP.IDE.WorkerSignal
( withWorkerSignalServer
, sendReload
) where

import ClassyPrelude
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)

-- | Run a Unix socket server at the given path. The callback receives a
-- @waitForReload@ action that blocks until the next reload signal arrives.
-- The socket file is created (and removed on exit) by this function.
withWorkerSignalServer :: FilePath -> (IO () -> IO a) -> IO a
withWorkerSignalServer path callback = do
    Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
    -- Clean up any stale socket file from a previous crashed run.
    Directory.removeFile path `Exception.catchAny` \_ -> pure ()

    Exception.bracket acquire release \sock -> do
        reloadVar <- newEmptyMVar
        Async.withAsync (acceptLoop sock reloadVar) \_ ->
            callback (takeMVar reloadVar)
  where
    acquire = do
        sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
        Socket.bind sock (Socket.SockAddrUnix path)
        Socket.listen sock 16
        pure sock
    release sock = do
        Socket.close sock `Exception.catchAny` \_ -> pure ()
        Directory.removeFile path `Exception.catchAny` \_ -> pure ()

acceptLoop :: Socket.Socket -> MVar () -> IO ()
acceptLoop sock reloadVar = forever do
    accepted <- Exception.tryAny (Socket.accept sock)
    case accepted of
        Right (conn, _) ->
            -- Handle each connection in the background so a slow client
            -- can't block the accept loop. tryPutMVar collapses rapid signals.
            void $ Async.async $ Exception.bracket_ (pure ()) (Socket.close conn) do
                _ <- Exception.tryAny (SocketBS.recv conn 64)
                void (tryPutMVar reloadVar ())
        Left _ ->
            -- Socket was closed (server is shutting down) or some other
            -- transient error. The bracket release will handle full shutdown.
            pure ()

-- | Send a reload signal to the worker process. Retries briefly if the
-- worker socket isn't ready yet — the worker may still be starting up.
sendReload :: FilePath -> IO ()
sendReload path = retryWithBackoff 25 100000 attempt
  where
    -- Use 'Exception.bracket' with the socket allocation as acquire so the
    -- close handler runs even when 'Socket.connect' fails (e.g. while the
    -- worker is still starting). Otherwise a failed connect would leak the
    -- FD on every retry / every reload.
    attempt = Exception.bracket
        (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
        (\sock -> Socket.close sock `Exception.catchAny` \_ -> pure ())
        \sock -> do
            Socket.connect sock (Socket.SockAddrUnix path)
            SocketBS.sendAll sock "reload\n"

retryWithBackoff :: Int -> Int -> IO () -> IO ()
retryWithBackoff 0 _ action = action  -- last attempt: let exceptions escape
retryWithBackoff n delayUs action = action `Exception.catchAny` \_ -> do
    threadDelay delayUs
    retryWithBackoff (n - 1) delayUs action
