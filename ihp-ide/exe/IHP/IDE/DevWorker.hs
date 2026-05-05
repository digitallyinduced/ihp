{-|
Dev-mode job worker process.

Spawned alongside 'IHP.IDE.DevServer' in the devenv process manager. The web
process owns the file watcher and signals this process over a Unix socket
whenever a Haskell file changes; in response we @:r@ the GHCi session and
restart the worker thread.
-}
module Main (main) where

import ClassyPrelude
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.Process as Process
import Main.Utf8 (withUtf8)

import qualified Control.Concurrent.Async as Async
import qualified IHP.EnvVar as EnvVar
import IHP.IDE.GhciSupport
import qualified IHP.IDE.SplitMode as SplitMode
import qualified IHP.IDE.WorkerSignal as WorkerSignal

main :: IO ()
main = withUtf8 do
    mainThreadId <- Concurrent.myThreadId

    -- devenv pipes our stdio through process-compose; default block buffering
    -- would hide our output until the buffer fills, so switch to line buffering.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering

    -- Always start the signal server, regardless of whether jobs exist yet.
    -- The web process re-checks 'hasJobs' on every file change and writes
    -- build/RunJobs.hs as soon as the project gains its first job; the signal
    -- it sends here is what wakes us up to discover the new file. Gating on
    -- 'hasJobs' at boot would leave the worker permanently idle for projects
    -- whose first job is scaffolded *after* `devenv up` has started.
    WorkerSignal.withWorkerSignalServer SplitMode.workerSocketPath \waitForReload -> do
        waitUntilJobsReady waitForReload
        runWithJobs mainThreadId waitForReload

-- | Block until both the schema (build/Generated/Types.hs, written by the web
-- process when it compiles the schema) and the worker entry module
-- (build/RunJobs.hs, written by the web process when it detects 'hasJobs')
-- are present. Wakes immediately on any reload signal from the web (which
-- fires on every Haskell change), with a 2-second poll fallback so we still
-- recover if a signal is dropped.
waitUntilJobsReady :: IO () -> IO ()
waitUntilJobsReady waitForReload = do
    putStrLn "[worker] waiting for build/Generated/Types.hs and build/RunJobs.hs..."
    let pollInterval = 2 * 1000000
    let loop = do
            ready <- bothReady
            unless ready do
                void $ Async.race waitForReload (Concurrent.threadDelay pollInterval)
                loop
    loop
  where
    bothReady = (&&)
        <$> Directory.doesFileExist "build/Generated/Types.hs"
        <*> Directory.doesFileExist "build/RunJobs.hs"

runWithJobs :: Concurrent.ThreadId -> IO () -> IO ()
runWithJobs mainThreadId waitForReload = do
    wrapWithDirenv <- EnvVar.envOrDefault "IHP_DEV_WRAP_DIRENV" False

    withGhci wrapWithDirenv mainThreadId \input output err processHandle -> do
        -- Initial load and start of the worker app.
        initialLoad <- loadRunJobs input output err
        case initialLoad of
            Left out -> do
                putStrLn "[worker] GHCi failed to load RunJobs.hs:"
                ByteString.putStrLn (toStrict out)
                -- Wait for a reload before retrying so we don't busy-loop.
                reloadLoop input output err processHandle waitForReload Nothing
            Right _ -> do
                appHandle <- startWorkerApp input output err
                reloadLoop input output err processHandle waitForReload (Just appHandle)

reloadLoop
    :: Handle -> Handle -> Handle -> Process.ProcessHandle
    -> IO () -- ^ waitForReload
    -> Maybe AppHandle -- ^ Nothing if previous load failed
    -> IO ()
reloadLoop input output err processHandle waitForReload =
    let go currentApp = do
            waitForReload
            putStrLn "[worker] Reload signal received."

            -- Stop the running app, if any.
            case currentApp of
                Just (AppHandle stopVar) -> do
                    sendGhciCommand input "ClassyPrelude.putMVar stopVar ()"
                    sendGhciCommand input "ClassyPrelude.cancel app"
                    -- Give GHCi a moment to drop refs to the old app.
                    Concurrent.threadDelay 100000
                    pure ()
                Nothing -> pure ()

            -- Reload modules.
            result <- refreshGhci input output err
            case result of
                Left out -> do
                    putStrLn "[worker] GHCi reload failed:"
                    ByteString.putStrLn (toStrict out)
                    go Nothing
                Right _ -> do
                    appHandle <- startWorkerApp input output err
                    go (Just appHandle)
    in go

-- | Per-app-instance GHCi state. The @stopVar@ is the GHCi-side MVar that the
-- worker thread races on; we fill it from the host side to ask the thread to exit.
newtype AppHandle = AppHandle ByteString -- name of the stopVar binding (always "stopVar")

-- | Send GHCi the commands to start the worker (i.e. to call its 'main')
-- inside an async, and wait until it logs "Starting worker".
startWorkerApp :: Handle -> Handle -> Handle -> IO AppHandle
startWorkerApp input output err = do
    sendGhciCommand input "stopVar :: ClassyPrelude.MVar () <- ClassyPrelude.newEmptyMVar"
    sendGhciCommand input "app <- ClassyPrelude.async (ClassyPrelude.race_ (ClassyPrelude.takeMVar stopVar) (main `ClassyPrelude.catch` \\(e :: ClassyPrelude.SomeException) -> ClassyPrelude.putStrLn (ClassyPrelude.tshow e) ClassyPrelude.>> ClassyPrelude.putStrLn \"[[IHP_WORKER_CRASHED]]\"))"

    waitForLine output err "Starting worker"
    pure (AppHandle "stopVar")

-- | Wait for a specific line to appear on either GHCi stream.
waitForLine :: Handle -> Handle -> ByteString -> IO ()
waitForLine output err marker = do
    seen <- newEmptyMVar
    outputVar <- newMVar mempty
    let onMatch line = when (marker `isInfixOf` line) (void (tryPutMVar seen ()))
    let pump h logSink = readHandleLines seen outputVar h logSink onMatch

    void $ runConcurrently $ (,,)
        <$> Concurrently (takeMVar seen)
        <*> Concurrently (pump output putForward)
        <*> Concurrently (pump err putForward)
  where
    putForward line = ByteString.putStrLn ("[worker] " <> line)

-- | Issue an initial `:l build/RunJobs.hs` and wait for "modules loaded" or
-- "Failed,". Returns Left full-output on failure, Right full-output on success.
loadRunJobs :: Handle -> Handle -> Handle -> IO (Either LByteString LByteString)
loadRunJobs input output err = ghciLoadOrReload input output err loadCmd
  where
    loadCmd = do
        sendGhciCommand input ":set prompt \"\""
        sendGhciCommand input "import qualified ClassyPrelude"
        sendGhciCommand input ":l build/RunJobs.hs"

-- | Issue `:r` and wait for the result.
refreshGhci :: Handle -> Handle -> Handle -> IO (Either LByteString LByteString)
refreshGhci input output err = ghciLoadOrReload input output err (sendGhciCommand input ":r")

ghciLoadOrReload :: Handle -> Handle -> Handle -> IO () -> IO (Either LByteString LByteString)
ghciLoadOrReload _input output err sendLoadCmd = do
    outputVar <- newMVar mempty
    resultVar <- newEmptyMVar
    let onMatch line
            | "Failed," `isInfixOf` line = void (tryPutMVar resultVar False)
            | "modules loaded." `isInfixOf` line || "modules reloaded." `isInfixOf` line =
                void (tryPutMVar resultVar True)
            | otherwise = pure ()

    let main = do
            sendLoadCmd
            ok <- takeMVar resultVar
            buf <- takeMVar outputVar
            let bs = Builder.toLazyByteString buf
            pure (if ok then Right bs else Left bs)

    let putForward line = ByteString.putStrLn ("[worker] " <> line)
    (result, _, _) <- runConcurrently $ (,,)
        <$> Concurrently main
        <*> Concurrently (readHandleLines resultVar outputVar output putForward onMatch)
        <*> Concurrently (readHandleLines resultVar outputVar err putForward onMatch)

    pure result
