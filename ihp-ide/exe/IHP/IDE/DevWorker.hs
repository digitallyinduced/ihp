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

    withGhci wrapWithDirenv mainThreadId \input output err _processHandle -> do
        -- One iteration of the worker lifecycle. @loaded@ says whether the most
        -- recent @:l@ / @:r@ succeeded. When the worker is running we drain
        -- GHCi for its whole lifetime ('withRunningWorker'); when it isn't we
        -- still drain GHCi so it can never block on a full pipe
        -- ('drainUntilReload'). Both return only once a reload signal has
        -- arrived, at which point we @:r@ and recurse. There is exactly one
        -- reader-set on the GHCi handles at a time: the lifetime readers while
        -- the worker runs, the bounded readers in 'ghciLoadOrReload' while it
        -- is stopped — they never overlap. Mirrors 'IHP.IDE.DevServer'.
        let loop loaded = do
                if loaded
                    then withRunningWorker input output err waitForReload
                    else drainUntilReload output err waitForReload
                putStrLn "[worker] Reload signal received."
                result <- refreshGhci input output err
                case result of
                    Left out -> do
                        putStrLn "[worker] GHCi reload failed:"
                        ByteString.putStrLn (toStrict out)
                        loop False
                    Right _ -> loop True

        initialLoad <- loadRunJobs input output err
        case initialLoad of
            Left out -> do
                putStrLn "[worker] GHCi failed to load RunJobs.hs:"
                ByteString.putStrLn (toStrict out)
                loop False
            Right _ -> loop True

-- | Start the worker app inside the loaded GHCi, drain its stdout/stderr for
-- the entire time it runs, and return once a reload signal has arrived.
--
-- Mirrors 'IHP.IDE.DevServer.withRunningApp': the two 'readHandleLines' race
-- @workerStopped@ (filled by @stopApp@ on bracket exit), so they keep draining
-- GHCi the whole time the worker runs — including while we are blocked on
-- @waitForReload@. @onMatch@ surfaces startup / crash without stopping them.
-- Startup is bounded by a 60s timeout and a crash marker so a worker that
-- throws before logging "Starting worker" can no longer hang the process — we
-- log it and wait for the next reload signal to recover.
withRunningWorker :: Handle -> Handle -> Handle -> IO () -> IO ()
withRunningWorker input output err waitForReload = do
    outputVar <- newMVar mempty
    started <- newEmptyMVar
    crashed <- newEmptyMVar
    workerStopped <- newEmptyMVar

    let onMatch line
            | "Starting worker" `isInfixOf` line = void (tryPutMVar started ())
            | "[[IHP_WORKER_CRASHED]]" `isInfixOf` line = void (tryPutMVar crashed ())
            | otherwise = pure ()

    let startApp = do
            sendGhciCommand input "stopVar :: ClassyPrelude.MVar () <- ClassyPrelude.newEmptyMVar"
            sendGhciCommand input "app <- ClassyPrelude.async (ClassyPrelude.race_ (ClassyPrelude.takeMVar stopVar) (main `ClassyPrelude.catch` \\(e :: ClassyPrelude.SomeException) -> ClassyPrelude.putStrLn (ClassyPrelude.tshow e) ClassyPrelude.>> ClassyPrelude.putStrLn \"[[IHP_WORKER_CRASHED]]\"))"

    let stopApp = do
            sendGhciCommand input "ClassyPrelude.putMVar stopVar ()"
            sendGhciCommand input "ClassyPrelude.cancel app"
            -- Give GHCi a moment to drop refs to the old app.
            Concurrent.threadDelay 100000
            void (tryPutMVar workerStopped ())

    let waitForWorkerStart = do
            outcome <- timeout (60 * 1000000) (Async.race (takeMVar started) (takeMVar crashed))
            case outcome of
                Just (Left ()) -> putStrLn "[worker] worker started."
                Just (Right ()) -> putStrLn "[worker] worker crashed during startup; waiting for a reload."
                Nothing -> putStrLn "[worker] worker startup timed out after 60s; waiting for a reload."
            -- Whether or not startup succeeded, block here until the next reload
            -- signal. GHCi keeps being drained throughout (the readers below
            -- race workerStopped, which stopApp only fills on bracket exit), so
            -- a chatty worker can't wedge on a full pipe and a failed startup
            -- recovers on the next file change instead of hanging.
            waitForReload

    void $ runConcurrently $ (,,)
        <$> Concurrently (bracket_ startApp stopApp waitForWorkerStart)
        <*> Concurrently (readHandleLines workerStopped outputVar output putForward onMatch)
        <*> Concurrently (readHandleLines workerStopped outputVar err putForward onMatch)
  where
    putForward line = ByteString.putStrLn ("[worker] " <> line)

-- | No worker is running (a @:l@/@:r@ failed). Keep GHCi drained so it can
-- never block on a full pipe, and return once a reload signal has arrived.
drainUntilReload :: Handle -> Handle -> IO () -> IO ()
drainUntilReload output err waitForReload = do
    outputVar <- newMVar mempty
    stop <- newEmptyMVar
    let onMatch _ = pure ()
    void $ runConcurrently $ (,,)
        <$> Concurrently (waitForReload `finally` void (tryPutMVar stop ()))
        <*> Concurrently (readHandleLines stop outputVar output putForward onMatch)
        <*> Concurrently (readHandleLines stop outputVar err putForward onMatch)
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
