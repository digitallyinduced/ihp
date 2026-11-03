module IHP.Job.Runner.MainLoop
( runJobWorkers
, dedicatedProcessMainLoop
, installSignalHandlers
, stopExitHandler
) where

import IHP.Prelude
import IHP.ControllerPrelude
import IHP.ScriptSupport
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified System.Posix.Signals as Signals
import qualified Control.Exception.Safe as Exception
import qualified IHP.Job.Queue.Worker as Worker
import qualified System.Timeout as Timeout
import qualified IHP.PGListener as PGListener
import Control.Monad.Trans.Resource
import System.Log.FastLogger (toLogStr)
import Control.Concurrent.STM (atomically, writeTVar)
import IHP.Job.Queue (tryWriteTBQueue)
import Control.Monad (void)

-- | Used by the RunJobs binary
runJobWorkers :: [JobWorker] -> Script
runJobWorkers jobWorkers = dedicatedProcessMainLoop jobWorkers

-- | This job worker main loop is used when the job workers are running as part of their own binary.
-- Both the production @RunJobs@ binary and the dev-mode @RunDevWorker@ use this.
dedicatedProcessMainLoop :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => [JobWorker] -> IO ()
dedicatedProcessMainLoop jobWorkers = do
    workerId <- UUID.nextRandom
    let pool = ?modelContext.hasqlPool
    Worker.ensureWorkerRegistry pool

    ?context.logger (toLogStr ("Starting worker " <> tshow workerId))

    -- The job workers use their own dedicated PG listener as e.g. AutoRefresh or DataSync
    -- could overload the main PGListener connection. In that case we still want jobs to be
    -- run independent of the system being very busy.
    PGListener.withPGListener ?context.databaseUrl ?context.logger \pgListener -> do
        runResourceT do
            waitForExitSignal <- liftIO installSignalHandlers
            -- Registered first, released last: jobs must have stopped executing
            -- before deleting the row releases their foreign keys.
            _ <- allocate (Worker.registerWorker pool workerId) (const (Worker.unregisterWorker pool workerId))
            (_, heartbeat) <- allocate (Async.async $ forever do
                Concurrent.threadDelay 30000000
                alive <- Timeout.timeout 10000000 (Worker.heartbeatWorker pool workerId)
                unless (alive == Just True) $ Exception.throwString "Job worker heartbeat lost; stopping this worker"
                ) Async.cancel
            liftIO $ Async.link heartbeat
            _ <- allocate (Async.async $ forever do
                result <- Exception.tryAny (Worker.reapExpiredWorkers pool)
                case result of
                    Left exception -> ?context.logger (toLogStr ("Job worker recovery: " <> tshow exception))
                    Right () -> pure ()
                Concurrent.threadDelay 30000000
                ) Async.cancel

            let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener }

            processes <- jobWorkers
                |> mapM (\(JobWorker listenAndRun)-> listenAndRun jobWorkerArgs)

            liftIO waitForExitSignal

            liftIO $ ?context.logger (toLogStr ("Waiting for jobs to complete. CTRL+C again to force exit" :: Text))

            -- Mark all workers as stopping before releasing producers, so running workers
            -- finish their current job but don't fetch another one during shutdown.
            liftIO $ forEach processes \JobWorkerProcess { action, isStopping } -> do
                atomically do
                    writeTVar isStopping True
                    _ <- tryWriteTBQueue action Stop
                    pure ()

            -- Stop subscriptions and poller already
            -- This will stop all producers for the queue
            liftIO $ forEach processes \JobWorkerProcess { pollerReleaseKey, subscription, staleRecoveryReleaseKey } -> do
                PGListener.unsubscribe subscription pgListener
                release pollerReleaseKey
                case staleRecoveryReleaseKey of
                    Just key -> release key
                    Nothing -> pure ()

            liftIO $ PGListener.stop pgListener

            -- A second signal ends draining. ResourceT then cancels all execution
            -- groups before unregistering this process. The signal waiter cannot
            -- escape this scope and interrupt a later worker invocation.
            liftIO $ void $ Async.race waitForExitSignal $
                forEach processes \JobWorkerProcess { dispatcher = (_, dispatcherAsync) } ->
                    Async.wait dispatcherAsync

-- | Installs signals handlers and returns an IO action that blocks until the next sigINT or sigTERM is sent
installSignalHandlers :: IO (IO ())
installSignalHandlers = do
    exitSignal <- Concurrent.newEmptyMVar

    let catchHandler = void (Concurrent.tryPutMVar exitSignal ())

    Signals.installHandler Signals.sigINT (Signals.Catch catchHandler) Nothing
    Signals.installHandler Signals.sigTERM (Signals.Catch catchHandler) Nothing

    pure (Concurrent.takeMVar exitSignal)

stopExitHandler :: JobWorkerArgs -> IO a -> IO a
stopExitHandler JobWorkerArgs { .. } main = main
