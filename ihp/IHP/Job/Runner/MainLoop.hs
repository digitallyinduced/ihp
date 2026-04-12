module IHP.Job.Runner.MainLoop
( runJobWorkers
, dedicatedProcessMainLoop
, devServerMainLoop
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
import qualified System.Exit as Exit
import qualified IHP.PGListener as PGListener
import Control.Monad.Trans.Resource
import qualified Control.Exception.Safe as Exception
import qualified IHP.Log as Log
import Control.Concurrent.STM (atomically, writeTBQueue)

-- | Used by the RunJobs binary
runJobWorkers :: [JobWorker] -> Script
runJobWorkers jobWorkers = dedicatedProcessMainLoop jobWorkers

-- | This job worker main loop is used when the job workers are running as part of their own binary
--
-- In dev mode the IHP dev server is using the 'devServerMainLoop' instead. We have two main loops
-- as the stop handling works a different in those cases.
--
dedicatedProcessMainLoop :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => [JobWorker] -> IO ()
dedicatedProcessMainLoop jobWorkers = do
    threadId <- Concurrent.myThreadId
    exitSignalsCount <- newIORef 0
    workerId <- UUID.nextRandom
    let logger = ?context.logger

    Log.info ("Starting worker " <> tshow workerId)

    -- The job workers use their own dedicated PG listener as e.g. AutoRefresh or DataSync
    -- could overload the main PGListener connection. In that case we still want jobs to be
    -- run independent of the system being very busy.
    PGListener.withPGListener ?context.databaseUrl ?context.logger \pgListener -> do
        stopSignal <- Concurrent.newEmptyMVar

        runResourceT do
            waitForExitSignal <- liftIO installSignalHandlers

            let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener, databaseUrl = ?context.databaseUrl }

            processes <- jobWorkers
                |> mapM (\(JobWorker listenAndRun)-> listenAndRun jobWorkerArgs)

            liftIO waitForExitSignal

            liftIO $ Log.info ("Waiting for jobs to complete. CTRL+C again to force exit" :: Text)

            -- Stop subscriptions and poller already
            -- This will stop all producers for the queue
            liftIO $ forEach processes \JobWorkerProcess { pollerReleaseKey, subscription, action, staleRecoveryReleaseKey } -> do
                PGListener.unsubscribe subscription pgListener
                release pollerReleaseKey
                case staleRecoveryReleaseKey of
                    Just key -> release key
                    Nothing -> pure ()
                -- Single Stop for the dispatcher (it waits for active workers internally)
                atomically $ writeTBQueue action Stop

            liftIO $ PGListener.stop pgListener

            -- While waiting for all jobs to complete, we also wait for another exit signal
            -- If the user sends two exit signals, we just kill all processes
            liftIO $ async do
                waitForExitSignal

                Log.info ("Canceling all running jobs. CTRL+C again to force exit" :: Text)

                forEach processes \JobWorkerProcess { dispatcher = (dispatcherKey, _) } -> do
                    release dispatcherKey  -- cancels dispatcher, whose finally cancels all workers

                Concurrent.throwTo threadId Exit.ExitSuccess

                pure ()

            -- Wait for dispatchers (which wait for their workers before exiting)
            liftIO $ forEach processes \JobWorkerProcess { dispatcher = (_, dispatcherAsync) } -> do
                Async.wait dispatcherAsync

            liftIO $ Concurrent.throwTo threadId Exit.ExitSuccess

devServerMainLoop :: (?modelContext :: ModelContext) => FrameworkConfig -> PGListener.PGListener -> [JobWorker] -> IO ()
devServerMainLoop frameworkConfig pgListener jobWorkers = do
    workerId <- UUID.nextRandom
    let ?context = frameworkConfig
    let logger = frameworkConfig.logger

    Log.info ("Starting worker " <> tshow workerId)

    runResourceT do
        let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener, databaseUrl = frameworkConfig.databaseUrl }

        processes <- jobWorkers
                |> mapM (\(JobWorker listenAndRun) -> listenAndRun jobWorkerArgs)

        liftIO $ (forever (Concurrent.threadDelay maxBound)) `Exception.finally` do
            forEach processes \JobWorkerProcess { action } -> do
                atomically $ writeTBQueue action Stop

-- | Installs signals handlers and returns an IO action that blocks until the next sigINT or sigTERM is sent
installSignalHandlers :: IO (IO ())
installSignalHandlers = do
    exitSignal <- Concurrent.newEmptyMVar

    let catchHandler = Concurrent.putMVar exitSignal ()

    Signals.installHandler Signals.sigINT (Signals.Catch catchHandler) Nothing
    Signals.installHandler Signals.sigTERM (Signals.Catch catchHandler) Nothing

    pure (Concurrent.takeMVar exitSignal)

stopExitHandler :: JobWorkerArgs -> IO a -> IO a
stopExitHandler JobWorkerArgs { .. } main = main
