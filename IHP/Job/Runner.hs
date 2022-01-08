{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.Job.Runner
Description: Functions to run jobs
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Job.Runner where

import IHP.Prelude
import IHP.ControllerPrelude
import IHP.ScriptSupport
import qualified IHP.Job.Queue as Queue
import qualified Control.Exception as Exception
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified System.Posix.Signals as Signals
import qualified System.Exit as Exit
import qualified System.Timeout as Timeout
import qualified Control.Concurrent.Async.Pool as Pool
import qualified IHP.PGListener as PGListener

import IHP.Log.Types
import qualified IHP.Log as Log

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
    let logger = ?context |> get #logger

    Log.info ("Starting worker " <> tshow workerId)

    -- The job workers use their own dedicated PG listener as e.g. AutoRefresh or DataSync
    -- could overload the main PGListener connection. In that case we still want jobs to be
    -- run independent of the system being very busy.
    pgListener <- PGListener.init ?modelContext
    stopSignal <- Concurrent.newEmptyMVar
    waitForExitSignal <- installSignalHandlers

    let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener }
    
    processes <- jobWorkers
        |> mapM (\(JobWorker listenAndRun)-> listenAndRun jobWorkerArgs)

    waitForExitSignal

    Log.info ("Waiting for jobs to complete. CTRL+C again to force exit" :: Text)

    -- Stop subscriptions and poller already
    -- This will stop all producers for the queue MVar
    forEach processes \JobWorkerProcess { poller, subscription, action } -> do
        PGListener.unsubscribe subscription pgListener
        Async.cancel poller
        Concurrent.putMVar action Stop

    PGListener.stop pgListener

    -- While waiting for all jobs to complete, we also wait for another exit signal
    -- If the user sends two exit signals, we just kill all processes
    async do
        waitForExitSignal

        Log.info ("Canceling all running jobs. CTRL+C again to force exit" :: Text)
        
        forEach processes \JobWorkerProcess { runners } -> do
            forEach runners Async.cancel

        Concurrent.throwTo threadId Exit.ExitSuccess

        pure ()

    -- Wait for all runners to complete
    forEach processes \JobWorkerProcess { runners } -> do
        forEach runners Async.wait

    Concurrent.throwTo threadId Exit.ExitSuccess

devServerMainLoop :: (?modelContext :: ModelContext) => FrameworkConfig -> PGListener.PGListener -> [JobWorker] -> IO ()
devServerMainLoop frameworkConfig pgListener jobWorkers = do
    workerId <- UUID.nextRandom
    let ?context = frameworkConfig
    let logger = frameworkConfig |> get #logger

    Log.info ("Starting worker " <> tshow workerId)

    let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener }
    
    processes <- jobWorkers
        |> mapM (\(JobWorker listenAndRun)-> listenAndRun jobWorkerArgs)

    (forever (Concurrent.threadDelay maxBound)) `Exception.finally` do
        forEach processes \JobWorkerProcess { poller, subscription, runners, action } -> do
            Concurrent.putMVar action Stop
            Async.cancel poller
            forEach runners Async.cancel

-- | Installs signals handlers and returns an IO action that blocks until the next sigINT or sigTERM is sent
installSignalHandlers :: IO (IO ())
installSignalHandlers = do
    exitSignal <- Concurrent.newEmptyMVar

    let catchHandler = Concurrent.putMVar exitSignal ()
            
    Signals.installHandler Signals.sigINT (Signals.Catch catchHandler) Nothing
    Signals.installHandler Signals.sigTERM (Signals.Catch catchHandler) Nothing

    pure (Concurrent.takeMVar exitSignal)

stopExitHandler JobWorkerArgs { .. } main = main

worker :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , SetField "attemptsCount" job Int
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , Table job
    ) => JobWorker
worker = JobWorker (jobWorkerFetchAndRunLoop @job)


jobWorkerFetchAndRunLoop :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , SetField "attemptsCount" job Int
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , Table job
    ) => JobWorkerArgs -> IO JobWorkerProcess
jobWorkerFetchAndRunLoop JobWorkerArgs { .. } = do
    let ?context = frameworkConfig
    let ?modelContext = modelContext

    action <- Concurrent.newMVar JobAvailable
    runners <- forM [0..(maxConcurrency @job)] \index -> async do
        let loop = do
                receivedAction <- Concurrent.takeMVar action

                case receivedAction of
                    JobAvailable -> do
                        maybeJob <- Queue.fetchNextJob @job workerId
                        case maybeJob of
                            Just job -> do
                                Log.info ("Starting job: " <> tshow job)

                                let ?job = job
                                let timeout :: Int = fromMaybe (-1) (timeoutInMicroseconds @job)
                                resultOrException <- Exception.try (Timeout.timeout timeout (perform job))
                                case resultOrException of
                                    Left exception -> Queue.jobDidFail job exception
                                    Right Nothing -> Queue.jobDidTimeout job
                                    Right (Just _) -> Queue.jobDidSucceed job

                                loop
                            Nothing -> loop
                    Stop -> do
                        -- Put the stop signal back in to stop the other runners as well
                        Concurrent.putMVar action Stop
                        pure ()

        loop

    (subscription, poller) <- Queue.watchForJob pgListener (tableName @job) (queuePollInterval @job) action


    pure JobWorkerProcess { runners, subscription, poller, action }