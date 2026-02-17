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
import qualified Control.Exception.Safe as Exception
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified System.Posix.Signals as Signals
import qualified System.Exit as Exit
import qualified System.Timeout as Timeout
import qualified IHP.PGListener as PGListener
import Control.Monad.Trans.Resource
import qualified IHP.Log as Log
import IHP.Hasql.FromRow (FromRowHasql)
import Control.Concurrent.STM (atomically, newTBQueue, readTBQueue, writeTBQueue, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar', check)
import IHP.Job.Queue (tryWriteTBQueue)

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

            let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener }

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
        let jobWorkerArgs = JobWorkerArgs { workerId, modelContext = ?modelContext, frameworkConfig = ?context, pgListener }

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

stopExitHandler JobWorkerArgs { .. } main = main

worker :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , Job job
    , Show job
    , Table job
    ) => JobWorker
worker = JobWorker (jobWorkerFetchAndRunLoop @job)


jobWorkerFetchAndRunLoop :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , Job job
    , Show job
    , Table job
    ) => JobWorkerArgs -> ResourceT IO JobWorkerProcess
jobWorkerFetchAndRunLoop JobWorkerArgs { .. } = do
    let ?context = frameworkConfig
    let ?modelContext = modelContext
    let pool = modelContext.hasqlPool
    action <- liftIO $ atomically $ newTBQueue (fromIntegral (maxConcurrency @job))
    -- Seed the queue with one initial JobAvailable so the dispatcher attempts a fetch on startup
    liftIO $ atomically $ writeTBQueue action JobAvailable

    activeCount <- liftIO $ newTVarIO (0 :: Int)
    activeWorkers <- liftIO $ newTVarIO ([] :: [Async ()])

    let runJobLoop = do
            fetchResult <- Exception.tryAny (Queue.fetchNextJob @job pool workerId)
            case fetchResult of
                Left exception -> do
                    Log.error ("Job worker: Failed to fetch next job: " <> tshow exception)
                    Concurrent.threadDelay 1000000  -- 1s backoff to avoid tight error loops
                Right (Just job) -> do
                    Log.info ("Starting job: " <> tshow job)

                    let ?job = job
                    let timeout :: Int = fromMaybe (-1) (timeoutInMicroseconds @job)
                    resultOrException <- Exception.tryAsync (Timeout.timeout timeout (perform job))
                    case resultOrException of
                        Left exception -> do
                            Queue.jobDidFail pool job exception
                            when (Exception.isAsyncException exception) (Exception.throwIO exception)
                        Right Nothing -> Queue.jobDidTimeout pool job
                        Right (Just _) -> Queue.jobDidSucceed pool job

                    runJobLoop -- try next job immediately
                Right Nothing -> pure ()

    let dispatcherLoop = do
            msg <- atomically $ readTBQueue action
            case msg of
                Stop -> do
                    -- Wait for all active workers to finish
                    atomically $ do
                        count <- readTVar activeCount
                        check (count == 0)
                JobAvailable -> do
                    acquired <- atomically $ do
                        count <- readTVar activeCount
                        if count < maxConcurrency @job
                            then do
                                writeTVar activeCount (count + 1)
                                pure True
                            else pure False
                    when acquired do
                        selfVar <- Concurrent.newEmptyMVar
                        workerAsync <- async $
                            (do self <- Concurrent.readMVar selfVar
                                runJobLoop)
                            `Exception.finally`
                                (do maybeSelf <- Concurrent.tryReadMVar selfVar
                                    atomically do
                                        modifyTVar' activeCount (subtract 1)
                                        case maybeSelf of
                                            Just self -> modifyTVar' activeWorkers (filter (/= self))
                                            Nothing -> pure ())
                        Concurrent.putMVar selfVar workerAsync
                        atomically $ modifyTVar' activeWorkers (workerAsync :)
                    dispatcherLoop

    let cancelAllWorkers = do
            workers <- readTVarIO activeWorkers
            mapM_ Async.cancel workers

    dispatcher <- allocate (async (dispatcherLoop `Exception.finally` cancelAllWorkers)) cancel

    (subscription, pollerReleaseKey) <- Queue.watchForJob pool pgListener (tableName @job) (queuePollInterval @job) action

    -- Start stale job recovery if configured
    staleRecoveryReleaseKey <- case staleJobTimeout @job of
        Just threshold -> do
            let intervalMicroseconds = round (threshold / 2) * 1000000
            let recoveryLoop = forever do
                    Queue.recoverStaleJobs @job pool threshold
                    -- Signal workers to check for recovered jobs
                    _ <- atomically $ tryWriteTBQueue action JobAvailable
                    pure ()
                    Concurrent.threadDelay intervalMicroseconds
            (key, _) <- allocate (Async.async recoveryLoop) Async.cancel
            pure (Just key)
        Nothing -> pure Nothing

    pure JobWorkerProcess { dispatcher, subscription, pollerReleaseKey, action, staleRecoveryReleaseKey, activeCount }
