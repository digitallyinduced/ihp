{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Runner.WorkerLoop
( worker
, jobWorkerFetchAndRunLoop
) where

import IHP.Prelude
import IHP.ControllerPrelude
import qualified IHP.Environment as Environment
import qualified IHP.Job.Queue as Queue
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified System.Timeout as Timeout
import Control.Monad.Trans.Resource
import qualified IHP.Log as Log
import IHP.Hasql.FromRow (FromRowHasql)
import Control.Concurrent.STM (atomically, newTBQueue, readTBQueue, writeTBQueue, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar', check)
import IHP.Job.Queue (tryWriteTBQueue)

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
                    runJobLoop -- retry after transient error
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

    let enablePollerTriggerRepair = frameworkConfig.environment == Environment.Development
    (subscription, pollerReleaseKey) <- Queue.watchForJobWithPollerTriggerRepair enablePollerTriggerRepair pool pgListener (tableName @job) (queuePollInterval @job) action

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
