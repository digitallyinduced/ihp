{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Runner.WorkerLoop
( worker
, jobWorkerFetchAndRunLoop
) where

import IHP.Prelude
import IHP.ControllerPrelude
import qualified IHP.Job.Queue as Queue
import qualified IHP.Job.Queue.Worker as Worker
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Async as Async
import qualified System.Timeout as Timeout
import Control.Monad.Trans.Resource
import System.Log.FastLogger (toLogStr)
import IHP.Hasql.FromRow (FromRowHasql)
import Control.Concurrent.STM (atomically, newTBQueue, readTBQueue, writeTBQueue, newTVarIO, readTVar, readTVarIO, modifyTVar')

worker :: forall job.
    ( job ~ GetModelByTableName (GetTableName job)
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , HasField "id" job (Id' (GetTableName job))
    , PrimaryKey (GetTableName job) ~ UUID
    , HasField "runAt" job UTCTime
    , HasField "attemptsCount" job Int
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
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
    , HasField "lockedBy" job (Maybe UUID)
    , HasField "lockedAt" job (Maybe UTCTime)
    , Job job
    , Show job
    , Table job
    ) => JobWorkerArgs -> ResourceT IO JobWorkerProcess
jobWorkerFetchAndRunLoop JobWorkerArgs { .. } = do
    let ?context = frameworkConfig
    let ?modelContext = modelContext
    let pool = modelContext.hasqlPool
    liftIO $ Worker.ensureJobWorkerForeignKey pool (tableName @job)
    action <- liftIO $ atomically $ newTBQueue (fromIntegral (max 1 (maxConcurrency @job)))
    liftIO $ atomically $ writeTBQueue action JobAvailable
    activeCount <- liftIO $ newTVarIO (0 :: Int)
    isStopping <- liftIO $ newTVarIO False

    let runJobLoop = do
            stopping <- readTVarIO isStopping
            unless stopping do
                -- bracketOnError closes the cancellation gap between claiming a
                -- job and entering perform, and also covers failed result writes.
                -- An uncertain database claim/result must stop this worker:
                -- continuing its heartbeat could strand a committed claim forever.
                result <- Exception.bracketOnError
                    (Queue.fetchNextJob @job pool workerId)
                    (mapM_ (Queue.jobDidInterrupt pool))
                    \case
                        Nothing -> pure False
                        Just job -> do
                            ?context.logger (toLogStr ("Starting job: " <> tshow job))
                            let ?job = job
                            let timeout = fromMaybe (-1) (timeoutInMicroseconds @job)
                            -- Async cancellation must unwind perform before the
                            -- bracket releases ownership. It is never a failed attempt.
                            outcome <- Exception.tryAny (Timeout.timeout timeout (perform job))
                            case outcome of
                                Left exception -> Queue.jobDidFail pool job exception
                                Right Nothing -> Queue.jobDidTimeout pool job
                                Right (Just _) -> Queue.jobDidSucceed pool job
                            pure True
                when result runJobLoop

    let executionLoop = do
            shouldRun <- atomically do
                stopping <- readTVar isStopping
                if stopping then pure False else do
                    message <- readTBQueue action
                    pure case message of
                        JobAvailable -> True
                        Stop -> False
            when shouldRun do
                Exception.bracket_
                    (atomically $ modifyTVar' activeCount (+ 1))
                    (atomically $ modifyTVar' activeCount (subtract 1))
                    runJobLoop
                executionLoop

    -- A structured group owns every execution thread, including during startup
    -- and cancellation. No thread can escape the dispatcher's finalizer.
    dispatcher <- allocate
        (Async.async (Async.replicateConcurrently_ (max 1 (maxConcurrency @job)) executionLoop))
        Async.uninterruptibleCancel
    liftIO $ Async.link (snd dispatcher)
    (subscription, pollerReleaseKey) <- Queue.watchForJob pool pgListener (tableName @job) (queuePollInterval @job) action
    let staleRecoveryReleaseKey = Nothing
    pure JobWorkerProcess { dispatcher, subscription, pollerReleaseKey, action, staleRecoveryReleaseKey, activeCount, isStopping }
