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

runJobWorkers :: [JobWorker] -> Script
runJobWorkers jobWorkers = do
    workerId <- UUID.nextRandom
    allJobs <- newIORef []
    let oneSecond = 1000000

    putStrLn ("Starting worker " <> tshow workerId)

    let jobWorkerArgs = JobWorkerArgs { allJobs, workerId, modelContext = ?modelContext, frameworkConfig = ?context}

    threadId <- Concurrent.myThreadId
    exitSignalsCount <- newIORef 0
    let catchHandler = do
            exitSignalsCount' <- readIORef exitSignalsCount
            modifyIORef exitSignalsCount ((+) 1)
            allJobs' <- readIORef allJobs
            allJobsCompleted <- allJobs'
                    |> mapM Async.poll
                    >>= pure . filter isNothing
                    >>= pure . null
            if allJobsCompleted
                then Concurrent.throwTo threadId Exit.ExitSuccess
                else if exitSignalsCount' == 0
                    then do
                        putStrLn "Waiting for jobs to complete. CTRL+C again to force exit"
                        forEach allJobs' Async.wait
                        Concurrent.throwTo threadId Exit.ExitSuccess
                else if exitSignalsCount' == 1 then do
                        putStrLn "Canceling all running jobs. CTRL+C again to force exit"
                        forEach allJobs' Async.cancel
                        Concurrent.throwTo threadId Exit.ExitSuccess
                else Concurrent.throwTo threadId Exit.ExitSuccess

            
    Signals.installHandler Signals.sigINT (Signals.Catch catchHandler) Nothing
    Signals.installHandler Signals.sigTERM (Signals.Catch catchHandler) Nothing

    jobWorkers
        |> mapM (\(JobWorker listenAndRun)-> listenAndRun jobWorkerArgs)
        >>= Async.waitAnyCancel

    pure ()


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
    ) => JobWorkerArgs -> IO (Async.Async ())
jobWorkerFetchAndRunLoop JobWorkerArgs { .. } = do
    let ?context = frameworkConfig
    let ?modelContext = modelContext

    -- This loop schedules all jobs that are in the queue.
    -- It will be initally be called when first starting up this job worker
    -- and after that it will be called when something has been inserted into the queue (or changed to retry)
    let startLoop = do
            asyncJob <- Async.async do
                Exception.mask $ \restore -> do
                    maybeJob <- Queue.fetchNextJob @job workerId
                    case maybeJob of
                        Just job -> do
                            putStrLn ("Starting job: " <> tshow job)
                            let ?job = job
                            let timeout :: Int = fromMaybe (-1) (timeoutInMicroseconds @job)
                            resultOrException <- Exception.try (Timeout.timeout timeout $ restore (perform job))
                            case resultOrException of
                                Left exception -> Queue.jobDidFail job exception
                                Right Nothing -> Queue.jobDidTimeout job
                                Right (Just _) -> Queue.jobDidSucceed job

                            startLoop
                        Nothing -> pure ()
            modifyIORef allJobs (asyncJob:)

    -- Start all jobs in the queue
    startLoop

    -- Start a job when a new job is added to the table or when it's set to retry
    watcher <- Queue.watchForJob (tableName @job) (queuePollInterval @job) startLoop

    pure watcher
