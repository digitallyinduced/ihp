{-|
Module: IHP.Job.Queue
Description: Functions to operate on the Job Queue Database
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Job.Queue where

import IHP.Prelude
import IHP.Job.Types
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Hasql
import qualified Hasql.Notifications as Notifications
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent as Concurrent
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch
import IHP.Controller.Param
import qualified System.Random as Random
import qualified IHP.PGListener as PGListener
import qualified IHP.Log as Log
import Control.Monad.Trans.Resource

-- | Lock and fetch the next available job. In case no job is available returns Nothing.
--
-- The lock is set on the job row in an atomic way.
--
-- The job status is set to JobStatusRunning, lockedBy will be set to the worker id and the attemptsCount is incremented.
--
-- __Example:__ Locking a SendMailJob
--
-- > let workerId :: UUID = "faa5ba30-1d76-4adf-bf01-2d1f95cddc04"
-- > job <- fetchNextJob @SendMailJob workerId
--
-- After you're done with the job, call 'jobDidFail' or 'jobDidSucceed' to make it available to the queue again.
fetchNextJob :: forall job.
    ( ?modelContext :: ModelContext
    , job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , Table job
    , PrimaryKey (GetTableName job) ~ UUID
    ) => Maybe Int -> BackoffStrategy -> UUID -> IO (Maybe job)
fetchNextJob timeoutInMicroseconds backoffStrategy workerId = do
    let table = tableName @job
    let tableBS = cs table :: ByteString
    let snippet =
            Snippet.sql ("UPDATE \"" <> tableBS <> "\" SET status = ")
            <> Snippet.param (jobStatusToText JobStatusRunning)
            <> Snippet.sql ", locked_at = NOW(), locked_by = "
            <> Snippet.param workerId
            <> Snippet.sql ", attempts_count = attempts_count + 1 WHERE id IN (SELECT id FROM \""
            <> Snippet.sql (tableBS <> "\" WHERE (((status = ")
            <> Snippet.param (jobStatusToText JobStatusNotStarted)
            <> Snippet.sql ") OR (status = "
            <> Snippet.param (jobStatusToText JobStatusRetry)
            <> Snippet.sql (" AND " <> retryQuery backoffStrategy)
            <> Snippet.sql ")) AND locked_by IS NULL AND run_at <= NOW()) "
            <> timeoutConditionSnippet timeoutInMicroseconds
            <> Snippet.sql " ORDER BY created_at LIMIT 1 FOR UPDATE) RETURNING id"

    result :: [Id job] <- withoutQueryLogging do
        sqlQuery snippet (Decoders.rowList (Decoders.column (Decoders.nonNullable (Id <$> Decoders.uuid))))
    case result of
        [] -> pure Nothing
        [id] -> Just <$> withoutQueryLogging (fetch id)
        otherwise -> error (show otherwise)

-- | Calls a callback every time something is inserted, updated or deleted in a given database table.
--
-- In the background this function creates a database trigger to notify this function about table changes
-- using pg_notify. When there are existing triggers, it will silently recreate them. So this will most likely
-- not fail.
--
-- This function returns a Async. Call 'cancel' on the async to stop watching the database.
--
-- __Example:__
--
-- > watchInsertOrUpdateTable "projects" do
-- >     putStrLn "Something changed in the projects table"
--
-- Now insert something into the @projects@ table. E.g. by running @make psql@ and then running @INSERT INTO projects (id, name) VALUES (DEFAULT, 'New project');@
-- You will see that @"Something changed in the projects table"@ is printed onto the screen.
--
watchForJob :: (?modelContext :: ModelContext) => PGListener.PGListener -> Text -> Int -> Maybe Int -> BackoffStrategy -> Concurrent.MVar JobWorkerProcessMessage -> ResourceT IO (PGListener.Subscription, ReleaseKey)
watchForJob pgListener tableName pollInterval timeoutInMicroseconds backoffStrategy onNewJob = do
    let tableNameBS = cs tableName
    liftIO $ withoutQueryLogging (sqlExec (createNotificationTrigger tableNameBS))

    poller <- pollForJob tableName pollInterval timeoutInMicroseconds backoffStrategy onNewJob
    subscription <- liftIO $ pgListener |> PGListener.subscribe (channelName tableNameBS) (const (Concurrent.putMVar onNewJob JobAvailable))

    pure (subscription, poller)

-- | Periodically checks the queue table for open jobs. Calls the callback if there are any.
--
-- 'watchForJob' only catches jobs when something is changed on the table. When a job is scheduled
-- with a 'runAt' in the future, and no other operation is happening on the queue, the database triggers
-- will not run, and so 'watchForJob' cannot pick up the job even when 'runAt' is now in the past.
--
-- This function returns a Async. Call 'cancel' on the async to stop polling the database.
--
pollForJob :: (?modelContext :: ModelContext) => Text -> Int -> Maybe Int -> BackoffStrategy -> Concurrent.MVar JobWorkerProcessMessage -> ResourceT IO ReleaseKey
pollForJob tableName pollInterval timeoutInMicroseconds backoffStrategy onNewJob = do
    let tableBS = cs tableName :: ByteString
    let countSnippet =
            Snippet.sql ("SELECT COUNT(*) FROM \"" <> tableBS <> "\" WHERE (((status = ")
            <> Snippet.param (jobStatusToText JobStatusNotStarted)
            <> Snippet.sql ") OR (status = "
            <> Snippet.param (jobStatusToText JobStatusRetry)
            <> Snippet.sql (" AND " <> retryQuery backoffStrategy)
            <> Snippet.sql ")) AND locked_by IS NULL AND run_at <= NOW()) "
            <> timeoutConditionSnippet timeoutInMicroseconds
            <> Snippet.sql " LIMIT 1"
    let handler = do
            forever do
                -- We don't log the queries to the console as it's filling up the log entries with noise
                count :: Int <- withoutQueryLogging (sqlQueryScalar countSnippet (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))))

                -- For every job we send one signal to the job workers
                -- This way we use full concurrency when we find multiple jobs
                -- that haven't been picked up by the PGListener
                forEach [1..count] \_ -> do
                    Concurrent.putMVar onNewJob JobAvailable

                -- Add up to 2 seconds of jitter to avoid all job queues polling at the same time
                jitter <- Random.randomRIO (0, 2000000)
                let pollIntervalWithJitter = pollInterval + jitter

                Concurrent.threadDelay pollIntervalWithJitter

    fst <$> allocate (Async.async handler) Async.cancel

createNotificationTrigger :: ByteString -> Snippet.Snippet
createNotificationTrigger tableName = Snippet.sql $ ""
        <> "BEGIN;\n"
        <> "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
        <> "BEGIN\n"
        <> "    PERFORM pg_notify('" <> channelName tableName <> "', '');\n"
        <> "    RETURN new;"
        <> "END;\n"
        <> "$$ language plpgsql;"
        <> "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "COMMIT;"
    where
        functionName = "notify_job_queued_" <> tableName
        insertTriggerName = "did_insert_job_" <> tableName
        updateTriggerName = "did_update_job_" <> tableName

-- | Retuns the event name of the event that the pg notify trigger dispatches
channelName :: ByteString -> ByteString
channelName tableName = "job_available_" <> tableName

-- | Called when a job failed. Sets the job status to 'JobStatusFailed' or 'JobStatusRetry' (if more attempts are possible) and resets 'lockedBy'
jobDidFail :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> SomeException -> IO ()
jobDidFail job exception = do
    updatedAt <- getCurrentTime

    Log.warn ("Failed job with exception: " <> tshow exception)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusFailed
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> set #lastError (Just (tshow exception))
        |> updateRecord

    pure ()

jobDidTimeout :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> IO ()
jobDidTimeout job = do
    updatedAt <- getCurrentTime

    Log.warn ("Job timed out" :: Text)

    let ?job = job
    let canRetry = job.attemptsCount < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusTimedOut
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> setJust #lastError "Timeout reached"
        |> updateRecord

    pure ()


-- | Called when a job succeeded. Sets the job status to 'JobStatusSucceded' and resets 'lockedBy'
jobDidSucceed :: forall job context.
    ( job ~ GetModelByTableName (GetTableName job)
    , SetField "lockedBy" job (Maybe UUID)
    , SetField "status" job JobStatus
    , SetField "updatedAt" job UTCTime
    , HasField "attemptsCount" job Int
    , SetField "lastError" job (Maybe Text)
    , Job job
    , CanUpdate job
    , Show job
    , ?modelContext :: ModelContext
    , ?context :: context
    , HasField "logger" context Log.Logger
    ) => job -> IO ()
jobDidSucceed job = do
    Log.info ("Succeeded job" :: Text)
    updatedAt <- getCurrentTime
    job
        |> set #status JobStatusSucceeded
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> updateRecord

    pure ()

-- | Mapping for @JOB_STATUS@ enum
jobStatusToText :: JobStatus -> Text
jobStatusToText JobStatusNotStarted = "job_status_not_started"
jobStatusToText JobStatusRunning = "job_status_running"
jobStatusToText JobStatusFailed = "job_status_failed"
jobStatusToText JobStatusTimedOut = "job_status_timed_out"
jobStatusToText JobStatusSucceeded = "job_status_succeeded"
jobStatusToText JobStatusRetry = "job_status_retry"

-- The default state is @not started@
instance Default JobStatus where
    def = JobStatusNotStarted

instance InputValue JobStatus where
    inputValue = jobStatusToText

instance IHP.Controller.Param.ParamReader JobStatus where
    readParameter = IHP.Controller.Param.enumParamReader

instance FromField JobStatus where
    fromField = Decoders.enum \case
        "job_status_not_started" -> Just JobStatusNotStarted
        "job_status_running" -> Just JobStatusRunning
        "job_status_failed" -> Just JobStatusFailed
        "job_status_timed_out" -> Just JobStatusTimedOut
        "job_status_succeeded" -> Just JobStatusSucceeded
        "job_status_retry" -> Just JobStatusRetry
        _ -> Nothing

retryQuery :: BackoffStrategy -> ByteString
retryQuery LinearBackoff {}      = "updated_at < NOW() + (interval '1 second' * ?)"
retryQuery ExponentialBackoff {} = "updated_at < NOW() - interval '1 second' * ? * POW(2, attempts_count)"

timeoutConditionSnippet :: Maybe Int -> Snippet.Snippet
timeoutConditionSnippet (Just timeoutInMicroseconds) =
    Snippet.sql "OR (status = 'job_status_running' AND locked_by IS NOT NULL AND locked_at + (("
    <> Snippet.param (timeoutInMicroseconds + 1000000)
    <> Snippet.sql ") || 'microseconds')::interval < NOW())"
timeoutConditionSnippet Nothing = Snippet.sql ""
