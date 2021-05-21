{-|
Module: IHP.Job.Queue
Description: Functions to operate on the Job Queue Database
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Job.Queue where

import IHP.Prelude
import IHP.Job.Types
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Control.Concurrent.Async as Async
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch
import IHP.Controller.Param

fetchRunningJobs :: forall job.
  ( ?modelContext :: ModelContext
  , job ~ GetModelByTableName (GetTableName job)
  , FilterPrimaryKey (GetTableName job)
  , FromRow job
  , Show (PrimaryKey (GetTableName job))
  , PG.FromField (PrimaryKey (GetTableName job))
  , KnownSymbol (GetTableName job)
  ) => IO [job]
fetchRunningJobs = do
  let query = "SELECT * " <>
              "FROM ? " <>
              "WHERE status = 'job_status_running'"
  let params = PG.Only $ PG.Identifier (tableName @job)
  sqlQuery query params
  
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
    , PG.FromField (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    ) => UUID -> IO (Maybe job)
fetchNextJob workerId = do
    let query = "UPDATE ? SET status = ?, locked_at = NOW(), locked_by = ?, attempts_count = attempts_count + 1 WHERE id IN (SELECT id FROM ? WHERE (status = ?) OR (status = ? AND updated_at < NOW() + interval '30 seconds') AND locked_by IS NULL ORDER BY created_at LIMIT 1 FOR UPDATE) RETURNING id"
    let params = (PG.Identifier (tableName @job), JobStatusRunning, workerId, PG.Identifier (tableName @job), JobStatusNotStarted, JobStatusRetry)

    result :: [PG.Only (Id job)] <- sqlQuery query params
    case result of
        [] -> pure Nothing
        [PG.Only id] -> Just <$> fetch id
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
watchForJob :: (?modelContext :: ModelContext) => Text -> IO () -> IO (Async.Async ())
watchForJob tableName handleJob = do
    sqlExec (PG.Query $ cs $ createNotificationTrigger tableName) ()

    let listenStatement = "LISTEN " <> PG.Query (cs $ eventName tableName)
    Async.asyncBound do
        forever do
            notification <- withDatabaseConnection \databaseConnection -> do
                PG.execute databaseConnection listenStatement ()
                PG.getNotification databaseConnection

            handleJob

createNotificationTrigger :: Text -> Text
createNotificationTrigger tableName = "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
        <> "BEGIN\n"
        <> "    PERFORM pg_notify('" <> eventName tableName <> "', '');\n"
        <> "    RETURN new;"
        <> "END;\n"
        <> "$$ language plpgsql;"
        <> "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW WHEN (NEW.status = 'job_status_not_started' OR NEW.status = 'job_status_retry') EXECUTE PROCEDURE " <> functionName <> "();\n"
    where
        functionName = "notify_job_queued_" <> tableName
        insertTriggerName = "did_insert_job_" <> tableName
        updateTriggerName = "did_update_job_" <> tableName

-- | Retuns the event name of the event that the pg notify trigger dispatches
eventName :: Text -> Text
eventName tableName = "job_available_" <> tableName

-- | Called when a job failed. Uses jobDidFail' under the hood to set the job's status.
jobDidFail :: forall job.
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
    ) => job -> SomeException -> IO ()
jobDidFail job exception = do
    putStrLn ("Failed job with exception: " <> tshow exception)
    jobDidFail' job $ tshow exception

-- | Called by jobDidFail. Sets the job status to 'JobStatusFailed' or 'JobStatusRetry' (if more attempts are possible) and resets 'lockedBy'
jobDidFail' :: forall job.
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
    ) => job -> Text -> IO ()
jobDidFail' job lastExceptionMessage = do
    updatedAt <- getCurrentTime

    let ?job = job
    let canRetry = get #attemptsCount job < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusFailed
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> set #lastError (Just lastExceptionMessage)
        |> updateRecord

    pure ()

jobDidTimeout :: forall job.
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
    ) => job -> IO ()
jobDidTimeout job = do
    updatedAt <- getCurrentTime

    putStrLn "Job timed out"

    let ?job = job
    let canRetry = get #attemptsCount job < maxAttempts
    let status = if canRetry then JobStatusRetry else JobStatusTimedOut
    job
        |> set #status status
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> set #lastError (Just "Timeout reached")
        |> updateRecord

    pure ()
  

-- | Called when a job succeeded. Sets the job status to 'JobStatusSucceded' and resets 'lockedBy'
jobDidSucceed :: forall job.
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
    ) => job -> IO ()
jobDidSucceed job = do
    putStrLn "Succeeded job"
    updatedAt <- getCurrentTime
    job
        |> set #status JobStatusSucceeded
        |> set #lockedBy Nothing
        |> set #updatedAt updatedAt
        |> updateRecord

    pure ()

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
instance PG.FromField JobStatus where
    fromField field (Just "job_status_not_started") = pure JobStatusNotStarted
    fromField field (Just "job_status_running") = pure JobStatusRunning
    fromField field (Just "job_status_failed") = pure JobStatusFailed
    fromField field (Just "job_status_timed_out") = pure JobStatusTimedOut
    fromField field (Just "job_status_succeeded") = pure JobStatusSucceeded
    fromField field (Just "job_status_retry") = pure JobStatusRetry
    fromField field (Just value) = PG.returnError PG.ConversionFailed field ("Unexpected value for enum value. Got: " <> cs value)
    fromField field Nothing = PG.returnError PG.UnexpectedNull field "Unexpected null for enum value"

-- The default state is @not started@
instance Default JobStatus where
    def = JobStatusNotStarted

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
instance PG.ToField JobStatus where
    toField JobStatusNotStarted = PG.toField ("job_status_not_started" :: Text)
    toField JobStatusRunning = PG.toField ("job_status_running" :: Text)
    toField JobStatusFailed = PG.toField ("job_status_failed" :: Text)
    toField JobStatusTimedOut = PG.toField ("job_status_timed_out" :: Text)
    toField JobStatusSucceeded = PG.toField ("job_status_succeeded" :: Text)
    toField JobStatusRetry = PG.toField ("job_status_retry" :: Text)

instance InputValue JobStatus where
    inputValue JobStatusNotStarted = "job_status_not_started" :: Text
    inputValue JobStatusRunning = "job_status_running" :: Text
    inputValue JobStatusFailed = "job_status_failed" :: Text
    inputValue JobStatusTimedOut = "job_status_timed_out" :: Text
    inputValue JobStatusSucceeded = "job_status_succeeded" :: Text
    inputValue JobStatusRetry = "job_status_retry" :: Text

instance IHP.Controller.Param.ParamReader JobStatus where
    readParameter = IHP.Controller.Param.enumParamReader
