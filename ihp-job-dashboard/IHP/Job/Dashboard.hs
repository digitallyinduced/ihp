{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module: IHP.Job.Dashboard
Description:  Auto-generate a dashboard for job types

This module allows IHP applications to generate a dashboard for interacting with job types.
To start, first define a type for the dashboard:

> type MyDashboard = JobsDashboardController NoAuth '[]

And include the following in the 'controllers' list of a FrontController:

> parseRoute @MyDashboard

This generates a dashboard with listings for all tables which have names ending with "_jobs".

All views are fully customizable. For more info, see the documentation for 'DisplayableJob'.
If you implement custom behavior for a job type, add it to the list in the Dashboard type:

> type MyDashboard = JobsDashboardController NoAuth '[EmailUserJob, UpdateRecordJob]
-}
module IHP.Job.Dashboard (
    module IHP.Job.Dashboard.View,
    module IHP.Job.Dashboard.Auth,
    module IHP.Job.Dashboard.Types,

    JobsDashboard(..),
    DisplayableJob(..),
    JobsDashboardController(..),
    getTableName,
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.ControllerPrelude
import Wai.Request.Params.Middleware (Respond)
import Unsafe.Coerce
import IHP.Job.Queue ()
import IHP.Pagination.Types
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Network.Wai as Wai
import Network.Wai (requestMethod)
import Network.HTTP.Types.Method (methodPost)

import IHP.Job.Dashboard.Types
import IHP.Job.Dashboard.View
import IHP.Job.Dashboard.Auth
import IHP.Hasql.FromRow (FromRowHasql)
import IHP.Job.Dashboard.Utils
import qualified Hasql.Implicits.Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet

-- | The crazy list of type constraints for this class defines everything needed for a generic "Job".
-- All jobs created through the IHP dev IDE will automatically satisfy these constraints and thus be able to
-- be used as a 'DisplayableJob'.
-- To customize the dashboard behavior for each job, you should provide a custom implementation of 'DisplayableJob'
-- for your job type. Your custom implementations will then be used instead of the defaults.
class ( job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , FromRowHasql job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
    , PG.ToField (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , HasField "id" job (Id job)
    , HasField "status" job JobStatus
    , HasField "updatedAt" job UTCTime
    , HasField "createdAt" job UTCTime
    , HasField "lastError" job (Maybe Text)
    , CanUpdate job
    , CanCreate job
    , Record job
    , Show job
    , Eq job
    , Table job
    , Typeable job
    , Hasql.Implicits.Encoders.DefaultParamEncoder (Id' (GetTableName job))
    ) => DisplayableJob job where

    -- | How this job's section should be displayed in the dashboard. By default it's displayed as a table,
    -- but this can be any arbitrary view! Make some cool graphs :)
    makeDashboardSection :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO SomeView

    makePageView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO SomeView

    -- | The content of the page that will be displayed for a detail view of this job.
    -- By default, the ID, Status, Created/Updated at times, and last error are displayed.
    -- Can be defined as any arbitrary view.
    makeDetailView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => job -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ HtmlView $ renderBaseJobDetailView (buildBaseJob job)

    -- | The content of the page that will be displayed for the "new job" form of this job.
    -- By default, only the submit button is rendered. For additonal form data, define your own implementation.
    -- Can be defined as any arbitrary view, but it should be a form.
    makeNewJobView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO SomeView
    makeNewJobView = pure $ SomeView $ HtmlView $ renderNewBaseJobForm $ tableName @job

    -- | The action run to create and insert a new value of this job into the database.
    -- By default, create an empty record and insert it.
    -- To add more data, define your own implementation.
    createNewJob :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
    createNewJob = do
        newRecord @job |> create
        pure ()




-- | Defines implementations for actions for acting on a dashboard made of some list of types.
-- This is included to allow these actions to recurse on the types, isn't possible in an IHP Controller
-- action implementation.
--
-- Later functions and typeclasses introduce constraints on the types in this list,
-- so you'll get a compile error if you try and include a type that is not a job.
class JobsDashboard (jobs :: [Type]) where
    -- | Creates the entire dashboard by recursing on the type list and calling 'makeDashboardSection' on each type.
    makeDashboard :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?request :: Wai.Request) => IO SomeView

    includedJobTables :: [Text]

    -- | Renders the index page, which is the view returned from 'makeDashboard'.
    indexPage :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => IO ()

    listJob :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Text -> IO ()
    listJob' :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Bool -> IO ()

    -- | Renders the detail view page. Rescurses on the type list to find a type with the
    -- same table name as the "tableName" query parameter.
    viewJob :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Text -> UUID -> IO ()
    viewJob' :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Bool -> IO ()

    -- | If performed in a POST request, creates a new job depending on the "tableName" query parameter.
    -- If performed in a GET request, renders the new job from depending on said parameter.
    newJob :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Text -> IO ()
    newJob' :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Bool -> IO ()

    -- | Deletes a job from the database.
    deleteJob :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Text -> UUID -> IO ()
    deleteJob' :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Bool -> IO ()

    retryJob :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => Text -> UUID -> IO ()
    retryJob' :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Wai.Request) => IO ()

-- If no types are passed, try to get all tables dynamically and render them as BaseJobs
instance JobsDashboard '[] where

    -- | Invoked at the end of recursion
    makeDashboard = pure $ SomeView $ HtmlView [hsx|
        <script>
            function initPopover() {
                $('[data-bs-toggle="popover"]').popover({ trigger: 'hover click' })
            }
            $(document).on('ready turbolinks:load', initPopover);
            $(initPopover);
        </script>
        <style>
        .popover-body {
            background-color: #01313f;
            color: rgb(147, 161, 161);
            font-family: Monaco, Menlo, "Ubuntu Mono", Consolas, source-code-pro, monospace;
            font-size: 11px;
        }
        </style>
    |]

    includedJobTables = []

    indexPage = do
        tableNames <- getAllTableNames
        tables <- mapM buildBaseJobTable tableNames
        render $ SomeView tables
        where
            getAllTableNames = sqlQueryHasql getHasqlPool
                (Snippet.sql "SELECT table_name FROM information_schema.tables WHERE table_name LIKE '%_jobs'")
                (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

    listJob = error "listJob: Requested job type not in JobsDashboard Type"
    listJob' _ = do
        let table = param "tableName"
            options = defaultPaginationOptions
            page = paramOrDefault 1 "page"
            pageSize = paramOrDefault (maxItems options) "maxItems"
        totalItems <- totalRecordsForTable table
        jobs <- queryBaseJobsFromTablePaginated table (page - 1) pageSize
        let pagination = Pagination { currentPage = page, totalItems, pageSize, window = windowSize options }
        render $ HtmlView $ renderBaseJobTablePaginated table jobs pagination

    viewJob = error "viewJob: Requested job type not in JobsDashboard Type"
    viewJob' _ = do
        baseJob <- queryBaseJob (param "tableName") (param "id")
        render $ HtmlView $ renderBaseJobDetailView baseJob

    newJob = error "newJob: Requested job type not in JobsDashboard Type"
    newJob' _ = do
        if requestMethod request == methodPost
            then do
                insertJob
                setSuccessMessage (columnNameToFieldLabel (param "tableName") <> " job started.")
                redirectTo ListJobsAction
            else render $ HtmlView $ renderNewBaseJobForm (param "tableName")
        where
            insertJob = do
                let tableName = param "tableName"
                sqlExecHasql getHasqlPool (Snippet.sql "INSERT INTO " <> sqlIdentifier tableName <> Snippet.sql " DEFAULT VALUES")

    deleteJob = error "deleteJob: Requested job type not in JobsDashboard Type"
    deleteJob' _ = do
        let id    :: UUID = param "id"
            table :: Text = param "tableName"
        delete id table
        setSuccessMessage (columnNameToFieldLabel table <> " record deleted.")
        redirectTo ListJobsAction

        where
            delete id table = sqlExecHasql getHasqlPool
                (Snippet.sql "DELETE FROM " <> sqlIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)

    retryJob = error "retryJob: Requested job type not in JobsDashboard Type"
    retryJob' = do
        let id    :: UUID = param "id"
            table :: Text = param "tableName"
        retryJobById table id
        setSuccessMessage (columnNameToFieldLabel table <> " record marked as 'retry'.")
        redirectTo ListJobsAction

        where
            retryJobById table id = sqlExecHasql getHasqlPool
                (Snippet.sql "UPDATE " <> sqlIdentifier table <> Snippet.sql " SET status = 'job_status_retry' WHERE id = " <> Snippet.param id)


-- | Defines the default implementation for a dashboard of a list of job types.
-- We know the current job is a 'DisplayableJob', and we can recurse on the rest of the list to build the rest of the dashboard.
-- You probably don't want to provide custom implementations for these. Read the documentation for each of the functions if
-- you'd like to know how to customize the behavior. They mostly rely on the functions from 'DisplayableJob'.
instance {-# OVERLAPPABLE #-} (DisplayableJob job, JobsDashboard rest) => JobsDashboard (job:rest) where

    -- | Recusively create a list of views that are concatenated together as 'SomeView's to build the dashboard.
    -- To customize, override 'makeDashboardSection' for each job.
    makeDashboard = do
        section <- makeDashboardSection @job
        restSections <- SomeView <$> makeDashboard @rest
        pure $ SomeView (section : [restSections])

    -- | Recursively build list of included table names
    includedJobTables = tableName @job : includedJobTables @rest

    -- | Build the dashboard and render it.
    indexPage = do
        dashboardIncluded <- makeDashboard @(job:rest)
        notIncluded <- getNotIncludedTableNames (includedJobTables @(job:rest))
        baseJobTables <- mapM buildBaseJobTable notIncluded
        render $ dashboardIncluded : baseJobTables

    listJob table = do
        let page = fromMaybe 1 $ param "page"
        page <- makePageView @job page 25
        render page

    listJob' isFirstTime = do
        let table = param "tableName"

        when isFirstTime $ do
            notIncluded <- getNotIncludedTableNames (includedJobTables @(job:rest))
            when (table `elem` notIncluded) (listJob' @'[] False)

        if tableName @job == table
            then listJob @(job:rest) table
            else listJob' @rest False

    -- | View the detail page for the job with a given uuid.
    viewJob _ uuid = do
        let id :: Id job = unsafeCoerce uuid
        j <- fetch id
        view <- makeDetailView @job j
        render view

    -- | For a given "tableName" parameter, try and recurse over the list of types
    -- in order to find a type with the some table name as the parameter.
    -- If one is found, attempt to construct an ID from the "id" parameter,
    -- and render a page using the type's implementation of 'makeDetailView'.
    -- If you want to customize the page, override that function instead.
    viewJob' isFirstTime = do
        let table = param "tableName"

        when isFirstTime $ do
            notIncluded <- getNotIncludedTableNames (includedJobTables @(job:rest))
            when (table `elem` notIncluded) (viewJob' @'[] False)

        if tableName @job == table
            then viewJob @(job:rest) table (param "id")
            else viewJob' @rest False

    -- For POST, create a new job using the job's implementation of 'createNewJob'.
    -- To include other request data and parameters, override that function, not this one.
    -- If it's a GET request, render a new job form with the job's implementation of 'makeNewJobView'.
    -- For customizing this form, override 'makeNewJobView'.
    newJob tableName = do
        if requestMethod request == methodPost
            then do
                createNewJob @job
                setSuccessMessage (columnNameToFieldLabel tableName <> " job started.")
                redirectTo ListJobsAction
            else do
                view <- makeNewJobView @job
                render view

    -- | For a given "tableName" parameter, try and recurse over the list of types
    -- in order to find a type with the some table name as the parameter.
    -- If such a type is found, call newJob.
    newJob' isFirstTime = do
        let table = param "tableName"

        when isFirstTime $ do
            notIncluded <- getNotIncludedTableNames (includedJobTables @(job:rest))
            when (table `elem` notIncluded) (newJob' @'[] False)

        if tableName @job == table
            then newJob @(job:rest) table
            else newJob' @rest False

    -- | Delete job in 'table' with ID 'uuid'.
    deleteJob table uuid = do
        let id :: Id job = unsafeCoerce uuid
        deleteRecordById @job id
        setSuccessMessage (columnNameToFieldLabel table <> " record deleted.")
        redirectTo ListJobsAction

    -- | For a given "tableName" parameter, try and recurse over the list of types
    -- in order to find a type with the some table name as the parameter.
    -- If one is found, delete the record with the given id.
    deleteJob' isFirstTime = do
        let table = param "tableName"

        when isFirstTime $ do
            notIncluded <- getNotIncludedTableNames (includedJobTables @(job:rest))
            when (table `elem` notIncluded) (deleteJob' @'[] False)

        if tableName @job == table
            then deleteJob @(job:rest) table (param "id")
            else deleteJob' @rest False

    retryJob table uuid = do
        let id    :: UUID = param "id"
            table :: Text = param "tableName"
        sqlExecHasql getHasqlPool
            (Snippet.sql "UPDATE " <> sqlIdentifier table <> Snippet.sql " SET status = 'job_status_retry' WHERE id = " <> Snippet.param id)
        setSuccessMessage (columnNameToFieldLabel table <> " record marked as 'retry'.")
        redirectTo ListJobsAction
    retryJob' = do
        let table = param "tableName"

        if tableName @job == table
            then retryJob @(job:rest) table (param "id")
            else retryJob' @rest

jobStatusDecoder :: Decoders.Value JobStatus
jobStatusDecoder = Decoders.enum (Just "public") "job_status" \case
    "job_status_not_started" -> Just JobStatusNotStarted
    "job_status_running" -> Just JobStatusRunning
    "job_status_failed" -> Just JobStatusFailed
    "job_status_timed_out" -> Just JobStatusTimedOut
    "job_status_succeeded" -> Just JobStatusSucceeded
    "job_status_retry" -> Just JobStatusRetry
    _ -> Nothing

baseJobDecoder :: Decoders.Row BaseJob
baseJobDecoder = BaseJob
    <$> Decoders.column (Decoders.nonNullable Decoders.text)        -- table
    <*> Decoders.column (Decoders.nonNullable Decoders.uuid)        -- id
    <*> Decoders.column (Decoders.nonNullable jobStatusDecoder)     -- status
    <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz) -- updatedAt
    <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz) -- createdAt
    <*> Decoders.column (Decoders.nullable Decoders.text)           -- lastError

getNotIncludedTableNames :: (?modelContext :: ModelContext) => [Text] -> IO [Text]
getNotIncludedTableNames includedNames = sqlQueryHasql getHasqlPool
    (Snippet.sql "SELECT table_name FROM information_schema.tables WHERE table_name LIKE '%_jobs' AND NOT (table_name = ANY(" <> Snippet.param includedNames <> Snippet.sql "))")
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

buildBaseJobTable :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?request :: Wai.Request) => Text -> IO SomeView
buildBaseJobTable tableName = do
    baseJobs <- sqlQueryHasql getHasqlPool
        (Snippet.sql "SELECT " <> Snippet.param tableName <> Snippet.sql ", id, status, updated_at, created_at, last_error FROM "
            <> sqlIdentifier tableName <> Snippet.sql " ORDER BY created_at DESC LIMIT 10")
        (Decoders.rowList baseJobDecoder)
    baseJobs
        |> renderBaseJobTable tableName
        |> HtmlView
        |> SomeView
        |> pure

buildBaseJob :: forall job. (DisplayableJob job) => job -> BaseJob
buildBaseJob job = BaseJob
    (tableName @job)
    (unsafeCoerce $ job.id) -- model Id type -> UUID. Pls don't use integer IDs for your jobs :)
    (job.status)
    (job.updatedAt)
    (job.createdAt)
    (job.lastError)


-- | We can't always access the type of our job in order to use type application syntax for 'tableName'.
-- This is just a convinence function for those cases.
getTableName :: forall job. (DisplayableJob job) => job -> Text
getTableName _ = tableName @job

-- | Get the job with in the given table with the given ID as a 'BaseJob'.
queryBaseJob :: (?modelContext :: ModelContext) => Text -> UUID -> IO BaseJob
queryBaseJob table id = sqlQueryHasql getHasqlPool
    (Snippet.sql "SELECT " <> Snippet.param table <> Snippet.sql ", id, status, updated_at, created_at, last_error FROM "
        <> sqlIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)
    (Decoders.singleRow baseJobDecoder)

queryBaseJobsFromTablePaginated :: (?modelContext :: ModelContext) => Text -> Int -> Int -> IO [BaseJob]
queryBaseJobsFromTablePaginated table page pageSize = sqlQueryHasql getHasqlPool
    (Snippet.sql "SELECT " <> Snippet.param table <> Snippet.sql ", id, status, updated_at, created_at, last_error FROM "
        <> sqlIdentifier table <> Snippet.sql " OFFSET " <> Snippet.param (page * pageSize) <> Snippet.sql " LIMIT " <> Snippet.param pageSize)
    (Decoders.rowList baseJobDecoder)

instance (JobsDashboard jobs, AuthenticationMethod authType) => Controller (JobsDashboardController authType jobs) where
    beforeAction = authenticate @authType
    action ListJobsAction   = autoRefresh $ indexPage @jobs
    action ListJobAction'   = autoRefresh $ listJob' @jobs True
    action ViewJobAction'   = autoRefresh $ viewJob' @jobs True
    action CreateJobAction' = newJob' @jobs True
    action DeleteJobAction' = deleteJob' @jobs True
    action RetryJobAction'  = retryJob' @jobs
    action _ = error "Cannot call this action directly. Call the backtick function with no parameters instead."
