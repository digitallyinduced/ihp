{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

{-|
Module: IHP.Job.Dashboard
Description:  Auto-generate a dashboard for job types
Maintainer: zac.wood@hey.com

This module allows IHP applications to generate a dashboard for interacting with job types.
To generate a dashboard, first define a type for the dashboard containing the jobs you wish to display:

> type MyDashboard = JobsDashboardController [EmailUserJob, UpdateRecordJob, RandomJob]

And include the following in the 'controllers' list of a FrontController:

> parseRoute @MyDashboard

All views are fully customizable. For more info, see the documentation for 'DisplayableJob'.
-}
module IHP.Job.Dashboard (
    JobsDashboard(..),
    DisplayableJob(..),
    JobsDashboardController(..),
    IncludeWrapper(..),
    SomeView(..),
    EmptyView(..),
    TableViewable(..),
    TableView(..),

    -- Auth
    AuthenticationMethod(..),
    NoAuth(..),
    BasicAuth(..),
    BasicAuthStatic(..),

    -- Utility functions
    getTableName,
    statusToBadge,
    newJobFormForTableHeader,
) where

import IHP.Prelude
import IHP.ViewPrelude (Html, View, hsx, html)
import IHP.ModelSupport
import IHP.ControllerPrelude
import Unsafe.Coerce
import IHP.RouterPrelude hiding (get, tshow, error, map)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified IHP.Log as Log
import Network.Wai (requestMethod)
import Network.HTTP.Types.Method (methodGet, methodPost)
import System.Environment (lookupEnv)

-- | Defines one method, 'authenticate', called before every action. Use to authenticate user.
--
-- Three implementations are provided:
-- - 'NoAuth' : No authentication
-- - 'BasicAuth' : HTTP Basic Auth using environment variables
-- - 'BasicAuthStatic' : HTTP Basic Auth using static values
--
-- Define your own implementation to use custom authentication for production.
class AuthenticationMethod a where
    authenticate :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()

-- | Don't use any authentication for jobs.
data NoAuth

-- | Authenticate using HTTP Basic Authentication by looking up username/password values
-- in environment variables given as type-level strings.
data BasicAuth (userEnv :: Symbol) (passEnv :: Symbol)

-- | Authenticate using HTTP Basic Authentication using username/password given as type level strings.
-- Meant for development only!
data BasicAuthStatic (user :: Symbol) (pass :: Symbol)

instance AuthenticationMethod NoAuth where
    authenticate = pure ()

instance (KnownSymbol userEnv, KnownSymbol passEnv) => AuthenticationMethod (BasicAuth userEnv passEnv) where
    authenticate = do
        creds <- (,) <$> lookupEnv (symbolVal $ Proxy @userEnv) <*> lookupEnv (symbolVal $ Proxy @passEnv)
        case creds of
            (Just user, Just pass) -> basicAuth (cs user) (cs pass) "jobs"
            _ -> error "Did not find HTTP Basic Auth credentials for Jobs Dashboard."

instance (KnownSymbol user, KnownSymbol pass) => AuthenticationMethod (BasicAuthStatic user pass) where
    authenticate = basicAuth (cs $ symbolVal $ Proxy @user) (cs $ symbolVal $ Proxy @pass) "jobs"

-- | Defines controller actions for acting on a dashboard made of some list of types.
-- Later functions and typeclasses introduce constraints on the types in this list,
-- so you'll get a compile error if you try and include a type that is not a job.
data JobsDashboardController authType (jobs :: [*])
    = AllJobsAction
    | ViewJobAction
    | CreateJobAction
    deriving (Show, Eq, Data)

-- | Defines implementations for actions for acting on a dashboard made of some list of types.
-- This is included to allow these actions to recurse on the types, isn't possible in an IHP Controller
-- action implementation.
--
-- Later functions and typeclasses introduce constraints on the types in this list,
-- so you'll get a compile error if you try and include a type that is not a job.
class JobsDashboard (jobs :: [*]) where
    -- | Creates the entire dashboard by recursing on the type list and calling 'makeSection' on each type.
    makeDashboard :: (?context::ControllerContext, ?modelContext :: ModelContext) => IO SomeView

    -- | Renders the index page, which is the view returned from 'makeDashboard'.
    indexPage :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

    -- | Renders the detail view page. Rescurses on the type list to find a type with the
    -- same table name as the "tableName" query parameter.
    viewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

    -- | If performed in a POST request, creates a new job depending on the "tableName" query parameter.
    -- If performed in a GET request, renders the new job from depending on said parameter.
    newJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

-- | The only function that should ever be invoked for this class is 'makeDashboard'
-- which ends the recursion the build the index view. If other cases are reached,
-- that means the type passed in "tableName" was not found and thus an error is thrown.
instance JobsDashboard '[] where
    makeDashboard = pure (SomeView EmptyView)
    indexPage = error "No Index implemented for empty jobs list"
    viewJob = error "viewJob: Requested job type not in JobsDashboard Type"
    newJob = error "newJob: Requested job type not in JobsDashboard Type"

-- | Provides a type-erased view. This allows us to specify a view as a return type without needed
-- to know exactly what type the view will be, which in turn allows for custom implmentations of
-- almost all the view functions in this module. Go GADTs!
data SomeView where
    SomeView :: forall a. (View a) => a -> SomeView

-- | Since the only constructor for 'SomeView' requires that it is passed a 'View', we can use
-- that to implement a 'View' instance for 'SomeView'
instance View SomeView where
    html (SomeView a) = let ?view = a in IHP.ViewPrelude.html a

-- | Define how to render a list of views as a view. Just concatenate them together!
instance (View a) => View [a] where
    html [] = [hsx||]
    html (x:xs) =
        -- need to nest let's here in order to satisfy the implicit ?view parameter for 'html'.
        -- ?view needs to be the type of the view being rendered, so set it before each render
        -- here we render single view
        let ?view = x in
            let current = IHP.ViewPrelude.html x in
                -- now rendering a list view
                let ?view = xs in
                    let rest = IHP.ViewPrelude.html xs in
                        [hsx|{current}{rest}|]

-- | A view containing no data. Used occasionally as a default implementation for some functions.
data EmptyView = EmptyView
instance View EmptyView where
    html _ = [hsx||]

-- | Defines the default implementation for a dashboard of a list of job types.
-- We know the current job is a 'DisplayableJob', and we can recurse on the rest of the list to build the rest of the dashboard.
-- You probably don't want to provide custom implementations for these. Read the documentation for each of the functions if
-- you'd like to know how to customize the behavior. They mostly rely on the functions from 'DisplayableJob'.
instance {-# OVERLAPPABLE #-} (DisplayableJob job, JobsDashboard rest) => JobsDashboard (job:rest) where

    -- | Recusively create a list of views that are concatenated together as 'SomeView's to build the dashboard.
    -- To customize, override 'makeSection' for each job.
    makeDashboard = do
        section <- makeSection @job
        restSections <- SomeView <$> makeDashboard @rest
        pure $ SomeView (section : [restSections])

    -- | Build the dashboard and render it.
    indexPage = do
        dashboard <- makeDashboard @(job:rest)
        render dashboard

    -- | For a given "tableName" parameter, try and recurse over the list of types
    -- in order to find a type with the some table name as the parameter.
    -- If one is found, attempt to construct an ID from the "id" parameter,
    -- and render a page using the type's implementation of 'makeDetailView'.
    -- If you want to customize the page, override that function instead.
    viewJob = do
        let table = param "tableName"
        if tableName @job == table
            then do
                let id :: Id job = unsafeCoerce (param "id" :: UUID) -- TODO: safe cast?
                j <- fetch id
                view <- makeDetailView @job j
                render view
            else do
                viewJob @rest

    -- | For a given "tableName" parameter, try and recurse over the list of types
    -- in order to find a type with the some table name as the parameter.
    -- If one is found, and the request is POST, create a new job using the job's implementation of 'createNewJob'.
    -- To include other request data and parameters, override that function, not this one.
    -- If it's a GET request, render a new job form with the job's implementation of 'makeNewJobView'.
    -- For customizing this form, override 'makeNewJobView'.
    newJob = do
        let table = param "tableName"
        if tableName @job == table
            then do
                if requestMethod request == methodPost
                    then do
                        createNewJob @job
                        redirectToPath "/jobs/ListJobs"
                    else do
                        view <- makeNewJobView @job
                        render view
            else do
                newJob @rest

-- | The crazy list of type constraints for this class defines everything needed for a generic "Job".
-- All jobs created through the IHP dev IDE will automatically satisfy these constraints and thus be able to
-- be used as a 'DisplayableJob'.
-- To customize the dashboard behavior for each job, you should provide a custom implementation of 'DisplayableJob'
-- for your job type. Your custom implementations will then be used instead of the defaults.
class ( job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
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
    , Typeable job) => DisplayableJob job where

    -- | How this job's section should be displayed in the dashboard. By default it's displayed as a table,
    -- but this can be any arbitrary view! Make some cool graphs :)
    makeSection :: (?modelContext :: ModelContext) => IO SomeView

    -- | The content of the page that will be displayed for a detail view of this job.
    -- By default, the ID, Status, Created/Updated at times, and last error are displayed.
    -- Can be defined as any arbitrary view.
    makeDetailView :: (?modelContext :: ModelContext) => job -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ GenericShowView job

    -- | The content of the page that will be displayed for the "new job" form of this job.
    -- By default, only the submit button is rendered. For additonal form data, define your own implementation.
    -- See 'GenericNewJobView' for a guide on how it can look.look
    -- Can be defined as any arbitrary view, but it should be a form.
    makeNewJobView :: (?modelContext :: ModelContext) => IO SomeView
    makeNewJobView = pure $ SomeView $ GenericNewJobView @job

    -- | The action run to create and insert a new value of this job into the database.
    -- By default, create an empty record and insert it.
    -- To add more data, define your own implementation.
    createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    createNewJob = do
        newRecord @job |> create
        pure ()



data TableView a = TableView [a]

-- A TableView is only a View if it's contents are TableViewable.
instance forall a. (TableViewable a) => View (TableView a) where
    html (TableView rows) = renderTable rows

-- | Creates a form with a single small button that links to the Create Job form for some 'DisplayableJob;.
newJobFormForTableHeader :: forall job. (DisplayableJob job) => Html
newJobFormForTableHeader =
        let
            t = tableName @job
            link :: Text = "/jobs/CreateJob?tableName=" <> t
        in [hsx|
            <form action={link}>
                <button type="submit" class="btn btn-primary btn-sm">+ New Job</button>
            </form>
        |]

-- | Defines a set of necessary functions to display some data 'a' in a table format given a list of '[a]'.
class TableViewable a where
    tableTitle :: Text

    -- | Headers that describe each column in the table
    tableHeaders :: [Text]

    -- | Optional HTML that will be placed in the top right of the table. Used for a "create new" button.
    createNewForm :: Html
    createNewForm = [hsx||]

    renderTableRow :: a -> Html

    renderTable :: [a] -> Html
    renderTable rows =
        let
            title = tableTitle @a
            headers = tableHeaders @a
            renderRow = renderTableRow @a
            newForm = createNewForm @a
        in [hsx|
        <div>
            <div class="d-flex justify-content-between align-items-center">
                <h3>Job type: {title}</h3>
                {newForm}
            </div>
            <table class="table table-sm">
                <thead>
                    <tr>
                        {forEach headers renderHeader}
                    </tr>
                </thead>

                <tbody>
                    {forEach rows renderRow}
                </tbody>
            </table>
        </div>
    |]
        where renderHeader field = [hsx|<th>{field}</th>|]

-- | The crazy list of type constraints for this instance defines everything needed for a generic "Job".
-- Given all the constraints are satisfied, we can define how this job type can be used to display in a table format.
instance {-# OVERLAPPABLE #-} (job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
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
    , Typeable job
    ) => TableViewable job where

    tableTitle = tableName @job
    tableHeaders = ["ID", "Updated at", "Status", ""]
    createNewForm = newJobFormForTableHeader @job
    renderTableRow job =
        let
            table = tableName @job
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
            <tr>
                <td>{get #id job}</td>
                <td>{get #updatedAt job}</td>
                <td>{statusToBadge $ get #status job}</td>
                <td><a href={linkToView} class="text-primary">Show</a></td>
            </tr>
        |]

-- | Given a generic job that is able to be displayed in a table through the 'TableViewable' constraint,
-- define the functions needed for a 'DisplayableJob'.
instance {-# OVERLAPPABLE #-} (job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
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
    , Typeable job
    , TableViewable job
    ) => DisplayableJob job where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        jobs <- query @job |> fetch
        pure $ SomeView (TableView jobs)


-- | Often, jobs are related to some model type. These relations are modeled through the type system.
-- For example, the type 'Include "userId" UpdateUserJob' models an 'UpdateUserJob' type that can access
-- the 'User' it belongs to through the 'userId' field.
-- For some reason, GHC doesn't allow us to create implementations of type family applications, so the following doesn't work:
--
-- > instance DisplayableJob (Include "userId" UpdateUserJob) where
--
-- However, if we wrap this in a concrete type, it works fine. That's what this wrapper is for.
-- To get the same behavior as above, just define
--
-- > instance DisplayableJob (IncludeWrapper "userId" UpdateUserJob) where
--
-- and wrap the values as so:
--
-- > jobsWithUsers <- query @UpdateUserJob
-- >    |> fetch
-- >    >>= mapM (fetchRelated #userId)
-- >    >>= pure . map (IncludeWrapper @"userId" @UpdateUserJob)
newtype IncludeWrapper (id :: Symbol) job = IncludeWrapper (Include id job)

-- CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
statusToBadge :: JobStatus -> Html
statusToBadge JobStatusNotStarted= [hsx|<span class="badge badge-info">Not Started</span>|]
statusToBadge JobStatusRunning = [hsx|<span class="badge badge-info">Running</span>|]
statusToBadge JobStatusSucceeded = [hsx|<span class="badge badge-success">Succeeded</span>|]
statusToBadge JobStatusFailed = [hsx|<span class="badge badge-danger">Failed</span>|]
statusToBadge JobStatusRetry = [hsx|<span class="badge badge-info">Retrying</span>|]

instance (JobsDashboard jobs, AuthenticationMethod authType) => Controller (JobsDashboardController authType jobs) where
    beforeAction = do
        Log.info "Attempting to authenticate."
        authenticate @authType

    action AllJobsAction = autoRefresh $ do
        indexPage @(jobs)

    action ViewJobAction = autoRefresh $ do
        viewJob @(jobs)

    action CreateJobAction = autoRefresh $ do
        newJob @jobs

-- | We can't always access the type of our job in order to use type application syntax for 'tableName'.
-- This is just a convinence function for those cases.
getTableName :: forall job. (DisplayableJob job) => job -> Text
getTableName _ = tableName @job

data GenericShowView job = GenericShowView job
instance (DisplayableJob job) => View (GenericShowView job) where
    html (GenericShowView job) =
        let
            table = getTableName job
        in [hsx|
            <br>
                <h5>Viewing Job {get #id job} in {table}</h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>

            <form action="/jobs/CreateJob" method="POST">
                <input type="hidden" id="tableName" name="tableName" value={table}>
                <button type="submit" class="btn btn-primary">Run again</button>
            </form>
        |]


data GenericNewJobView job = GenericNewJobView
instance (DisplayableJob job) => View (GenericNewJobView job) where
    html _ =
        let
            table = tableName @job
        in [hsx|
            <br>
                <h5>New Job: {table}</h5>
            <br>
            <form action="/jobs/CreateJob" method="POST">
                <input type="hidden" id="tableName" name="tableName" value={table}>
                <button type="submit" class="btn btn-primary">New Job</button>
            </form>
        |]

instance HasPath (JobsDashboardController authType jobs) where
    pathTo _ = error "HasPath not supported for JobsDashboardController."

instance CanRoute (JobsDashboardController authType jobs) where
    parseRoute' = do
        (string "/jobs/ListJobs" <* endOfInput >> pure AllJobsAction)
        <|> (string "/jobs/ViewJob" <* endOfInput >> pure ViewJobAction)
        <|> (string "/jobs/CreateJob" <* endOfInput >> pure CreateJobAction)

