{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module IHP.Job.Dashboard (
    JobsDashboard(..),
    DisplayableJob(..),
    JobsDashboardController(..),
    IncludeWrapper(..),
    SomeView(..),
    EmptyView(..),
    TableViewable(..),
    TableView(..),
    getTableName,
    statusToBadge,
) where

import IHP.Prelude
import Generated.Types
import IHP.ViewPrelude (Html, View, hsx, html)
import IHP.ModelSupport
import IHP.ControllerPrelude
import Unsafe.Coerce
import IHP.RouterPrelude hiding (get, tshow, error, map)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

data JobsDashboardController (jobs :: [*])
    = AllJobsAction
    | ViewJobAction
    | CreateJobAction
    deriving (Show, Eq, Data)

class JobsDashboard (jobs :: [*]) where
    makeDashboard :: (?modelContext :: ModelContext) => IO SomeView
    indexPage :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    viewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    newJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

instance JobsDashboard '[] where
    makeDashboard = pure (SomeView EmptyView)
    indexPage = error "No Index implemented for empty jobs list"
    viewJob = error "viewJob: Requested job type not in JobsDashboard Type"
    newJob = error "newJob: Requested job type not in JobsDashboard Type"

data SomeView = forall a. (View a) => SomeView a
instance View SomeView where
    html (SomeView a) = let ?view = a in IHP.ViewPrelude.html a

instance (View a) => View [a] where
    html [] = [hsx||]
    html (x:xs) =
        let ?view = x in
            let current = IHP.ViewPrelude.html x in
                let ?view = xs in
                    let rest = IHP.ViewPrelude.html xs in
                        [hsx|{current}{rest}|]

data EmptyView = EmptyView
instance View EmptyView where
    html _ = [hsx||]

instance {-# OVERLAPPABLE #-} (DisplayableJob job, JobsDashboard rest) => JobsDashboard (job:rest) where
    makeDashboard = do
        section <- makeSection @job
        restSections <- SomeView <$> makeDashboard @rest
        pure $ SomeView (section : [restSections])

    indexPage = do
        dashboard <- makeDashboard @(job:rest)
        render dashboard

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
    newJob = do
        let table = param "tableName"
        if tableName @job == table
            then do
                createNewJob @job
                redirectToPath "/jobs/ListJobs"
            else do
                newJob @rest

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
    , Typeable job
    ) => DisplayableJob job where

    makeSection :: (?modelContext :: ModelContext) => IO SomeView

    makeDetailView :: (?modelContext :: ModelContext) => job -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ GenericShowView job

    createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    createNewJob = do
        newRecord @job |> create
        pure ()



data TableView = forall a. (TableViewable a) => TableView [a]
instance View TableView where
    html (TableView rows) = renderTable rows


class TableViewable a where
    tableTitle :: Text
    tableHeaders :: [Text]
    renderTableRow :: a -> Html

    renderTable :: [a] -> Html
    renderTable rows =
        let
            title = tableTitle @a
            headers = tableHeaders @a
            renderRow = renderTableRow @a
        in [hsx|
        <div>
            <h3>Job type: {title}</h3>
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


newtype IncludeWrapper (id :: Symbol) job = IncludeWrapper (Include id job)

-- CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
statusToBadge :: JobStatus -> Html
statusToBadge JobStatusSucceeded = [hsx|<span class="badge badge-success">Succeeded</span>|]
statusToBadge JobStatusFailed = [hsx|<span class="badge badge-danger">Failed</span>|]
statusToBadge JobStatusRunning = [hsx|<span class="badge badge-info">Running</span>|]
statusToBadge _ = [hsx|<span class="badge badge-secondary">Other</span>|]

 -- CONTROLLER
instance (JobsDashboard jobs) => Controller (JobsDashboardController jobs) where
    action AllJobsAction = autoRefresh $ do
        indexPage @(jobs)

    action ViewJobAction = autoRefresh $ do
        viewJob @(jobs)

    action CreateJobAction = autoRefresh $ do
        newJob @jobs

data GenericShowView = forall job. (DisplayableJob job) => GenericShowView job

-- VIEW
getTableName :: forall job. (DisplayableJob job) => job -> Text
getTableName _ = tableName @job

instance View GenericShowView where
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


instance HasPath (JobsDashboardController jobs) where
    pathTo _ = error "HasPath not supported for JobsDashboardController."

instance CanRoute (JobsDashboardController jobs) where
    parseRoute' = do
        (string "/jobs/ListJobs" <* endOfInput >> pure AllJobsAction)
        <|> (string "/jobs/ViewJob" <* endOfInput >> pure ViewJobAction)
        <|> (string "/jobs/CreateJob" <* endOfInput >> pure CreateJobAction)

