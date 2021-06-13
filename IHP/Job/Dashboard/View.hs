{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

{-|
Module: IHP.Job.Dashboard.View
Description:  Views for Job dashboard
-}
module IHP.Job.Dashboard.View where

import IHP.Prelude
import IHP.ViewPrelude (JobStatus(..), ControllerContext, Html, View, hsx, html, timeAgo, columnNameToFieldLabel)
import qualified Data.List as List
import IHP.Job.Dashboard.Types
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import IHP.Job.Dashboard.Utils
import qualified IHP.Log as Log

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

-- | A view constructed from some HTML.
newtype HtmlView = HtmlView Html
instance View HtmlView where
    html (HtmlView html) = [hsx|{html}|]

renderStatus job = case get #status job of
    JobStatusNotStarted -> [hsx|<span class="badge badge-secondary">Not Started</span>|]
    JobStatusRunning -> [hsx|<span class="badge badge-primary">Running</span>|]
    JobStatusFailed -> [hsx|<span class="badge badge-danger" title="Last Error" data-container="body" data-toggle="popover" data-placement="left" data-content={fromMaybe "" (get #lastError job)}>Failed</span>|]
    JobStatusSucceeded -> [hsx|<span class="badge badge-success">Succeeded</span>|]
    JobStatusRetry -> [hsx|<span class="badge badge-warning" title="Last Error" data-container="body" data-toggle="popover" data-placement="left" data-content={fromMaybe "" (get #lastError job)}>Retry</span>|]

-- BASE JOB VIEW HELPERS --------------------------------

renderBaseJobTable :: Text -> [BaseJob] -> Html
renderBaseJobTable table rows =
    let
        headers :: [Text] = ["ID", "Updated At", "Status", "", ""]
        humanTitle = table |> columnNameToFieldLabel
    in [hsx|
    <div>
        <div class="d-flex justify-content-between align-items-center">
            <h3>{humanTitle}</h3>
            {renderNewBaseJobLink table}
        </div>
        <table class="table table-sm table-hover">
            <thead>
                <tr>
                    {forEach headers renderHeader}
                </tr>
            </thead>

            <tbody>
                {forEach rows renderBaseJobTableRow}
            </tbody>
        </table>
        <a href={ListJobAction table 1} class="link-primary">See all {humanTitle}</a>
        <hr />
    </div>
|]
    where renderHeader field = [hsx|<th>{field}</th>|]

renderBaseJobTablePaginated :: Text -> [BaseJob] -> Int -> Int -> Html
renderBaseJobTablePaginated table jobs page totalPages =
    let
        headers :: [Text] = ["ID", "Updated At", "Status", "", ""]
        lastJobIndex = (List.length jobs) - 1
    in
        [hsx|
            <div>
                <div class="d-flex justify-content-between align-items-center">
                    <h3>{table |> columnNameToFieldLabel}</h3>
                    {renderNewBaseJobLink table}
                </div>
                <table class="table table-sm table-hover">
                    <thead>
                        <tr>
                            {forEach headers renderHeader}
                        </tr>
                    </thead>

                    <tbody>
                        {forEach jobs renderBaseJobTableRow}
                    </tbody>
                </table>
            </div>
            <nav aria-label="Page navigation example">
                <ul class="pagination justify-content-end">
                    {renderPrev}
                    {when (totalPages /= 1) renderDest}
                    {renderNext}
                </ul>
            </nav>
        |]
    where
        renderHeader field = [hsx|<th>{field}</th>|]
        renderDest = [hsx|<li class="page-item active"><a class="page-link" href={ListJobAction table page}>{page}</a></li>|]
        renderPrev
            | page == 1 = [hsx||]
            | otherwise = [hsx|
                <li class="page-item">
                    <a class="page-link" href={ListJobAction table (page - 1)} aria-label="Previous">
                        <span aria-hidden="true">&laquo;</span>
                        <span class="sr-only">Previous</span>
                    </a>
                </li>
        |]
        renderNext
            | page == totalPages || totalPages == 0 = [hsx||]
            | otherwise = [hsx|
                <li class="page-item">
                    <a class="page-link" href={ListJobAction table (page + 1)} aria-label="Next">
                        <span aria-hidden="true">&raquo;</span>
                        <span class="sr-only">Next</span>
                    </a>
                </li>
            |]

renderBaseJobTableRow :: BaseJob -> Html
renderBaseJobTableRow job = [hsx|
        <tr>
            <td>{get #id job}</td>
            <td>{get #updatedAt job |> timeAgo}</td>
            <td>{renderStatus job}</td>
            <td><a href={ViewJobAction (get #table job) (get #id job)} class="text-primary">Show</a></td>
            <td>
                <form action={CreateJobAction (get #table job)} method="POST">
                    <button type="submit" style={retryButtonStyle} class="btn btn-link text-secondary">Retry</button>
                </form>
            </td>
        </tr>
    |]

-- | Link included in table to create a new job.
renderNewBaseJobLink :: Text -> Html
renderNewBaseJobLink table =
    let
        link = "/jobs/CreateJob?tableName=" <> table
    in [hsx|
        <form action={link}>
            <button type="submit" class="btn btn-primary btn-sm">+ New Job</button>
        </form>
    |]

renderNewBaseJobForm :: Text -> Html
renderNewBaseJobForm table = [hsx|
    <br>
        <h5>New Job: {table}</h5>
    <br>
    <form action="/jobs/CreateJob" method="POST">
        <input type="hidden" id="tableName" name="tableName" value={table}>
        <button type="submit" class="btn btn-primary">New Job</button>
    </form>
|]

renderBaseJobDetailView :: BaseJob -> Html
renderBaseJobDetailView job = let table = get #table job in [hsx|
    <br>
        <h5>Viewing Job {get #id job} in {table |> columnNameToFieldLabel}</h5>
    <br>
    <table class="table">
        <tbody>
            <tr>
                <th>Updated At</th>
                <td>{get #updatedAt job |> timeAgo} ({get #updatedAt job})</td>
            </tr>
            <tr>
                <th>Created At</th>
                <td>{get #createdAt job |> timeAgo} ({get #createdAt job})</td>
            </tr>
            <tr>
                <th>Status</th>
                <td>{renderStatus job}</td>
            </tr>
            <tr>
                <th>Last Error</th>
                <td>{fromMaybe "No error" (get #lastError job)}</td>
            </tr>
        </tbody>
    </table>

    <div class="d-flex flex-row">
        <form class="mr-2" action="/jobs/DeleteJob" method="POST">
            <input type="hidden" id="tableName" name="tableName" value={table}>
            <input type="hidden" id="id" name="id" value={tshow $ get #id job}>
            <button type="submit" class="btn btn-danger">Delete</button>
        </form>
        <form action="/jobs/CreateJob" method="POST">
            <input type="hidden" id="tableName" name="tableName" value={table}>
            <button type="submit" class="btn btn-primary">Run again</button>
        </form>
    </div>
|]
------------------------------------------------------------------

-- TABLE VIEWABLE view helpers -----------------------------------
makeDashboardSectionFromTableViewable :: forall a. (TableViewable a
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext) => IO SomeView
makeDashboardSectionFromTableViewable = do
    indexRows <- getIndex @a
    pure $ SomeView $ HtmlView $ renderTableViewableTable indexRows

renderTableViewableTable :: forall a. TableViewable a => [a] -> Html
renderTableViewableTable rows = let
        headers = tableHeaders @a
        title = tableTitle @a
        link = newJobLink @a
        renderRow = renderTableRow @a
        table = modelTableName @a
    in [hsx|
    <div>
        <div class="d-flex justify-content-between align-items-center">
            <h3>{title}</h3>
            {link}
        </div>
        <table class="table table-sm table-hover">
            <thead>
                <tr>
                    {forEach headers renderHeader}
                </tr>
            </thead>

            <tbody>
                {forEach rows renderRow}
            </tbody>
        </table>
        <a href={ListJobAction table 1} class="link-primary">See all {title}</a>
        <hr />
    </div>
|]
    where renderHeader field = [hsx|<th>{field}</th>|]



makeListPageFromTableViewable :: forall a. (TableViewable a, ?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO SomeView
makeListPageFromTableViewable page pageSize = do
    pageData <- getPage @a (page - 1) pageSize
    numPages <- numberOfPagesForTable (modelTableName @a) pageSize
    pure $ SomeView $ HtmlView $ renderTableViewableTablePaginated pageData page numPages

renderTableViewableTablePaginated :: forall a. TableViewable a => [a] -> Int -> Int -> Html
renderTableViewableTablePaginated jobs page totalPages =
    let
        title = tableTitle @a
        table = modelTableName @a
        headers = tableHeaders @a
        lastJobIndex = (List.length jobs) - 1
        newLink = newJobLink @a
    in
        [hsx|
            <div>
                <div class="d-flex justify-content-between align-items-center">
                    <h3>{title}</h3>
                    {newLink}
                </div>
                <table class="table table-sm table-hover">
                    <thead>
                        <tr>
                            {forEach headers renderHeader}
                        </tr>
                    </thead>

                    <tbody>
                        {forEach jobs renderTableRow}
                    </tbody>
                </table>
            </div>
            <nav aria-label="Page navigation example">
                <ul class="pagination justify-content-end">
                    {renderPrev}
                    {when (totalPages /= 1) renderDest}
                    {renderNext}
                </ul>
            </nav>
        |]
    where
        renderHeader field = [hsx|<th>{field}</th>|]
        renderDest = let table = modelTableName @a in [hsx|<li class="page-item active"><a class="page-link" href={ListJobAction table page}>{page}</a></li>|]
        renderPrev
            | page == 1 = [hsx||]
            | otherwise = let table = modelTableName @a in [hsx|
                <li class="page-item">
                    <a class="page-link" href={ListJobAction table (page - 1)} aria-label="Previous">
                        <span aria-hidden="true">&laquo;</span>
                        <span class="sr-only">Previous</span>
                    </a>
                </li>
        |]
        renderNext
            | page == totalPages || totalPages == 0 = [hsx||]
            | otherwise = let table = modelTableName @a in [hsx|
                <li class="page-item">
                    <a class="page-link" href={ListJobAction table (page + 1)} aria-label="Next">
                        <span aria-hidden="true">&raquo;</span>
                        <span class="sr-only">Next</span>
                    </a>
                </li>
            |]
------------------------------------------------------------

retryButtonStyle :: Text
retryButtonStyle = "outline: none !important; padding: 0; border: 0; vertical-align: baseline;"
