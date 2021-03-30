{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

{-|
Module: IHP.Job.Dashboard.View
Description:  Views for Job dashboard
-}
module IHP.Job.Dashboard.View where

import IHP.Prelude
import IHP.ViewPrelude (Html, View, hsx, html, timeAgo, columnNameToFieldLabel, JobStatus(..))
import qualified Data.List as List
import IHP.Job.Dashboard.Types

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
                <h3>{title}</h3>
                {newForm}
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
            <a href={ListJobsAction} class="link-primary">See all {title}</a>
            <hr />
        </div>
    |]
        where renderHeader field = [hsx|<th>{field}</th>|]

data TableView a = TableView [a]

-- A TableView is only a View if it's contents are TableViewable.
instance forall a. (TableViewable a) => View (TableView a) where
    html (TableView rows) = renderTable rows

statusToBadge :: JobStatus -> Html
statusToBadge JobStatusNotStarted= [hsx|<span class="badge badge-info">Not Started</span>|]
statusToBadge JobStatusRunning = [hsx|<span class="badge badge-info">Running</span>|]
statusToBadge JobStatusSucceeded = [hsx|<span class="badge badge-success">Succeeded</span>|]
statusToBadge JobStatusFailed = [hsx|<span class="badge badge-danger">Failed</span>|]
statusToBadge JobStatusRetry = [hsx|<span class="badge badge-info">Retrying</span>|]

renderBaseJobTableRow :: BaseJob -> Html
renderBaseJobTableRow job = let
        btnStyle :: Text = "outline: none !important; padding: 0; border: 0; vertical-align: baseline;"
    in [hsx|
        <tr>
            <td>{get #id job}</td>
            <td>{get #updatedAt job}</td>
            <td>{statusToBadge $ get #status job}</td>
            <td><a href={ViewJobAction (get #table job) (get #id job)} class="text-primary">Show</a></td>
            <td>
                <form action={CreateJobAction (get #table job)} method="POST">
                    <button type="submit" style={btnStyle} class="btn btn-link text-secondary">Retry</button>
                </form>
            </td>
        </tr>
    |]

renderBaseJobTable :: Text -> [BaseJob] -> Html
renderBaseJobTable table rows =
    let
        headers :: [Text] = ["ID", "Updated At", "Status", "", ""]
    in [hsx|
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
                {forEach rows renderBaseJobTableRow}
            </tbody>
        </table>
        <a href={ListJobAction table 1} class="link-primary">See all {table}</a>
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
                <td>{get #createdAt job |> timeAgo}</td>
            </tr>
            <tr>
                <th>Status</th>
                <td>{statusToBadge (get #status job)}</td>
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


