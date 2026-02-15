# Auto Refresh

```toc

```

## Setup

Auto Refresh requires no global configuration. Just make sure these two components are in your layout:

### 1. Add Meta Tag to Layout

In your `Web/View/Layout.hs`, add `{autoRefreshMeta}` inside the `<head>` section:

```haskell
metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    {autoRefreshMeta}
|]
```

### 2. Include Required JavaScript

In your `Web/View/Layout.hs`, ensure these scripts are included (order matters - morphdom must come before ihp-auto-refresh):

```haskell
scripts :: Html
scripts = [hsx|
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <!-- ... other scripts ... -->
    |]
```

Once these two components are in place, you can use `autoRefresh` in your actions — no middleware setup needed. The auto-refresh server is created lazily on first use.

## Introduction

Auto Refresh offers a way to re-render views of your application when the underlying data changes. This is useful when you want your views to always reflect the live database state. Auto Refresh can be an easy replacement for manually polling for changes using AJAX.

**Use Cases:**

1. This is used in Shipnix to display the current deployment status. Whenever e.g. the deployment progress or status changes, the view gets updated automatically.
2. When building a monitoring tool for background job workers. Using auto refresh the view can always represent the current state of all the job queues.
3. When building a small social media site: Automatically display new posts in the feed when they become available.

## How It Works

It's good to have a general understanding of how IHP Auto Refresh works.

Auto Refresh first has to be activated for an action by calling [`autoRefresh`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:autoRefresh). Once activated the framework will automatically track all tables your action is using e.g. in `SELECT * FROM ...` queries. Once the action sends a response IHP will start watching for any kind of `INSERT`, `UPDATE` or `DELETE` statement to all the tables used by your action.

When the page is rendered a small JavaScript function will connect back to the IHP server using a WebSocket connection.

Whenever an `INSERT`, `UPDATE` or `DELETE` happens to the tables used by your action IHP will rerun your action on the server-side. When the generated HTML looks different than the HTML generated on the initial page load it will send the new HTML to the browser using the WebSocket connection. The JavaScript listening on the WebSocket will use the new HTML to update the current page. It uses morphdom to only touch the parts of your current DOM that have changed.


### Using Auto Refresh

Let's say we have a `ShowProjectAction` like this:

```haskell
action ShowProjectAction { projectId } = do
    project <- fetch projectId
    render ShowView { .. }
```

To enable auto refresh we have to add [`autoRefresh`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:autoRefresh) in front of the `do`:

```haskell
action ShowProjectAction { projectId } = autoRefresh do
    project <- fetch projectId
    render ShowView { .. }
```

That's it. When you open your browser dev tools, you will see that a WebSocket connection has been started when opening the page. When we update the project from a different browser tab, we will see that the page instantly updates to reflect our changes.

## Advanced Auto Refresh

### Auto Refresh Only for Specific Tables

By default IHP tracks all the tables in an action with Auto Refresh enabled.

In scenarios where you're processing a lot of data for a view, but only a small portion needs Auto Refresh, you can enable Auto Refresh only for the specific tables:

```haskell
action MyAction = do -- <-- We don't enable auto refresh at the action start in this case

    -- This part is not tracked by auto refresh, as `autoRefresh` wasn't called yet
    -- Therefore we can do our "expensive" operations here
    expensiveModels <- query @Expensive |> fetch

    autoRefresh do
        -- Inside this block auto refresh is active and all queries here are tracked
        cheap <- query @Cheap |> fetch
        render MyView { expensiveModels, cheap }
```

### Fine-grained Auto Refresh (experimental)

By default `autoRefresh` works on the table level: **any** `INSERT`, `UPDATE` or `DELETE` on a tracked table will wake the
auto refresh session and trigger a server-side re-render.

This is great for simple pages, but it can become expensive when:

- You have many concurrent auto refresh sessions (many open tabs / users)
- You track a high-churn table (e.g. background jobs, logs, metrics, audit events)
- Only a small subset of rows can actually affect the rendered HTML (e.g. scoped by `projectId`, `userId`, foreign keys, etc.)

In those cases you can use `autoRefreshWith` and decide, based on the changed rows, whether the page should re-render.
This can significantly reduce unnecessary re-renders and make auto refresh scale better under write-heavy workloads.

`autoRefreshWith` uses row-level notifications and provides helpers like `changesForTable`, `rowFieldNew`, and
`rowFieldOld`. For updates and deletes the payload includes both the old and the new row data, so you can decide based on
what changed.

The change information is only used on the server to decide whether to re-render. It is **not** sent to the browser.

If you want row-level filtering, you can decide on refreshes based on row JSON:

```haskell
action ShowProjectAction { projectId } =
    autoRefreshWith AutoRefreshOptions { shouldRefresh } do
        project <- fetch projectId
        render ShowView { .. }
  where
    shouldRefresh changes =
        let projectChanges = changesForTable "projects" changes
            isTarget change = rowFieldNew @"id" change == Just projectId || rowFieldOld @"id" change == Just projectId
        in pure (any isTarget projectChanges)
```

### Filtering by ids or foreign keys

The change set includes full row JSON for each change,
so you can filter directly on any column without extra SQL.

Example: refresh when any changed project belongs to the current user.

```haskell
action ProjectsAction { userId } =
    autoRefreshWith AutoRefreshOptions { shouldRefresh } do
        projects <- query @Project |> filterWhere (#userId, userId) |> fetch
        render ProjectsView { .. }
  where
    shouldRefresh changes =
        let changedProjects = changesForTable "projects" changes
            belongsToUser change = rowFieldNew @"userId" change == Just userId || rowFieldOld @"userId" change == Just userId
        in pure (any belongsToUser changedProjects)
```

Example: multiple table tracking with mixed checks.

```haskell
action DashboardAction { projectId, userId } =
    autoRefreshWith AutoRefreshOptions { shouldRefresh } do
        project <- fetch projectId
        tasks <- query @Task |> filterWhere (#projectId, projectId) |> fetch
        comments <- query @Comment |> filterWhere (#projectId, projectId) |> fetch
        render DashboardView { .. }
  where
    shouldRefresh changes =
        let projectMatches = any (\change -> rowFieldNew @"id" change == Just projectId || rowFieldOld @"id" change == Just projectId) (changesForTable "projects" changes)
            taskMatches = any (\change -> rowFieldNew @"projectId" change == Just projectId || rowFieldOld @"projectId" change == Just projectId) (changesForTable "tasks" changes)
            commentMatches = any (\change -> rowFieldNew @"projectId" change == Just projectId || rowFieldOld @"projectId" change == Just projectId) (changesForTable "comments" changes)
        in pure (projectMatches || taskMatches || commentMatches)
```

Deletes are passed to `shouldRefresh` like any other change, so you can decide when to re-render.

If you want to check across all tables without filtering by table name:

```haskell
action MyAction { userId } =
    autoRefreshWith AutoRefreshOptions { shouldRefresh } do
        -- ...
        render MyView { .. }
  where
    shouldRefresh changes =
        pure (anyChangeWithField @"userId" userId changes)
```

Tip: Keep `shouldRefresh` fast and avoid extra SQL queries inside it whenever possible.

### Custom SQL Queries with Auto Refresh

Auto Refresh automatically tracks all tables your action is using by hooking itself into the Query Builder and `fetch` functions.

Let's say we're using custom sql query like this:

```haskell
action StatsAction = autoRefresh do
    dailyNewCompanies <- sqlQuery "SELECT date, COUNT(distinct id) AS count FROM (SELECT date_trunc('day', companies.created_at) AS date, id FROM companies) AS companies_with_date GROUP BY date" ()

    pure StatsView { ..}
```

When using this custom query with [`sqlQuery`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:sqlQuery), Auto Refresh is not aware that we're reading from the `companies` table. In this case we need to help out Auto Refresh by calling [`trackTableRead`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:trackTableRead):


```haskell
action StatsAction = autoRefresh do
    dailyNewCompanies <- sqlQuery "SELECT date, COUNT(distinct id) AS count FROM (SELECT date_trunc('day', companies.created_at) AS date, id FROM companies) AS companies_with_date GROUP BY date" ()

    trackTableRead "companies"

    pure StatsView { ..}
```

The [`trackTableRead`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:trackTableRead) marks the table as accessed for Auto Refresh and leads to the table being watched.
