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

For QueryBuilder queries, Auto Refresh also tracks the primary keys returned by each query and retains the prepared query together with its bound parameters. When the table uses the conventional single-column `id` key, updates and deletes to unrelated rows can therefore be ignored. For inserts, and for updates that might make a row newly match, IHP reuses the captured parameterized query to check only the changed row before re-rendering. Custom and composite primary keys are retained for future relevance checks but currently use the conservative table-level refresh fallback. The captured query snapshot is replaced after every re-render.


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

### Notification Batching

By default Auto Refresh does not add a batching delay. Each PostgreSQL
notification is checked as soon as its listener worker can run, which preserves
the real-time behaviour expected by latency-sensitive pages.

For write-heavy applications with many connected users, you can opt into a
short batching window in `Config/Config.hs`:

```haskell
config :: ConfigBuilder
config = do
    option (AutoRefreshBatchWindow 100)
```

The value is in milliseconds. During that window IHP accumulates changed row
IDs per table, then performs one relevance check per connected session for the
whole batch. Continuous writes use consecutive windows, so changes arriving
while a batch is being processed are not lost.

Set the value to `0` to disable the intentional delay:

```haskell
option (AutoRefreshBatchWindow 0)
```

The default is `0`. You can also set the same option through the
`IHP_AUTO_REFRESH_BATCH_WINDOW_MS` environment variable. A positive window is
useful for high-fan-out dashboards; leave it at zero when minimum update latency
matters more than limiting the maximum relevance-check rate.

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

Manually tracked raw SQL uses the safe table-level fallback, because IHP only knows the table name in that case.

### Typed SQL Queries with Auto Refresh

Queries executed with `sqlQueryTyped [typedSql| ... |]` are tracked automatically. Typed SQL retains the query's bound parameters, so Auto Refresh can apply the same changed-row check as QueryBuilder when the query result directly exposes the table's conventional single-column `id` key:

```haskell
action ProjectTasksAction { projectId } = autoRefresh do
    tasks <- sqlQueryTyped [typedSql|
        SELECT id, title
        FROM tasks
        WHERE project_id = ${projectId}
        ORDER BY created_at
    |]

    render TasksView { .. }
```

If the result does not expose the primary key, or the query reads a table with a composite/non-standard primary key, Auto Refresh still watches the detected table but falls back to refreshing for every change to that table.

The fine-grained path is intended for direct, row-local queries like the example above. Query shapes where one row can change the output of another row (for example `OFFSET`, window functions, correlated subqueries or aggregates) need conservative table-level tracking. Until Typed SQL emits explicit safety metadata for these shapes, call `trackTableRead "tasks"` in the same `autoRefresh` action to force that fallback.
