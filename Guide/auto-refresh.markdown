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

### Active Session Snapshots for Custom Clients

Custom clients that manage several Auto Refresh views can send the session IDs currently present in their DOM. Send the concatenated UUID strings, without separators, in the `X-IHP-Auto-Refresh-Sessions` HTTP header. Browser WebSocket clients can send the same value in the `autoRefreshSessions` query parameter because the WebSocket API does not support custom headers.

For example, a client can collect IDs from the rendered meta tags before an HTTP request:

```javascript
const sessionIds = Array.from(document.querySelectorAll('meta[property="ihp-auto-refresh-id"]'))
    .map(meta => meta.content)
    .join('');

fetch('/YourAutoRefreshAction', {
    headers: { 'X-IHP-Auto-Refresh-Sessions': sessionIds }
});
```

For a WebSocket, append `?autoRefreshSessions=` followed by `encodeURIComponent(sessionIds)` to `/AutoRefreshWSApp`, then send the individual session ID on the socket as usual.

A non-empty explicit snapshot replaces the older cookie snapshot. This prevents discarded responses from accumulating stale session IDs in the encrypted cookie until server garbage collection runs. It also lets a client reconnect to a rendered session whose cookie entry was overwritten by a concurrent response. Duplicate IDs are removed, and malformed IDs or IDs no longer present on the server are ignored. When both explicit values are empty or absent, IHP continues to use the session cookie, so existing clients keep their current behavior. The bundled client continues to use this cookie fallback; custom clients opt in by sending a snapshot.

Explicit session IDs are bearer credentials for the corresponding rendered views: possession of a live ID grants access to its Auto Refresh updates, even if the cookie no longer contains it. Only send IDs obtained from the client's own rendered views, keep them private, and avoid recording the WebSocket query string in shared access logs.

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
