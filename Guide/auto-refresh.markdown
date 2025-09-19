# Auto Refresh

```toc

```

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

### Using Auto Refresh with HTMX

HTMX endpoints often render just a fragment and swap it into an existing container. Auto Refresh can cooperate with that flow as long as the client knows which element to morph and the fragment exposes the session meta data.

1. Wrap the HTMX action in `autoRefresh` and select the container you want to keep in sync using [`setAutoRefreshTarget`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:setAutoRefreshTarget).
2. Include `{autoRefreshMeta}` somewhere inside the fragment. The meta tag does not need to live in `<head>`—the Auto Refresh script will pick it up after the HTMX swap.
3. Keep the container stable (e.g. the same `id`) so morphdom can update its children without losing your `hx-*` attributes.

```haskell
-- Controller
action RefineChatPaneAction { chatId } = autoRefresh do
    setAutoRefreshTarget "#chat-pane"
    messages <- query @Message
        |> filterWhere (#chatId, chatId)
        |> orderByDesc #createdAt
        |> fetch
    render RefineChatPaneView { .. }

-- View
instance View RefineChatPaneView where
    html RefineChatPaneView { .. } = [hsx|
        {autoRefreshMeta}
        <div id="chat-pane" class="h-full">
            {forEach messages renderMessage}
        </div>
    |]
```

On the page you can keep your skeleton loader and HTMX setup:

```haskell
[hsx|
<div
    id="chat-pane"
    class="h-full"
    hx-get={pathTo RefineChatPaneAction { chatId }}
    hx-trigger="load once"
    hx-swap="innerHTML"
>
    {skeleton}
</div>
|]
```

After HTMX swaps in the fragment, the Auto Refresh client reuses the session id, reconnects the WebSocket, and limits updates to `#chat-pane`, so the skeleton no longer flashes when the backing data changes.
