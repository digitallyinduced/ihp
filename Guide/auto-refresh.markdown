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

HTMX endpoints often render just a fragment and swap it into an existing container. Auto Refresh can cooperate with that flow as long as the client knows which element to morph and the fragment exposes the session meta data. You can use multiple Auto Refresh-powered HTMX fragments on one page as long as each swap target has its own stable `id`.

Auto Refresh decides which DOM node to update by looking at a target selector stored on the meta tag:

- If the meta tag has `data-ihp-auto-refresh-target`, that selector is used.
- Otherwise, after an HTMX swap, the client uses the swap target `id` (from `htmx:afterSwap`) and treats it as `#id`.
- If neither is available, Auto Refresh falls back to the full page, which is usually not what you want for fragments.

In practice:

1. Wrap the HTMX action in `autoRefresh`.
2. Include `{autoRefreshMeta}` inside the fragment that HTMX swaps in, or omit it and let Auto Refresh inject it automatically. The meta tag can be anywhere in the fragment; the client moves it into `<head>` after the swap.
3. Give the swap target a stable `id` so Auto Refresh can infer `#id`. If the target has no `id`, Auto Refresh will generate one in the browser (e.g. `ihp-auto-refresh-target-1`). If you want a different selector, set it explicitly with [`setAutoRefreshTarget`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:setAutoRefreshTarget).
4. Keep the container stable (e.g. the same `id`) so morphdom can update its children without losing your `hx-*` attributes.

#### Example 1: Basic fragment swap (no setAutoRefreshTarget)

```haskell
-- Controller
action RefineChatPaneAction { chatId } = autoRefresh do
    messages <- query @Message
        |> filterWhere (#chatId, chatId)
        |> orderByDesc #createdAt
        |> fetch
    render RefineChatPaneView { .. }

-- View
instance View RefineChatPaneView where
    html RefineChatPaneView { .. } = [hsx|
        {autoRefreshMeta}
        {forEach messages renderMessage}
    |]
```

On the page you can keep your skeleton loader and HTMX setup. Because HTMX swaps into `<div id="chat-pane">`, the `htmx:afterSwap` handler derives the target selector `#chat-pane` automatically:

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

After HTMX swaps in the fragment, the Auto Refresh client moves the meta tag into `<head>`, reuses the session id, reconnects the WebSocket, and limits updates to `#chat-pane`. Avoid rendering another `#chat-pane` inside the fragment when using `hx-swap="innerHTML"`, or you will end up with duplicate `id` values.

#### Example 2: No `id` on the swap target (use setAutoRefreshTarget)

If the HTMX target is selected by class or some other selector, Auto Refresh cannot infer the target. Set it explicitly:

```haskell
-- Controller
action SidebarAction = autoRefresh do
    setAutoRefreshTarget ".sidebar-pane"
    items <- query @Item |> fetch
    render SidebarView { .. }

-- View
instance View SidebarView where
    html SidebarView { .. } = [hsx|
        {autoRefreshMeta}
        {forEach items renderItem}
    |]
```

```haskell
[hsx|
<aside
    class="sidebar-pane"
    hx-get={pathTo SidebarAction}
    hx-trigger="load once"
    hx-swap="innerHTML"
></aside>
|]
```

#### Example 3: Outer swap (fragment includes the container)

If you want the fragment to include the wrapper, use `hx-swap="outerHTML"`:

```haskell
-- View
instance View RefineChatPaneView where
    html RefineChatPaneView { .. } = [hsx|
        {autoRefreshMeta}
        <div id="chat-pane" class="h-full">
            {forEach messages renderMessage}
        </div>
    |]
```

```haskell
[hsx|
<div
    id="chat-pane"
    class="h-full"
    hx-get={pathTo RefineChatPaneAction { chatId }}
    hx-trigger="load once"
    hx-swap="outerHTML"
>
    {skeleton}
</div>
|]
```

#### Example 4: Multiple fragments on one page

Each fragment has its own target `id` and its own Auto Refresh session:

```haskell
[hsx|
<div id="chat-pane" hx-get={pathTo RefineChatPaneAction { chatId }} hx-trigger="load once" hx-swap="innerHTML"></div>
<div id="activity-pane" hx-get={pathTo ActivityPaneAction} hx-trigger="load once" hx-swap="innerHTML"></div>
|]
```

```haskell
-- RefineChatPaneView
[hsx|{autoRefreshMeta}{forEach messages renderMessage}|]

-- ActivityPaneView
[hsx|{autoRefreshMeta}{forEach activities renderActivity}|]
```
