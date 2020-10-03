# Auto Refresh

```toc
```

## Introduction

Auto Refresh offers a way to re-render views of your application when the underlying data changes. This is useful when you want your views to always reflect the live database state. Auto Refresh can be an easy replacement for manually polling for changes using AJAX.

**Use Cases:**

1. This is used in IHP Cloud to display the current deployment status. Whenever e.g. the deployment progress or status changes, the view automatically gets updated.
2. When building a monitoring tool for background job workers. Using auto refresh the view can always represent the current state of all the job queues.
3. When building a small social media site: Automatically display new posts in the feed when they become available. 

## How It Works

It's good to have a general understanding of how IHP Auto Refresh works.

Auto Refresh first have to be activated for an action by calling `autoRefresh`. Once activated the framework will automatically track all tables your action is using e.g. in `SELECT * FROM ...` queries. Once the action sends a response IHP will start watching for any kind of `INSERT`, `UPDATE` or `DELETE` statement to all the tables used by your action.

When the page is rendered a small javascript function will connect back to the IHP server using a WebSocket connection.

Whenenver a `INSERT`, `UPDATE` or `DELETE` happens to the tables used by your action IHP will rerun your action on the serverside. When the generated html looks different to the html generated on the initial page load it will send the new html to the browser using the WebSocket connection. The javascript listening on the WebSocket will use the new html to update the current page. It uses morphdom to only touch the parts of your current DOM that have actually changed.

## Enabling Auto Refresh

As this feature is pretty new it currently needs some manual setup.

### FrontController

First you need to add it to your `Web/FrontController.hs`:

```haskell
module Web.FrontController where

import IHP.AutoRefresh -- <------ ADD THIS IMPORT

instance InitControllerContext WebApplication where
    initContext = initAutoRefresh -- <----- ADD THIS LINE


-- When you use authentication use this:
instance InitControllerContext WebApplication where
    initContext =
        initAuthentication @User
        >=> initAutoRefresh -- <----- ADD THIS LINE
```

### ViewContext

After that we also need to extend our apps view context. This is required to pass an request identifier to the view, so that auto refresh knows which request it's dealing with.

Extend your `ViewContext` in `Web/Types.hs`:

```haskell
module Web.Types where

import IHP.AutoRefresh.Types -- <----- ADD THIS IMPORT

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    -- ...
    , autoRefreshState :: AutoRefreshState -- <-------- ADD THIS LINE
    }
```

We also need to initialize this new field in `Web/View/Context.hs`:

```haskell
module Web.View.Context where

import IHP.AutoRefresh -- <----- ADD THIS IMPORT

instance ViewSupport.CreateViewContext ViewContext where
    type ViewApp ViewContext = WebApplication
    createViewContext = do
        -- ...
        autoRefreshState <- autoRefreshViewContext -- <----- ADD THIS LINE
        -- ...
        let viewContext = ViewContext {
                -- ...
                autoRefreshState -- <-------- ADD THIS LINE
            }
        pure viewContext
```

### Layout

Next we need to add a new meta tag to our layout in `Web/View/Layout.hs`:

```haskell
metaTags :: Html <---- ADD THIS TYPE SIGNATURE
metaTags = [hsx|
    <meta charset="utf-8"/>

    ...

    {autoRefreshMeta} <------ ADD THIS
|]
```

When you get an type error, make sure the type signature `metaTags :: Html` is there.

Additionally you need to include the `/ihp-auto-refresh.js` in your `Web/View/Layout.hs`:

```haskell
scripts = do
    when (isDevelopment FrameworkConfig.environment) [hsx|
        ...
        <script src="/ihp-auto-refresh.js"></script> <------ ADD THIS
    |]
```

### Using Auto Refresh

Let's say we have a `ShowProjectAction` like this:

```haskell
action ShowProjectAction { projectId } = do
    project <- fetch projectId
    render ShowView { .. }
```

To enable auto refresh we have to add `autoRefresh` in front of the `do`:

```haskell
action ShowProjectAction { projectId } = autoRefresh do
    project <- fetch projectId
    render ShowView { .. }
```

That's it. When you open your browsers dev tools, you will see that a WebSocket connection has been started when opening the page. When we update the project from a different browser tab, we will see that the page instantly updates to reflect our changes.
