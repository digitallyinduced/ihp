# View

```toc
```

## Introduction

IHP views are usually represented as HTML, but can also be represented as json or other formats.

The html templating is implemented on top of the well-known blaze-html Haskell library. To quickly build html views, IHP supports a JSX-like syntax called HSX. HSX is type-checked and compiled to Haskell code at compile-time.

To render a view, it has to be provided with a custom data structure called a ViewContext. A ViewContext provides the view with information it might need to render, without always explicitly passing it. This is usually used to pass e.g. the current http request, current logged in user, flash messages, the layout, etc..

Usually a view consist of a data structure and a `View` instance. E.g. like this:

```haskell
data ExampleView = ExampleView { optionA :: Text, optionB :: Bool }

instance View ExampleView ViewContext where
    html ExampleView { .. } = [hsx|Hello World {optionA}!|]
```

## Layouts

By default when rendering a HTML view, IHP uses the default application layout to render your view. It's defined at `defaultLayout` in `Web.View.Layout`.

A layout is just a function taking a view and returning a new view:

```haskell
type Layout = Html -> Html
```

### Adding a new layout

To add a new layout, add a new function to the `Web.View.Layout`:

```haskell
appLayout :: Layout
appLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    <title>My App</title>
</head>
<body>
    <h1>Welcome to my app</h1>
    {inner}
</body>
|]
```

Now add `appLayout` to the export list of the module header:

```haskell
module Web.View.Layout (defaultLayout, appLayout) where
```

### Using a layout

To use the layout inside a view, set the layout attribute of the view context inside your view:

```haskell
instance View MyView ViewContext where
    beforeRender (context, view) = (context { layout = appLayout }, view)
```

### Changing the default layout

You can change the default layout of your application by updating `createViewContext` in `Web.View.Context`.

```haskell
instance ViewSupport.CreateViewContext ViewContext where
    -- ...
    createViewContext = do
        -- ...
        let viewContext = ViewContext {
                -- ...
                layout = let ?viewContext = viewContext in appLayout
            }
        pure viewContext
```

### Disabling the Layout for a View

You can disable the layout for a specific view by overriding the `beforeRender` function like this:

```haskell
instance View MyView ViewContext where
    beforeRender (context, view) = (context { layout = \view -> view }, view)

    -- ...
```

## Common View Tasks

### Accessing the Request

Use `theRequest` to access the current [WAI request](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html).

### Highlighting the current active link

Use [`isActivePath`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActivePath) to check whether the current request url matches a given action.

```haskell
<a href={ShowProjectAction} class={classes ["nav-link", ("active", isActivePath ShowProjectAction)]}>
    Show Project
</a>
```

### Check whether this view is called from a specific controller

Use [`isActiveController`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActiveController).

#### `timeAgo`

```haskell
timeAgo (get #createdAt post) -- "1 minute ago"
```

#### `dateTime`

```haskell
dateTime (get #createdAt post) -- "10.6.2019, 15:58"
```

## Diff-Based DOM Updates

When in development, your views will automatically refresh on code changes. This works by re-requesting the view from the server via AJAX and then using [morphdom](https://github.com/patrick-steele-idem/morphdom) to update the visible DOM.

## TurboLinks

In production mode your application is using a custom integration of morphdom and [TurboLinks](https://github.com/turbolinks/turbolinks) together with [InstantClick](http://instantclick.io/). TurboLinks makes navigating the application even faster because it's not doing a full page refresh. We've integrated TurboLinks with morphdom to only update the parts of your HTML that have actually changed. This was inspired by react.js's DOM patch approach and allows for e.g. CSS animations to run on a page transition. Using this makes your app feel like a [SPA](https://en.wikipedia.org/wiki/Single-page_application) without you writing any javascript code.

To improve latency, TurboLinks is configured to prefetch the URL immediately on mouse-hover. Usually the time between a mouse-hover of a link and mouse click is 100ms - 200ms. As long as the server responds in less than 100ms, the response is already there when the click event is fired. This makes your app faster than most single page application (most SPAs still need to fetch some data after clicking).

This setup is designed as a progressive enhancement. Your application is still usable when javascript is disabled.
Even when disabled, your application will still be amazingly fast.

You can disable this behavior by removing the following code from your `Web/Layout.hs`:

```haskell
    when (isProduction FrameworkConfig.environment) [hsx|
            <script src="/vendor/turbolinks.js"></script>
            <script src="/vendor/morphdom-umd.min.js"></script>
            <script src="/vendor/turbolinksMorphdom.js"></script>
            <script src="/vendor/turbolinksInstantClick.js"></script>
        |]
```
