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

### Customizing Delete Confirmation

By default a message `Are you sure you want to delete this?` is shown as a simple confirmation alert with yes/no choices. The message text can be customized.

```haskell
<a href={DeleteToolAction (get #id tool)} class="js-delete" data-confirm="Deleting a tool will also delete all usage of the tool. Continue?">Delete Tool</a>
```

#### Suppressing Delete Confirmation

```haskell
<a href={DeleteToolAction (get #id tool)} class="js-delete js-delete-no-confirm">Delete Tool</a>
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

## JSON

Views that are rendered by calling the `render` function can also respond with JSON.

Let's say we have a normal HTML view that renders all posts for our blog app:

```haskell
instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PostsAction}>Posts</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPostAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
        </div>
    |]
```

We can add a JSON output for all blog posts by add a `json` function to this:

```haskell
import Data.Aeson -- <--- Add this import at the top of the file

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        ...
    |]

    json IndexView { .. } = toJSON posts -- <---- The new json render function
```

In the above code, our `json` function has access to all arguments passed to the view. Here we call `toJSON`, which is provided [by the aeson haskell library](https://hackage.haskell.org/package/aeson). This simply encodes all the `posts` given to this view as json.

Additionally we need to define a `ToJSON` instance which describes how the `Post` record is going to be transformed to json. We need to add this to our view:

```haskell
instance ToJSON Post where
    toJSON post = object
        [ "id" .= get #id post
        , "title" .= get #title post
        , "body" .= get #body post
        ]
```


The full `Index` View for our `PostsController` looks like this:

```haskell
module Web.View.Posts.Index where
import Web.View.Prelude
import Data.Aeson

data IndexView = IndexView { posts :: [Post] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PostsAction}>Posts</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPostAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
        </div>
    |]

    json IndexView { .. } = toJSON posts

instance ToJSON Post where
    toJSON post = object
        [ "id" .= get #id post
        , "title" .= get #title post
        , "body" .= get #body post
        ]

renderPost post = [hsx|
    <tr>
        <td>{post}</td>
        <td><a href={ShowPostAction (get #id post)}>Show</a></td>
        <td><a href={EditPostAction (get #id post)} class="text-muted">Edit</a></td>
        <td><a href={DeletePostAction (get #id post)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
```

### Getting JSON responses

When you open the `PostsAction` at `/Posts` in your browser you will still get the html output. [This is because IHP uses the browsers `Accept` header to respond in the best format for the browser which is usually html.](https://en.wikipedia.org/wiki/Content_negotiation)

#### Javascript 

From javascript you can get the JSON using `fetch`:
```javascript
const response = await fetch('http://localhost:8000/Posts', { headers: { Accept: 'application/json' } })
    .then(response => response.json());
````

#### curl

You can use `curl` to check out the new JSON response from the terminal:

```bash
curl http://localhost:8000/Posts -H 'Accept: application/json'

[{"body":"This is a test json post","id":"d559cd60-e36e-40ef-b69a-d651e3257dc9","title":"Hello World!"}]
```

### Advanced: Rendering JSON directly from actions

When you are building an API and your action is only responding with JSON (so no html is expected), you can respond with your JSON directly from the controller using `renderJson`:

```haskell
instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch
        renderJson (toJSON posts)

-- The ToJSON instances still needs to be defined somewhere
instance ToJSON Post where
    toJSON post = object
        [ "id" .= get #id post
        , "title" .= get #title post
        , "body" .= get #body post
        ]
```

In this example no content negotiation takes place as the `renderJson` is used instead of the normal `render` function.

The `ToJSON` instances has to be defined somewhere, so it's usually placed inside the controller file. This often makes the file harder to read. We recommend to not use `renderJson` most times and instead stick with a separate view file as described in the section above. Using `renderJson` makes sense only when the controller is very small or you already have a predefined `ToJSON` instance which is not defined in your controller.