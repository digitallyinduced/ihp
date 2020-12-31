# View

```toc

```

## Introduction

IHP views are usually represented as HTML, but can also be represented as JSON or other formats.

The HTML templating is implemented on top of the well-known blaze-html Haskell library. To quickly build HTML views, IHP supports a JSX-like syntax called HSX. HSX is type-checked and compiled to Haskell code at compile-time.

The controller provides the view with a key-value map called `ControllerContext`. The `ControllerContext` provides the view information it might need to render, without always explicitly passing it. This is usually used to pass e.g. the current HTTP request, logged-in user, flash messages, the layout, etc..

Usually, a view consists of a data structure and a `View` instance. E.g. like this:

```haskell
data ExampleView = ExampleView { optionA :: Text, optionB :: Bool }

instance View ExampleView where
    html ExampleView { .. } = [hsx|Hello World {optionA}!|]
```

## Layouts

By default when rendering an HTML view, IHP uses the default application layout to render your view. It's defined at `defaultLayout` in `Web.View.Layout`.

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

### Using a layout inside a single view

To use the layout inside a view, call `setLayout` from the `beforeRender`:

```haskell
instance View MyView where
    beforeRender view = do
        setLayout appLayout
```

### Using a layout for a complete controller

When all views of a controller use a custom layout place the `setLayout` call in the `beforeAction` of the controller:

```haskell
instance Controller MyController where
    beforeAction = do
        setLayout appLayout

    action MyAction = do
        render MyView { .. }
```

### Changing the default layout

You can change the default layout of your application by updating `initContext` in `Web.FrontController`.

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout -- Change defaultLayout to your other layout function
```

### Disabling the Layout for a View

You can disable the layout for a specific view by overriding the `beforeRender` function like this:

```haskell
instance View MyView where
    beforeRender view = do
        setLayout (\view -> view)

    -- ...
```

## Common View Tasks

### Accessing the Request

Use `theRequest` to access the current [WAI request](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html).

### Highlighting the current active link

Use [`isActivePath`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActivePath) to check whether the current request URL matches a given action.

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

By default, a message `Are you sure you want to delete this?` is shown as a simple confirmation alert with yes/no choices. The message text can be customized.

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

In production mode, your application is using a custom integration of morphdom and [TurboLinks](https://github.com/turbolinks/turbolinks) together with [InstantClick](http://instantclick.io/). TurboLinks makes navigating the application even faster because it's not doing a full page refresh. We've integrated TurboLinks with morphdom to only update the parts of your HTML that have changed. This was inspired by react.js's DOM patch approach and allows e.g. CSS animations to run on a page transition. Using this makes your app feel like a [SPA](https://en.wikipedia.org/wiki/Single-page_application) without you writing any JavaScript code.

To improve latency, TurboLinks is configured to prefetch the URL immediately on mouse-hover. Usually, the time between a mouse-hover of a link and mouse click is 100ms - 200ms. As long as the server responds in less than 100ms, the response is already there when the click event is fired. This makes your app faster than most single page application (most SPAs still need to fetch some data after clicking).

This setup is designed as a progressive enhancement. Your application is still usable when JavaScript is disabled.
Even when disabled, your application will still be amazingly fast.

You can disable this behavior by removing the following code from your `Web/Layout.hs`:

```haskell
    when isProduction [hsx|
            <script src="/vendor/turbolinks.js"></script>
            <script src="/vendor/morphdom-umd.min.js"></script>
            <script src="/vendor/turbolinksMorphdom.js"></script>
            <script src="/vendor/turbolinksInstantClick.js"></script>
        |]
```

Preloading with InstantClick on hover will only happen with links that

1. Use the GET method
2. Do not link to an anchor or end in `#`
3. Link to a different URL other than `location.href`
4. Do not have an attribute `data-turbolinks-preload='false'`

(So putting an anchor on a link, or explicitly setting the `data-turbolinks-preload` attribute to `false`, will let you selectively turn off preloading for that link.)

## JSON

Views that are rendered by calling the `render` function can also respond with JSON.

Let's say we have a normal HTML view that renders all posts for our blog app:

```haskell
instance View IndexView where
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

We can add a JSON output for all blog posts by adding a `json` function to this:

```haskell
import Data.Aeson -- <--- Add this import at the top of the file

instance View IndexView where
    html IndexView { .. } = [hsx|
        ...
    |]

    json IndexView { .. } = toJSON posts -- <---- The new json render function
```

In the above code, our `json` function has access to all arguments passed to the view. Here we call `toJSON`, which is provided by the [aeson](https://hackage.haskell.org/package/aeson) Haskell library. This simply encodes all the `posts` given to this view as JSON.

Additionally we need to define a `ToJSON` instance which describes how the `Post` record is going to be transformed to JSON. We need to add this to our view:

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

instance View IndexView where
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

When you open the `PostsAction` at `/Posts` in your browser you will still get the HTML output. [This is because IHP uses the browser `Accept` header to respond in the best format for the browser which is usually HTML.](https://en.wikipedia.org/wiki/Content_negotiation)

#### JavaScript

From JavaScript you can get the JSON using `fetch`:

```javascript
const response = await fetch("http://localhost:8000/Posts", {
    headers: { Accept: "application/json" },
}).then((response) => response.json());
```

#### curl

You can use `curl` to check out the new JSON response from the terminal:

```bash
curl http://localhost:8000/Posts -H 'Accept: application/json'

[{"body":"This is a test json post","id":"d559cd60-e36e-40ef-b69a-d651e3257dc9","title":"Hello World!"}]
```

### Advanced: Rendering JSON directly from actions

When you are building an API and your action is only responding with JSON (so no HTML is expected), you can respond with your JSON directly from the controller using `renderJson`:

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

In this example, no content negotiation takes place as the `renderJson` is used instead of the normal `render` function.

The `ToJSON` instances have to be defined somewhere, so it's usually placed inside the controller file. This often makes the file harder to read. We recommend not using `renderJson` most times and instead stick with a separate view file as described in the section above. Using `renderJson` makes sense only when the controller is very small or you already have a predefined `ToJSON` instance which is not defined in your controller.
