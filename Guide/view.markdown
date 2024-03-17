# View

```toc

```

## Introduction

IHP views are usually represented as HTML, but can also be represented as JSON or other formats.

The HTML templating is implemented on top of the well-known blaze-html Haskell library. To quickly build HTML views, IHP supports a JSX-like syntax called HSX. HSX is type-checked and compiled to Haskell code at compile-time.

The controller provides the view with a key-value map called [`ControllerContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:ControllerContext). The [`ControllerContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:ControllerContext) provides the view information it might need to render, without always explicitly passing it. This is usually used to pass e.g. the current HTTP request, logged-in user, flash messages, the layout, etc.

Usually, a view consists of a data structure and a [`View`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#t:View) instance. E.g. like this:

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
appLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
    </head>
    <body>
        <h1>Welcome to my app</h1>
        {inner}
    </body>
</html>
|]
```

Now add `appLayout` to the export list of the module header:

```haskell
module Web.View.Layout (defaultLayout, appLayout) where
```

### Using a layout inside a single view

To use the layout inside a view, call [`setLayout`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Layout.html#v:setLayout) from the [`beforeRender`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:beforeRender):

```haskell
instance View MyView where
    beforeRender view = do
        setLayout appLayout
```

### Using a layout for a complete controller

When all views of a controller use a custom layout place the [`setLayout`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Layout.html#v:setLayout) call in the [`beforeAction`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#v:beforeAction) of the controller:

```haskell
instance Controller MyController where
    beforeAction = do
        setLayout appLayout

    action MyAction = do
        render MyView { .. }
```

### Changing the default layout

You can change the default layout of your application by updating [`initContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#v:initContext) in `Web.FrontController`.

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout -- Change defaultLayout to your other layout function
```

### Disabling the Layout for a View

You can disable the layout for a specific view by overriding the [`beforeRender`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:beforeRender) function like this:

```haskell
instance View MyView where
    beforeRender view = do
        setLayout (\view -> view)

    -- ...
```

### Layout Variables

Sometimes you want to pass values to the layout without always having to specify them manually inside the `render MyView { .. }` calls.

Here's some examples:

- Your business application wants to display the user's company name as part of the layout on every page
- The site's navigation is built dynamically based on a `navigation_items` table in the database
- The layout should show unread notifications
- The current logged in user. You don't pass this manually to the layout inside the `render MyView { .. }` statement of your action, but it's still accessible via `currentUser` inside the views.

In all of these cases you don't want to deal with passing the information to the layout inside every action of your application.

The general idea is that we store the needed information inside the controller context. The controller context is an implicit parameter that is passed around via the `?context` variable during the request response lifecycle. Think of it as a key-value map which you can write to before rendering, and read from during the view rendering.

Let's deal with the first case: Our business application wants to display the user's company name as part of the layout on every page.

Open `Web/FrontController.hs` and customize it like this:

```haskell
-- Web/FrontController.hs

instance InitControllerContext WebApplication where
    initContext = do
        -- ...

        initCompanyContext -- <---- ADD THIS

initCompanyContext :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
initCompanyContext =
    case currentUserOrNothing of
        Just currentUser -> do
            company <- fetch currentUser.companyId

            -- Here the magic happens: We put the company of the user into the context
            putContext company

        Nothing -> pure ()
```

The [`initContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#v:initContext) is called on every request, just before the action is executed. The `initCompanyContext` fetches the current user's company and then calls [`putContext company`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Context.html#v:putContext) to store it inside the controller context.

Next we'll read the company from the `Layout.hs`:

```haskell
-- Web/View/Layout.hs

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
    {inner}

    {when isLoggedIn renderCompany}
|]
    where
        isLoggedIn = isJust currentUserOrNothing

renderCompany :: Html
renderCompany = [hsx|
    <div class="company">
        {company.name}
    </div>
|]

company :: (?context :: ControllerContext) => Company
company = fromFrozenContext
```

Here the company is read by using the [`fromFrozenContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Context.html#v:fromFrozenContext) function.

You might wonder: How does [`fromFrozenContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Context.html#v:fromFrozenContext) know that I want the company? The context is a key-value map, where the key's are the type of the object. Using the `company :: Company` type annotation the [`fromFrozenContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Context.html#v:fromFrozenContext) knows we want to read the value with the key `Company`.

Now the `company` variable can be used to read the current user's company across the layout and also in all views (you need to add `company` to the export list of the Layout module for that). If the `company` value is used somewhere during rendering while the user is not logged in, it will raise a runtime error.

## Common View Tasks

### Accessing the Request

Use [`theRequest`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:theRequest) to access the current [WAI request](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html).

### Passing JSON to the View

You might need to pass JSON values to the view, so later you could have a JS script to read it. You should use Aeson's `toJSON` function to convert your data to JSON and then pass it to the view. Let's say you have a `Posts` controller and `Post`s records that you want to print in the console.

```haskell
-- Web/Controller/Posts.hs
instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch

        render IndexView { .. }

```

Then in the view, you can access the JSON data like this:

```haskell
-- Web/View/Posts/Index.hs

module Web.View.Posts.Index where
import Web.View.Prelude

-- Add Aeson import.
import Data.Aeson (encode)

data IndexView = IndexView { posts :: [Post] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            Open the developer's console to see the posts JSON data.
        </div>
        {- Pass the encoded JSON to the JS script -}
        <script data-posts={encode $ postsToJson posts}>
            // Parse the encoded JSON, and print to console
            console.log(JSON.parse(document.currentScript.dataset.posts));
        </script>
    |]
    where
        postsToJson :: [Post] -> Value
        postsToJson posts =
            posts
                |> fmap (\post -> object
                    [ "id" .= post.id
                    , "title" .= post.title
                    , "body" .= post.body
                    ])
                |> toJSON
```

No you can go to `/Posts`, create a few posts, and see their JSON in the browser's developer console.

### Highlighting the current active link

Use [`isActiveAction`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActiveAction) to check whether the current request URL matches a given action:

```haskell
<a href={ShowProjectAction} class={classes ["nav-link", ("active", isActiveAction ShowProjectAction)]}>
    Show Project
</a>
```

If you need to work with a Text for the URL you can use the [`isActivePath`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActiveAction).

```haskell
<a href={ShowProjectAction} class={classes ["nav-link", ("active", isActivePath "/ShowProject")]}>
    Show Project
</a>
```

Finally, if you only need to know if the current Controller is used, regardless of which action, use [`isActiveController`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:isActiveController)

#### `timeAgo`

```haskell
timeAgo post.createdAt -- "1 minute ago"
```

#### `dateTime`

```haskell
dateTime post.createdAt -- "10.6.2019, 15:58"
```

### Customizing Delete Confirmation

By default, a message `Are you sure you want to delete this?` is shown as a simple confirmation alert with yes/no choices. The message text can be customized:

```haskell
<a href={DeleteToolAction tool.id} class="js-delete" data-confirm="Deleting a tool will also delete all usage of the tool. Continue?">Delete Tool</a>
```

#### Suppressing Delete Confirmation

```haskell
<a href={DeleteToolAction tool.id} class="js-delete js-delete-no-confirm">Delete Tool</a>
```

### Breadcrumbs

When using the code generator for a new Controller or View, we get Breadcrumbs pre-configured. You can change them, with three helper functions. All those functions can get a simple text for the label (e.g. `"Posts"`) or even HTML opening the door the using SVG or font icons.

Here are a few common examples:

* `breadcrumbLink "Posts" PostsAction` - Show "Posts" label with link to the Posts index page.
* `breadcrumbLink [hsx|<i class="fas fa-home"></i>|] HomepageAction` - Using HTML to show a "Home" fonts-awesome icon.
* `breadcrumbText "Show Post"` - Showing text or HTML, without a link. Normally that would be for the last breadcrumb.
* `breadcrumbLinkExternal "Back to Portal" "https://example.com"` - Breadcrumb link to an external site.

```
instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Post</h1>
        <p>{post}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]
```

### SEO

#### Setting the Page Title

You can override the default page title by calling [`setTitle`](https://ihp.digitallyinduced.com/api-docs/IHP-PageHead-ControllerFunctions.html#v:setTitle) inside the [`beforeRender`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:beforeRender) function of your view:

```haskell
instance View MyView where
    beforeRender MyView { post } = do
        setTitle post.title

    -- ...
```

You can also call [`setTitle`](https://ihp.digitallyinduced.com/api-docs/IHP-PageHead-ControllerFunctions.html#v:setTitle) from the controller action if needed:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Show

instance Controller PostsController where
    action ShowPostAction { postId } = do
        post <- fetch postId
        setTitle post.title
        render ShowView { .. }
```

If the page title is not changed as expected, make sure that your `Layout.hs` is using [`pageTitleOrDefault`](https://ihp.digitallyinduced.com/api-docs/IHP-PageHead-ViewFunctions.html#v:pageTitleOrDefault):

```html
WRONG:
<head>
    <title>This title will not support customization</title>
</head>

RIGHT:
<head>
    <title>{pageTitleOrDefault "The default page title, can be overriden in views"}</title>
</head>
```


#### OG Meta Tags

To dynamically manage meta tags like `<meta property="og:description" content="dynamic content"/>` add this to your `Layout.hs`:

```html
<head>
    <title>App</title>

    <!-- ADD THIS: -->
    {descriptionOrDefault "default meta description"}
    {ogTitleOrDefault "default title"}
    {ogTypeOrDefault "article"}
    {ogDescriptionOrDefault "Hello world"}
    {ogUrl}
    {ogImage}
</head>
```

You can set the values for these meta tags from the [`beforeRender`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:beforeRender) function within your view:


```haskell
instance View MyView where
    beforeRender MyView { post } = do
        setOGTitle post.title
        setOGDescription post.summary
        setOGUrl (urlTo ShowPostAction { .. })

        case post.imageUrl of
            Just url -> setOGImage url
            Nothing -> pure () -- When setOGImage is not called, the og:image tag will not be rendered

    -- ...
```


## Diff-Based DOM Updates

When in development, your views will automatically refresh on code changes. This works by re-requesting the view from the server via AJAX and then using [morphdom](https://github.com/patrick-steele-idem/morphdom) to update the visible DOM.

## TurboLinks

In production mode, your application is using a custom integration of morphdom and [TurboLinks](https://github.com/turbolinks/turbolinks) together with [InstantClick](http://instantclick.io/). TurboLinks makes navigating the application even faster because it's not doing a full page refresh. We've integrated TurboLinks with morphdom to only update the parts of your HTML that have changed. This was inspired by react.js's DOM patch approach and allows e.g. CSS animations to run on a page transition. Using this makes your app feel like a [SPA](https://en.wikipedia.org/wiki/Single-page_application) without you writing any JavaScript code.

To improve latency, TurboLinks is configured to prefetch the URL immediately on mouse-hover. Usually, the time between a mouse-hover of a link and the mouse click is 100ms - 200ms. As long as the server responds in less than 100ms, the response is already there when the click event is fired. This makes your app faster than most single page application (most SPAs still need to fetch some data after clicking).

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
4. Do not have an attribute `data-turbolinks-preload="false"`

(So putting an anchor on a link, or explicitly setting the `data-turbolinks-preload` attribute to `false`, will let you selectively turn off preloading for that link.)

We provide two custom events

-   `ihp:load` that will trigger when `DOMContentLoaded` or `turbolinks:load`
-   `ihp:unload` that will fire on `beforeunload` and before [morphdom patches the page](#TurboLinks)

```javascript
document.addEventListener('ihp:load', () => {
    console.log('Page Loaded');
});

document.addEventListener('ihp:unload', () => {
    console.log('Page Unloaded');
});
```

## JSON

Views that are rendered by calling the [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:render) function can also respond with JSON.

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

We can add a JSON output for all blog posts by adding a [`json`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:json) function to this:

```haskell
import Data.Aeson -- <--- Add this import at the top of the file

instance View IndexView where
    html IndexView { .. } = [hsx|
        ...
    |]

    json IndexView { .. } = toJSON posts -- <---- The new json render function
```

In the above code, our [`json`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:json) function has access to all arguments passed to the view. Here we call [`toJSON`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:toJSON), which is provided by the [aeson](https://hackage.haskell.org/package/aeson) Haskell library. This simply encodes all the `posts` given to this view as JSON.

Additionally we need to define a [`ToJSON`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#t:ToJSON) instance which describes how the `Post` record is going to be transformed to JSON. We need to add this to our view:

```haskell
instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
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
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        ]

renderPost post = [hsx|
    <tr>
        <td>{post}</td>
        <td><a href={ShowPostAction post.id}>Show</a></td>
        <td><a href={EditPostAction post.id} class="text-muted">Edit</a></td>
        <td><a href={DeletePostAction post.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
```

### Getting JSON responses

When you open the `PostsAction` at `/Posts` in your browser you will still get the HTML output. [This is because IHP uses the browser `Accept` header to respond in the best format for the browser which is usually HTML.](https://en.wikipedia.org/wiki/Content_negotiation)

#### JavaScript

From JavaScript you can get the JSON using [`fetch`](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch):

```javascript
const response = await fetch('http://localhost:8000/Posts', {
    headers: { Accept: 'application/json' },
});
const json = await response.json();
```

#### curl

You can use [`curl`](https://curl.se/) to check out the new JSON response from the terminal:

```bash
curl http://localhost:8000/Posts -H 'Accept: application/json'

[{"body":"This is a test json post","id":"d559cd60-e36e-40ef-b69a-d651e3257dc9","title":"Hello World!"}]
```

### Advanced: Rendering JSON directly from actions

When you are building an API and your action is only responding with JSON (so no HTML is expected), you can respond with your JSON directly from the controller using [`renderJson`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:renderJson):

```haskell
instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch
        renderJson (toJSON posts)

-- The ToJSON instances still needs to be defined somewhere
instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        ]
```

In this example, no content negotiation takes place as the [`renderJson`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:renderJson) is used instead of the normal `render` function.

The [`ToJSON`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#t:ToJSON) instances have to be defined somewhere, so it's usually placed inside the controller file. This often makes the file harder to read. We recommend not using [`renderJson`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:renderJson) most times and instead stick with a separate view file as described in the section above. Using [`renderJson`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:renderJson) makes sense only when the controller is very small or you already have a predefined [`ToJSON`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#t:ToJSON) instance which is not defined in your controller.
