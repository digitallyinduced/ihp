# Controller & Actions

```toc

```

## Introduction

In IHP an action is a place for request handling logic. A controller is just a group of related actions.

You can think about actions as messages sent to your application, e.g. a `ShowPostAction { postId :: Id Post }` is the message "Show me the post with id $postId".

Each action needs to be defined as a data structure inside `Web/Types.hs`. Therefore you can see an overview of all the messages which can be sent to your application just by looking at `Web/Types.hs`.

## Creating a new Controller

We recommend using the code generators for adding a new controller. Using the web GUI, you can open [http://localhost:8001/NewController](http://localhost:8001/NewController). Using the CLI run `new-controller CONTROLLER_NAME`.

The following section will guide you through the manual process of creating a new controller.

Let's say we want to create a new controller with a single action `ShowPostAction`. First we need to define our types in `Web/Types.hs`:

```haskell
data PostsController
    = ShowPostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

This defines a type `PostsController` with a data constructor `ShowPostAction { postId :: !(Id Post) }`. The argument `postId` will later be filled with the `postId` parameter of the request URL. This is done automatically by the IHP router. IHP also requires the controller to have `Eq`, `Show` and `Data` instances. Therefore we derive them here.

After we have defined the "interface" for our controller, we need to implement the actual request handling logic. IHP expects to find this inside the `action` function of the [`Controller`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:Controller) instance. We can define this instance in `Web/Controller/Posts.hs`:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude

instance Controller PostsController where
    action ShowPostAction { postId } = renderPlain "Hello World"
```

This implementation for `ShowPostAction` responds with a simple plain text message. The `action` implementation is usually a big pattern match over all possible actions of a controller.

## Reading Query and Body Parameters

Inside the action, you can access request parameters using the `param` function. A parameter can either be a URL parameter like `?paramName=paramValue` (_this is also called a query parameter_), or given as a form field like `<form><input name="paramName" value="paramValue"/></form>` (_in that case we're talking about a body parameter_). The `param` function will work with query and body parameters, so you don't have to worry about that (in case a query and body parameter is set with the same name, the body parameter will take priority).

Given a request like `GET /UsersAction?maxItems=50`, you can access the `maxItems` like this:

```haskell
action UsersAction = do
    let maxItems = param @Int "maxItems"
```

An alternative request to that action can use a form for passing the `maxItems`:

```haskell
<form action={UsersAction} method="POST">
    <input type="text" name="maxItems" value="50" />
    <button type="submit">Send</button>
</form>
```

The value is automatically transformed to an `Int`. This parsing works out of the box for Ids, UUID, Bools, Timestamps, etc. Here are some more examples:

```haskell
action ExampleAction = do
    let userName = param @Text "userName"
    let timestamp = param @UTCTime "createdAt"
    let userId = param @(Id User) "userId"
```

### Missing parameters

In case there is a problem parsing the request parameter, an error will be triggered.

When the parameter is optional, use `paramOrDefault`:

```haskell
action UsersAction = do
    let maxItems = paramOrDefault @Int 50 "maxItems"
```

When this action is called without the `maxItems` parameter being set (or when invalid), it will fall back to the default value `50`.

There is also `paramOrNothing` which will return `Nothing` when the parameter is missing and `Just theValue` otherwise.

### Multiple Params With Same Name (Checkboxes)

When working with checkboxes sometimes there are multiple values for a given parameter name. Given a form like this:

```html
<h1>Pancake</h1>

<input name="ingredients" type="checkbox" value="milk" /> Milk

<input name="ingredients" type="checkbox" value="egg" /> Egg
```

When both checkboxes for Milk and Egg are checked, the usual way of calling `param @Text "ingredients"` will only return the first ingredient `"Milk"`. To access all the checked `ingredients` use `paramList`:

```haskell
action BuildFood = do
    let ingredients = paramList @Text "ingredients"
```

When this action is called with both checkboxes checked `ingredients` will be set to `["milk", "egg"]`. When no checkbox is checked it will return an empty list.

Similar to `param` this works out of the box for Ids, UUID, Bools, Timestamps, etc.

### Passing Data from the Action to the View

A common task is to pass data from the action to the view for rendering. You can do this by extending the view data structure by the required data.

Given an action like this:

```haskell
action ExampleAction = do
    render ExampleView { .. }
```

And an `ExampleView` like this:

```haskell
data ExampleView = ExampleView { }

instance View ExampleView where
    html ExampleView { .. } = [hsx|Hello World!|]
```

Now we want to pass the user's first name to the view, to make the hello world a bit more personal. We can just do it by adding a `firstname :: Text` field to the `ExampleView` data structure and then adding a `{firstname}` to the HSX:

```haskell
data ExampleView = ExampleView { firstname :: Text }

instance View ExampleView where
    html ExampleView { .. } = [hsx|Hello World, {firstname}!|]
```

By now, the compiler will already tell us that we have not defined the `firstname` field inside the action. So we also need to update the action:

```haskell
action ExampleAction = do
    let firstname = "Tester"
    render ExampleView { .. } -- Remember: ExampleView { .. } is just short for ExampleView { firstname = firstname }
```

This will pass the first name `"Tester"` to our view.

We can also make it act more dynamically and allow the user to specify the first name via a query parameter:

```haskell
action ExampleAction = do
    let firstname = paramOrDefault @Text "Unnamed" "firstname"
    render ExampleView { .. }
```

This will render `Hello World, Unnamed!` when the `ExampleAction` is called without a `firstname` parameter.

### Accessing the FrameworkConfig inside Controllers and Views.

The instance of the `FrameworkConfig` that is defined in `Config/Config.hs` will be part of the `RequestContext` and is thus available in controllers. To make it more convenient a helper function called `fromConfig` is available. This function takes a record from the FrameworkConfig and evaluates it.

For example:

```haskell
action MyAction = do
    let env = fromConfig environment
    when (isDevelopment env) (putStrLn "Running in dev mode")
```

### Advanced: Working with Custom Types

Rarely you might want to work with a custom scalar value which is not yet supported with `param`. Define a custom `ParamReader` instance to be able to use the `param` functions with your custom value type. [For that, take a look at the existing instances of `ParamReader`.](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#t:ParamReader)

### Records

When working with records, use `fill` instead of `param`. Fill automatically deals with validation failure when e.g. a field value needs to be an integer, but the submitted value is not numeric.

Here is an example of using `fill`:

```haskell
action CreatePostAction = do
    let post = newRecord @Post
    post
        |> fill @'["title", "body"]
        |> ifValid \case
            Left post -> render NewView { .. }
            Right post -> do
                post <- post |> createRecord
                setSuccessMessage "Post created"
                redirectTo PostsAction
```

## Lifecycle

The Controller instance provides a `beforeAction` function, which is called before the `action` function is called. Common request handling logic like authentication is often placed inside `beforeAction` to protect all actions of the controller against unprotected access.

Here is an example to illustrate the lifecycle:

```haskell
instance Controller PostsController where
    beforeAction = putStrLn "A"
    action ShowPotsAction { postId } = putStrLn "B"
```

Calling the `ShowPostAction` will cause the following output to be logged to the server console:

```html
A B
```

Here is an example to illustrate authentication:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser
    action ShowPostAction { postId } = ...
```

## Accessing the Current Action

Inside the `beforeAction` and `action` you can access the current action using the special `?theAction` variable. That is useful when writing controller helpers, because the variable is passed implicitly.

## Accessing the Current Request

IHP uses the Haskell WAI standard for dealing with HTTP requests and responses. You can get access to the Wai Request data structure by using `request`:

Take a look at [the Wai documentation](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html) to see what you can do with the Wai `Request`:

```haskell
action ExampleAction = do
    let requestBody = request |> getRequestBodyChunk
```

IHP provides a few shortcuts for commonly used request data:

```haskell
action ExampleAction = do
    -- Use `getRequestPath` for accessing the current request path (e.g. /Users)
    putStrLn ("Current request url: " <> tshow getRequestPath)

    -- Use `getRequestPathAndQuery` for accessing the path with all parameters (e.g. /ShowUser?userId=...)
    putStrLn ("Current request url: " <> tshow getRequestPathAndQuery)

    -- Access the request body
    body <- getRequestBody

    -- Get a request
    let theRequest = request
```

### Request Headers

Use `getHeader` to access a request header:

```haskell
action ExampleAction = do
    let userAgent = getHeader "User-Agent"
```

In this example, when the `User-Agent` header is not provided by the request
the `userAgent` variable will be set to `Nothing`. Otherwise, it will be set
to `Just "the user agent value"`.

The lookup for the header in the request is case insensitive.

## Rendering Responses

### Rendering Views

Inside a controller, you have several ways of sending a response. The most common way is to use the `render` function with a `View` data structure, like this:

```
render ShowPostView { .. }
```

The `render` function automatically picks the right response format based on the `Accept` header of the browser. It will try to send an HTML response when HTML is requested, and will also try to send a JSON response when a JSON response is expected. A [`406 Not Acceptable`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/406) will be send when the `render` function cannot fulfill the requested `Accept` formats.

### Rendering Plain Text

Call `renderPlain` to send a simple plain text response:

```haskell
action ExampleAction = do
    renderPlain "My output text"
```

### Rendering HTML

Usually, you want to render your HTML using a view. See `Rendering Views` for details.

Sometimes you want to render HTML without using views, e.g. doing it inline in the action. Call `respondHtml` for that:

```haskell
action ExampleAction = do
    respondHtml [hsx|<div>Hello World</div>|]
```

You will need to import `hsx` into your controller: `import IHP.ViewPrelude (hsx)`.

### Rendering a Static File

Use `renderFile path contentType` to respond with a static file:

```haskell
action ExampleAction = do
    renderFile "static/terms.pdf" "application/pdf"
```

### Rendering a Not Found Message

Use `renderNotFound` to render a generic not found message, e.g. when an entity cannot be found:

```haskell
action ExampleAction = do
    renderNotFound
```

## Redirects

### Redirect to an Action

Use `redirectTo` to redirect to an action:

```haskell
action ExampleAction = do
    redirectTo ShowPostAction { postId = ... }
```

When you need to pass a custom query parameter, you cannot use the `redirectTo` function. See `Redirect to a Path` for that.

The redirect will use HTTP status code `302`. The `baseUrl` in `Config/Config.hs` will be used. In development mode, the `baseUrl` might not be specified in `Config/Config.hs`. Then it will be set to localhost by default.

### Redirect to a Path

Use `redirectToPath` when you want to redirect to a path on the same domain:

```haskell
action ExampleAction = do
    redirectToPath "/blog/wp-login.php"
```

This can also be used to specify query parameter for actions:

```haskell
action ExampleAction = do
    redirectToPath ((pathTo ShowPostAction { .. }) <> "&details=on")
```

### Redirect to a URL

Use `redirectToUrl` to redirect to some external URL:

```haskell
action ExampleAction = do
    redirectToUrl "https://example.com/hello-world.html"
```

## Action Execution

When calling a function to send the response, IHP will stop executing the action. Internally this is implemented by throwing and catching a [`ResponseException`](https://ihp.digitallyinduced.com/api-docs/src/IHP.ControllerSupport.html#ResponseException). Any code after e.g. a `render SomeView { .. }` call will not be called. This also applies to all redirect helpers.

Here is an example of this behavior:

```haskell
action ExampleAction = do
    redirectTo SomeOtherAction
    putStrLn "This line here is not reachable"
```

The `putStrLn` will never be called because the `redirectTo` already stops execution.

When you have created a [`Response`](https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Response) manually, you can use [`respondAndExit`](https://ihp.digitallyinduced.com/api-docs/src/IHP.ControllerSupport.html#respondAndExit) to send your response and stop action execution.

## Request Context

Actions have access to the special variable `?requestContext`.

The `?requestContext` provides access to the Wai request as well as information like the request query and post parameters and the uploaded files. It's usually used by other functions to provide high-level functionality. E.g. the `getHeader` function uses the `?requestContext` to access the request headers.

## Controller Context

TODO

## File Uploads

TODO
