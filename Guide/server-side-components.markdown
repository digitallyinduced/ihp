# Server-Side Components

```toc

```

## Introduction

IHP Server-Side Components provide a toolkit for building interactive client-side functionality without needing to write too much JavaScript.

A Server-Side Component consist of a state object, a set of actions and a [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:render) function.

The typical lifecycle is like this:
1. The component is rendered based as part of a view
2. Once loaded, elements inside the component, e.g. a button, can call server-side actions using a simple javascript library
3. On the server-side the actions are evaluated and a new state object is generated
4. The new state will trigger a re-render
5. The re-rendered content will be diffed with the existing HTML and then HTML update instructions will be sent to client and the view is updated accordingly
6. Repeat at step 2
7. The component is stopped when the page is closed

The Server-Side Component toolkit has been tested in production environments. While the API is mostly stable, some changes may occur in future versions.

## Creating a Component

In this example we're building a counter component: The counter shows a number. When a button is clicked the number will be incremented.

To create this new component first we're creating a new file at `Web/Component/Counter.hs` (the `Component` directory likely does not exist yet, so you need to create it):

```haskell
module Web.Component.Counter where

import IHP.ViewPrelude
import IHP.ServerSideComponent.Types
import IHP.ServerSideComponent.ControllerFunctions

-- The state object
data Counter = Counter { value :: !Int }

-- The set of actions
data CounterController
    = IncrementCounterAction
    | SetCounterValue { newValue :: !Int }
    deriving (Eq, Show, Data)

$(deriveSSC ''CounterController)

instance Component Counter CounterController where
    initialState = Counter { value = 0 }

    -- The render function
    render Counter { value } = [hsx|
        Current: {value} <br />
        <button onclick="callServerAction('IncrementCounterAction')">Plus One</button>
        <hr />
        <input type="number" value={inputValue value} onchange="callServerAction('SetCounterValue', { newValue: parseInt(this.value, 10) })"/>
    |]
    
    -- The action handlers
    action state IncrementCounterAction = do
        state
            |> incrementField #value
            |> pure

    action state SetCounterValue { newValue } = do
        state
            |> set #value newValue
            |> pure

instance SetField "value" Counter Int where setField value' counter = counter { value = value' }
```

You can see that the `Counter` component has a state object with a number `data Counter = Counter { value :: !Int }`. It has two actions `IncrementCounterAction` and `SetCounterValue`. The [`initialState = Counter { value = 0 }`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:initialState) means that the counter starts at 0.

Inside the [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:render) function you can see how server-side actions are triggered from the client-side:

```html
<button onclick="callServerAction('IncrementCounterAction')">Plus One</button>
```

When the `callServerAction('IncrementCounterAction')` is called, it will trigger the [`action state IncrementCounterAction = do`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:action) haskell block to be called on the server.

You can see that the [`action`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:action) handler get's passed the current state and will return a new state based on the action and the current state.

### FrontController

To make the component available to the app, we need to add it to our `Web.FrontController`.

Open the `Web/FrontController.hs` and add these imports:

```haskell
import IHP.ServerSideComponent.RouterFunctions
import Web.Component.Counter
```

Inside the `instance FrontController WebApplication` add a [`routeComponent @Counter`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-RouterFunctions.html#v:routeComponent):

```haskell

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- ...
        , routeComponent @Counter
        ]
```

Now the websocket server for Counter is activated.

### Using the Component

We're adding the component to the standard Welcome view inside our project. But you can use it basically on any view you want.

Open the `Web/View/Static/Welcome.hs` and add these imports:

```haskell
import IHP.ServerSideComponent.ViewFunctions
import Web.Component.Counter
```

Now we change the welcome view to this:

```haskell
instance View WelcomeView where
    html WelcomeView = [hsx|
        <h1>Counter</h1>

        {counter}
    |]
        where
            counter = component @Counter
```

If you wonder why we're using the `where` instead of writing [`{component @Counter}`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-ViewFunctions.html#v:component): Currently the at-symbol `@` is not supported in HSX expressions.

We also need to load the `ihp-ssc.js` from our `Layout.hs`. Open the `Web/View/Layout.hs` and add `<script src="/vendor/ihp-ssc.js"></script>` inside your `scripts` section:

```
scripts :: Html
scripts = [hsx|
        <!-- ... ->
        <script src="/vendor/ihp-ssc.js"></script>
    |]
```

Now when opening the `WelcomeView` you will see the newly created counter.

## Advanced

### Actions with Parameters

Let's say we have actions like this:

```haskell
data BooksTableController
    = SetSearchQuery { searchQuery :: Text }
    | SetOrderBy { column :: Text }
    deriving (Eq, Show, Data, Read)
```

To call the `SetSearchQuery` action with a specific `searchQuery` value, we can pass this to `callServerAction`:

```html
<input
    type="text"
    value={inputValue searchQuery}
    onkeyup="callServerAction('SetSearchQuery', { searchQuery: this.value })"
/>
```

### Fetching from the Database

You can use the typical IHP database operations like [`query @Post`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:query) or [`createRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:createRecord) from your actions.

To fill the inital data you can use the [`componentDidMount`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:createRecord) lifecycle function:

```haskell
data PostsTable = PostsTable
    { posts :: Maybe [Post]
    }
    deriving (Eq, Show)


instance Component PostsTable PostsTableController where
    initialState = PostsTable { posts = Nothing }

    componentDidMount state = do
        books <- query @Post |> fetch

        state
            |> setJust #posts posts
            |> pure

    render PostsTable { .. } = [hsx|
        {when (isNothing posts) loadingIndicator}
        {forEach posts renderPost}
    |]
```

The [`componentDidMount`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:createRecord) get's passed the initial state and returns a new state. It's called right after the first render once the client has wired up the WebSocket connection.

When the `posts` field is set to `Nothing` we know that the data is still being fetched. In that case we render a loading spinner inside our [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:render) function.

### HTML Diffing & Patching

IHP uses a HTML Diff & Patch approach to update the components HTML. You can see this by analysing the data that is sent over the WebSocket connection.

In the above example, when the `Plus One` button of the counter is clicked, the client will send the following message to the server using the WebSocket connection:

```javascript
{"action":"IncrementCounterAction"}
```

After that the server will respond:

```haskell
[{"type":"UpdateTextContent","textContent":"Current: 1","path":[0]}]
```

So the server only responds with update instructions that transform the counter's `Current: 0 ` to `Counter: 1`.

This is useful if you have many interactive elements that are controlled by JavaScript libraries (e.g. a. `<video>` element that is playing). As long as the HTML code of these interactive elements doesn't change on the server-side, the DOM nodes will not be touched by IHP.


### Example Components

If you want to see some more code, you can find components inside the [IHP SSC Playground](https://github.com/digitallyinduced/ihp-ssc-playground/tree/master/Web/Component) or inside the [`ihp-ssc-block-editor-demo` repository](https://github.com/digitallyinduced/ihp-ssc-block-editor-demo/blob/master/Web/Component/BlockEditor.hs).

## Error Handling

Server-Side Components include built-in error handling for common failure scenarios.

### Action Errors

When an action handler throws an exception, the error is:
1. Logged on the server with full details
2. Sent to the client with a generic error message
3. Displayed to the user temporarily (auto-dismisses after 5 seconds)

```haskell
action state SomeAction = do
    -- If this throws an exception, the client will be notified
    result <- someOperationThatMightFail
    pure state
```

### Custom Error Handling

You can listen for SSC errors in JavaScript to implement custom error handling:

```javascript
document.addEventListener('ssc:error', function(event) {
    console.log('SSC Error:', event.detail.error);
    console.log('Component:', event.detail.component);

    // Implement custom error handling, e.g.:
    // - Send to error tracking service
    // - Show custom notification
});
```

### Parse Errors

If the client sends an invalid action payload (e.g., malformed JSON or unknown action), the server logs the error and sends an `SSCParseError` to the client.

## Connection Resilience

The SSC JavaScript client automatically handles connection issues:

### Automatic Reconnection

When the WebSocket connection is lost, the client will:
1. Automatically attempt to reconnect with exponential backoff
2. Show a visual indicator of the connection state
3. Queue any actions triggered while disconnected
4. Replay queued actions once reconnected

### Connection States

The client tracks these connection states:
- **Connecting**: Initial connection in progress
- **Connected**: WebSocket is open and ready
- **Reconnecting**: Attempting to restore a lost connection
- **Failed**: Max reconnection attempts reached

### Visibility Change Handling

When a browser tab becomes visible again, the client will attempt to reconnect if the connection was lost while the tab was in the background.

### Manual Retry

If automatic reconnection fails after multiple attempts, a "Retry" button is shown to allow manual reconnection.

## Production Considerations

### WebSocket Scaling

Each SSC component maintains an active WebSocket connection. Consider these factors when scaling:

1. **Connection Limits**: Each server process has a limit on concurrent WebSocket connections. Monitor your connection count.

2. **Sticky Sessions**: If using multiple server instances behind a load balancer, enable sticky sessions (session affinity) to ensure WebSocket connections route to the same server.

3. **Timeouts**: Configure appropriate WebSocket timeout settings on your load balancer to prevent premature connection drops.

### State Management

Component state is held in memory on the server:

1. **State Size**: Keep component state small. Large state objects increase memory usage per connection.

2. **Ephemeral State**: Component state is lost when the connection closes or the server restarts. For persistent data, use the database.

3. **State Recovery**: After a reconnection, the component reinitializes with `initialState` and `componentDidMount`. Design your components to handle this gracefully.

### Monitoring

IHP SSC logs lifecycle events that can be used for monitoring:

- Component connections and disconnections
- Action execution errors
- Parse errors from invalid client messages

Enable debug logging in development by adding `data-debug-mode="true"` to your script tag:

```html
<script src="/vendor/ihp-ssc.js" data-debug-mode="true"></script>
```

### Security Considerations

1. **Action Validation**: Always validate action parameters on the server. Never trust client-provided data.

2. **Authorization**: Check user permissions in action handlers if the component displays or modifies sensitive data.

3. **Rate Limiting**: Consider implementing rate limiting for action handlers that perform expensive operations.
