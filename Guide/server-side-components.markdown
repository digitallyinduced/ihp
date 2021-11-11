# Server-Side Components

```toc

```

## Introduction

IHP Server-Side Components provide a toolkit for building interactive client-side functionality without needing to write too much javascript.

A Server-Side Component consist of a state object, a set of actions and a [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-ServerSideComponent-Types.html#v:render) function.

The typical lifecycle is like this:
1. The component is rendered based as part of a view
2. Once loaded, elements inside the component, e.g. a button, can call server-side actions using a simple javascript library
3. On the server-side the actions are evaluated and a new state object is generated
4. The new state will trigger a re-render
5. The re-rendered content will be diffed with the existing HTML and then HTML update instructions will be sent to client and the view is updated accordingly
6. Repeat at step 2
7. The component is stopped when the page is closed

The Server-Side Component toolkit is currently still in a early development stage. So expect bugs and API changes.

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

This is useful if you have many interactive elements that are controlled by javascript libraries (e.g. a. `<video>` element that is playing). As long as the HTML code of these interactive elements doesn't change on the server-side, the DOM nodes will not be touched by IHP.


### Example Components

If you want to see some more code, you can find components inside the [IHP SSC Playground](https://github.com/digitallyinduced/ihp-ssc-playground/tree/master/Web/Component) or inside the [`ihp-ssc-block-editor-demo` repository](https://github.com/digitallyinduced/ihp-ssc-block-editor-demo/blob/master/Web/Component/BlockEditor.hs).
