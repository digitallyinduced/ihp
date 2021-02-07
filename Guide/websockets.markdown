# WebSockets

```toc

```

## Introduction

IHP has first class support for WebSockets.

**When you only want to use WebSockets to update the UI:** Check out [Auto Refresh](https://ihp.digitallyinduced.com/Guide/auto-refresh.html). The Auto Refresh API provides a high-level approach for pushing HTML/UI updates from the server-side build on top of WebSockets.

The entry points for your WebSocket servers are similiar to the typical IHP controllers, and are also stored in the `Web/Controller/` directory. Similiar to normal IHP controllers, the WebSocket servers are also added to the `FrontController` later on.

## Creating a WebSocket Controlller

Like a normal IHP controller, the WebSocket controllers are also represented with a custom data type. In this guide we're starting by creating a new `HelloWorld` WebSocket application.

Open `Web/Types.hs` and append this:

```haskell
data HelloWorldController
    = HelloWorldController
    deriving (Eq, Show, Data)
```

Next create a new file at `Web/Controller/HelloWorld.hs` with the following content:

```haskell
module Web.Controller.HelloWorld where

import Web.Controller.Prelude

instance WSApp HelloWorldController where
    initialState = HelloWorldController

    run = do
        sendTextData ("Hello World!" :: Text)
```

### Routing

The controller needs to be hooked into the routing. 

Open `Web/FrontController.hs` and add an import for the new controller:

```haskell
import Web.Controller.HelloWebSockets
```

After that mount it into the application like this:

```haskell
instance FrontController WebApplication where
    controllers = 
        [ startPage StartPageAction
        -- Generator Marker
        , webSocketApp @HelloWorldController
        ]
```

As you can see, the WebSocket controller is using `webSocketApp` instead of the usual `parseRoute` function.

Our `HelloWorldController` can now be accessed at `ws://localhost:8000/HelloWorldController`.

## Connecting via JS

It's time to try out our WebSocket controller. Open `static/app.js` and append this to open a new connection:

```javascript
var helloWorldController = new WebSocket('ws://localhost:8000/HelloWorldController');

helloWorldController.onopen = function (event) {
    console.log('Connected');
};

helloWorldController.onmessage = function (event) {
    console.log(event.data);
};
```

Open your application at `http://localhost:8000` and take a look at the JS console. You will see output like this:

```plain
Connected
Hello World!
```

## Sending and receiving messages

We're going to extend our `HelloWorldController` to first for the users name and then print out `Hello $name!` instead of `Hello World!`.

Open `Web/Controller/HelloWorld` and change the code to this:

```haskell
module Web.Controller.HelloWorld where

import Web.Controller.Prelude

instance WSApp HelloWorldController where
    initialState = HelloWorldController

    run = do
        name :: Text <- receiveData
        sendTextData ("Hello " <> name <> "!")
```

The `receiveData` function is used to read data sent by the `JS` side of our code. The `receiveData` function waits until the client is sending us some data before it continues running the `run` function.

On the JS side we now need to send our name like this:

```javascript
var helloWorldController = new WebSocket('ws://localhost:8000/HelloWorldController');

helloWorldController.onopen = function (event) {
    var name = prompt('Your name?');
    helloWorldController.send(name);
};

helloWorldController.onmessage = function (event) {
    console.log(event.data);
};
```

Once the connection is ready, this will ask for the users name and will then send it over the wire. On the server the call of `receiveData` will then return the name we entered and the server will send back the greeting.

### Receiving Other Data Types

The `receiveData` function can also deal with other types such as `Int` or `UUID`. You can use it like that: 

```haskell
myInt  :: Int  <- receiveData
myUUID :: UUID <- receiveData
myBool :: Bool <- receiveData
```


## Managing State

Let's say we want to print out `$name has left` when the WebSocket connection is closed. For that we need to keep track of the name somewhere.

First we need to change our data type to reflect our different states:

```haskell
data HelloWorldController
    = WaitForName
    | NameEntered { name :: Text }
    deriving (Eq, Show, Data)
```

The `WaitForName` now reflects the state before the names has been entered, and the `NameEntered { name = ".." }` keeps track of the entered name.

We need to update the controlller to reflect our new data structure. First we need to make `WaitForName` the start state. For that we need to update the `initialState` in `Web/Controller/HelloWorld.hs`:

```haskell
    initialState = WaitForName
```

Next we're going to update our `run` function to update the state after it got the name from the JS side:

```haskell
    run = do
        name <- receiveData @Text

        setState NameEntered { name }

        sendTextData ("Hello " <> name <> "!")
```

The `setState NameEntered { name }` changes the state from `WaitForName` to `NameEntered`.

To do something when the connection is closed, we're using the `onClose` lifecycle event:

```haskell
module Web.Controller.HelloWorld where

import Web.Controller.Prelude

instance WSApp HelloWorldController where
    initialState = WaitForName

    run = do
        name <- receiveData @Text
        setState NameEntered { name }
        sendTextData ("Hello " <> name <> "!")

    onClose = do
        state <- getState
        case state of
            WaitForName -> pure () -- Do nothing in case the connection is closed before a name was entered
            NameEntered { name } -> do
                let message :: Text = name <> " has left!"
                putStrLn message
```

You can see that we can access the state using `state <- getState`.

Now whenever the connection is closed, it will print out the message inside the terminal where the IHP server is running.

**Good to know:** The connection is automatically closed when the `run` function has finished. Therefore it's normal that the `... has left!` message is directly printed after you entered the name. To keep it running until the browser windows is closed, add a `forever receiveDataMessage` to the end of the `run`.

As you've seen above the primitive state operations are `setState` and `getState`. Using these two operations together with a good data structure is a very powerful way to manage your stateful WebSocket connections.


## Accessing the current user

When the user is logged in, you can use the normal auth functions like `currentUser` inside the WebSocket controller as well:

```haskell
module Web.Controller.HelloWorld where

import Web.Controller.Prelude

instance WSApp HelloWorldController where
    run = do
        let name = currentUser |> get #name
        sendTextData ("Hello " <> name <> "!")
```

## Advanced

### Receiving Custom Data Types

You can write a custom decoder for `receiveData` by writing an instance of `WebSocketsData` for your data type. Here's an example for how the `UUID` decoder is implemented:

```haskell
import qualified Network.WebSockets as WS
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Maybe as Maybe

instance WS.WebSocketsData UUID where
    fromDataMessage (WS.Text byteString _) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromDataMessage (WS.Binary byteString) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromLazyByteString byteString = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    toLazyByteString = UUID.toLazyASCIIBytes
```

### Custom Routing

You can override the usual path at which the WebSocket controller is available by using `webSocketAppWithCustomPath`:

```haskell
instance FrontController WebApplication where
    controllers = 
        [ startPage StartPageAction
        -- Generator Marker
        , webSocketAppWithCustomPath @HelloWorldController "my-ws"
        ]
```

In this example the WebSocket server will be available at `/my-ws`.

### Ping

By default the server will ping the browser every 30 seconds to make sure that the connection is still alive. You can run custom code whenever the ping has finished by overriding the `onPing` function.

Here's an example of how `AutoRefresh` uses `onPing` to keep Auto Refresh Sessions from being closed:

```haskell
    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession sessionId (\session -> session { lastPing = now })
```
