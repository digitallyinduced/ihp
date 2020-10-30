## Description
This document describes breaking changes, as well as how to fix them, that have occured at given releases.
After updating your project, please consult the segments from your current release until now.

## In upcoming releases:

*Please add a section here when making a PR containing breaking changes. Please use the following header:* `### [Title](link to PR)`

### [FrameworkConfig is now a datatype](https://github.com/digitallyinduced/ihp/pull/485)

In order to remove a number of unsafeIO operations, the FrameworkConfig has been refactored to a datatype. 
Furthermore, the construction of the FrameworkConfig now leverages a state monad. 
To changes config values use the `option` function:

```haskell
-- Config/Config.hs

config :: ConfigBuilder
config = do
    option Development
    option $ AppHostname "localhost"

```

The option function uses a typemap to assign the values to different properties.
For generally used types, such as the `appHostName :: Text`, a newtype has been introduced
The config now also has to be explicitly passed to the `Server.run` function.

```haskell
-- Main.hs

main :: IO ()
main = IHP.Server.run config
```

If you wish to access configuration properties in Controllers, Views or Scripts, then you can leverage the `getConfig` function.

```haskell
-- in Controllers
getConfig :: (?requestContext :: RequestContext) => (FrameworkConfig -> a) -> a

-- in Views
getConfig :: (?viewContext :: ViewContext) => (FrameworkConfig -> a) -> a
```

Because the `ViewContext` is defined in the application itself, we quickly need to implement the getConfig for views ourselves:

```haskell
-- Web/Types.hs

-- ...

import IHP.FrameworkConfig


-- ...

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

-- Add this:
getConfig :: (?viewContext :: ViewContext) => (FrameworkConfig -> a) -> a
getConfig = let ?requestContext = requestContext ?viewContext in RequestContext.getConfig

-- ...
```

Which brings us the last change, namely that the functions described above have to be used in `Web/View/Layout.hs`

```haskell
-- Replace this syntax
when (isDevelopment FrameworkConfig.environment) 

-- With this
when (isDevelopment $ getConfig environment) 
```

Also define the type headers for all the functions in `Layout.hs` in order to capture the ViewContext:

```haskell
defaultLayout :: (?viewContext :: ViewContext) => Html -> Html
stylesheets :: (?viewContext :: ViewContext) => Html
scripts :: (?viewContext :: ViewContext) => Html 
metaTags :: (?viewContext :: ViewContext) => Html
```

