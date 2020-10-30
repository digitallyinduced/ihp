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


Which brings their usage, namely that the functions described above have to be used in `Web/View/Layout.hs`

```haskell
-- Replace this syntax
when (isDevelopment FrameworkConfig.environment) 

-- With this
when (isDevelopment $ fromConfig environment) 
```

Also define the type headers for all the functions in `Layout.hs` in order to capture the ViewContext:

```haskell
defaultLayout :: (?context :: ViewContext) => Html -> Html
stylesheets :: (?context :: ViewContext) => Html
scripts :: (?context :: ViewContext) => Html 
metaTags :: (?context :: ViewContext) => Html
```

Finally, the naming of different kinds of contexts in implicit parameters have been normalized to be called just `?context`. In `Web/View/Context.sh` change the following:

```haskell
-- Web/View/Context.hs

-- change this
let viewContext = ViewContext {
        requestContext = ?context,
        -- user = currentUserOrNothing,
        flashMessages,
        controllerContext = ?controllerContext,
        layout = let ?viewContext = viewContext in defaultLayout
    }

-- to this  (rename ?viewContext to ?context)
let viewContext = ViewContext {
        requestContext = ?context,
        -- user = currentUserOrNothing,
        flashMessages,
        controllerContext = ?controllerContext,
        layout = let ?context = viewContext in defaultLayout
    }
