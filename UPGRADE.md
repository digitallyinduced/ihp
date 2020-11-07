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

-- OLD:
main = IHP.Server.run

-- NEW:
main = IHP.Server.run config
```


Which brings their usage, namely that the functions described above have to be used in `Web/View/Layout.hs`

```haskell
-- Replace this syntax
when (isDevelopment FrameworkConfig.environment) 

-- With this
when isDevelopment 
```

Also define the type headers for all the functions in `Layout.hs` in order to capture the `?context`:

```haskell
-- OLD:
defaultLayout view = [hsx|...|]

-- NEW:
defaultLayout :: Html -> Html
defaultLayout view = [hsx|...|]

-- OLD:
stylesheets = [hsx|...|]

-- NEW:
stylesheets :: Html
stylesheets = [hsx|...|]

-- OLD:
scripts = [hsx|...|]

-- NEW:
scripts :: Html
scripts = [hsx|...|]

-- OLD:
metaTags = [hsx|...|]

-- NEW:
metaTags :: Html
metaTags = [hsx|...|]
```

### View Context has been removed

#### 1. Remove the `Web/View/Context.hs`

```bash
rm Web/View/Context.hs
```

If you have other applications such as `Admin`, please also remove the `$APP/View/Context.hs` files.

#### 2. Update all View Files in `Web/View/*`

Remove references to `ViewContext`:

```diff
-instance View EditView ViewContext where
+instance View EditView where
```

Does the view have a custom view-specific layout?

```diff
-instance View ShowEnumView ViewContext where
-    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)
+instance View ShowEnumView where
+    beforeRender view = setLayout schemaDesignerLayout
```

### `?controllerContext` has been renamed to `?context`

In case you use `?controllerContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`

### `?viewContext` has been renamed to `?context`

In case you use `?viewContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`