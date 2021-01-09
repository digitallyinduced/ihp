## Upgrading IHP Versions
This document describes breaking changes, as well as how to fix them, that have occured at given releases.
After updating your project, please consult the segments from your current release until now.

# Upgrade to Beta 0.8.0 from Beta 13.12.2020 (v20201213)

## Update `Config/nix/nixpkgs-config.nix`

Replace the content of that file with this:
```nix
# See https://ihp.digitallyinduced.com/Guide/package-management.html
{ ihp }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    ihp = ihp;
    haskellPackagesDir = ./haskell-packages/.;
}
```

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-rev = "67e99ec469d5a215a0b92d8759d5b7a4c7b0e0e1";
+ref = "refs/tags/v0.8.0";
```

**IMPORTANT: `rev` changes to `ref` here. Make sure that you don't miss that. Otherwise nix will complain.**

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

# Upgrade to Beta 13.12.2020 (v20201213) from Beta 27.11.2020 (v20201127)

## Update `Main.hs`

1. Add import for `import IHP.Job.Types` to your `Main.hs`:
    
    ```haskell
    import IHP.Job.Types
    ```
2. Add the following instance to your `Main.hs`:
    ```haskell
    instance Worker RootApplication where
        workers _ = []
    ```

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "17c9507e519a7c37ccf001ada050df171e4af8ef";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


# Upgrade to Beta 27.11.2020 (v20201127) from Beta 13.11.2020 (v20201113)

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "79d4892d6cd531eb2b446a46a2a0e434c8a39895";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


# Upgrade to Beta 13.11.2020 (v20201113) from Beta 30.10.2020 (v20201030)

## Update your `Config/Config.hs`

Old:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types

instance FrameworkConfig where 
    environment = Development
    appHostname = "localhost"
```

New:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
```

Do you have a ´baseUrl´ key in your config?

Old:

```haskell
    baseUrl = "https://..."
```

New:

```haskell
    option (BaseUrl "https://...")
```

### MailServer

Do you have a `mailServer` key in your config?

Old:

```haskell
    mailServer = SES { .. }
```

New:

```haskell
    option SES { .. }
```

## Update `Main.hs`

Old:

```haskell
main = IHP.Server.run
```

New:

```haskell
main = IHP.Server.run config
```

## Update `Web/Routes.hs`

Remove all lines like `type instance ModelControllerMap AdminApplication Project = ProjectsController`.

Search for `ModelControllerMap` in your project. If there are still some results for this, remove the found lines.

## Update `Web/Types.hs`

Remove the `data ViewContext = ..`. The View Context is not used anymore in IHP.

## Update all Views `Web/View/*/*.hs`

Open every view file in the `View` directory.

Remove the `ViewContext` from the `instance View`:

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

This is quite common in `View\Sessions\New.hs` if you are using the built-in authentication.

## Update `Web/View/Layouts.hs`

1. Remove:

```haskell
type Html = HtmlWithContext ViewContext
```

2. If you have other applications such as `Admin`, please also remove the `$APP/View/Context.hs` files.

3. Update calls to `isDevelopment`:

```diff
-when (isDevelopment FrameworkConfig.environment) [hsx|
+when isDevelopment [hsx|
```

4. Update calls to `isProduction`:

```diff
-when (isProduction FrameworkConfig.environment) [hsx|
+when isProduction [hsx|
```

5. Add type signatures to all functions in `Layout.hs`:

```diff
-defaultLayout view = [hsx|...|]
+defaultLayout :: Html -> Html
+defaultLayout view = [hsx|...|]
```

```diff
-stylesheets = [hsx|...|]
+stylesheets :: Html
+stylesheets = [hsx|...|]
```

```diff
-scripts = [hsx|...|]
+scripts :: Html
+scripts = [hsx|...|]
```

```diff
-metaTags = [hsx|...|]
+metaTags :: Html
+metaTags = [hsx|...|]
```

## Remove the `Web/View/Context.hs`

```bash
rm Web/View/Context.hs
```

## Update `Web/View/Prelude.hs`

```diff
-, module Web.View.Context
```

```diff
-import Web.View.Context
```


### `?controllerContext` has been renamed to `?context`

In case you use `?controllerContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`.

### `?viewContext` has been renamed to `?context`

In case you use `?viewContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`.

## Update `Web/FrontController.hs`

1. Add an `import Web.View.Layout (defaultLayout)` at the top of the file
2. Make sure there is a `InitControllerContext`. If it does not exist, place this at the bottom of the file:

```haskell
-- Add these imports, most other imports can propably be removed
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
```

**If you miss this step, you will get an error like `Unable to find ViewLayout in controller context`.**

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "d02a0699220a87d32889ff2a7b87ad81f8bc8195";
```

After that run the following command to update your project:

```bash
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.
