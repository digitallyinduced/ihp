## Upgrading IHP Versions
This document describes breaking changes, as well as how to fix them, that have occured at given releases.
After updating your project, please consult the segments from your current release until now.


# Upgrade to Beta 0.16.0 from Beta 0.15.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.15.0";
        +ref = "refs/tags/v0.16.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.16 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

3. **Tests**

    If you're app has Hspec tests, you will need to follow this step. If your app has no Hspec tess, you can skip this.

    The `withParams` test helper has been replaced with `callActionWithParams`.

    Test code like this:

    ```haskell
            it "creates a new post" $ withParams [("title", "Post title"), ("body", "Body of post")] do
                response <- callAction CreatePostAction

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"
    ```

    needs to be changed to this:

    ```haskell
            it "creates a new post" $ withContext do -- <-- `withContext` here, otherwise it will not work
                response <- callActionWithParams CreatePostAction [("title", "Post title"), ("body", "Body of post")] -- <-- `callAction` turns into `callActionWithParams`

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"
    ```
    

# Upgrade to Beta 0.15.0 from Beta 0.14.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.14.0";
        +ref = "refs/tags/v0.15.0";
        ```

        Please continue the upgrade instructions and don't run any 'make' commands yet.

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.15 URL into your `default.nix`.
2. **Patch `Config/nix/nixpkgs-config.nix`**
    
    Open `Config/nix/nixpkgs-config.nix` and replace it with this:

    ```diff
    # See https://ihp.digitallyinduced.com/Guide/package-management.html
    { ihp, additionalNixpkgsOptions, ... }:
    import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
        ihp = ihp;
        haskellPackagesDir = ./haskell-packages/.;
        additionalNixpkgsOptions = additionalNixpkgsOptions;
    }
    ```

    **Did you run a recent master version of IHP?**

    Please also apply this patch if you have been using a recent master version of IHP. The `additionalNixpkgsOptions` is likely missing in your file.

3. **Remake Env**

    Run the following commands:

    ```bash
    make clean
    nix-shell -j auto --cores 0 --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

4. **.gitignore**

    Add the following lines to your `.gitignore` file:

    ```
    # Ignore locally checked out IHP version
    IHP
    ```

5. **`./start` script**

    Open the project's `start` script and append the following after `set -e` in line 4:

    ```bash
    # On macOS the default max count of open files is 256. IHP needs atleast 1024 to run well.
    #
    # The wai-static-middleware sometimes doesn't close it's file handles directly (likely because of it's use of lazy bytestrings)
    # and then we usually hit the file limit of 256 at some point. With 1024 the limit is usually never hit as the GC kicks in earlier
    # and will close the remaining lazy bytestring handles.
    if [[ $OSTYPE == 'darwin'* ]]; then
        ulimit -n 4096
    fi
    ```

    The file should now look like this: https://github.com/digitallyinduced/ihp-boilerplate/blob/62754efc0b7f8c82f36d0bbdf84e68418fc571c7/start

6. **Session Cookies**

    With IHP v0.15 we've switched the encoding of session cookies from a textual encoding to a binary encoding. IHP v0.15 will ignore the old session format.

    After switching v0.15 your users will be logged out and need to log in again.

# Upgrade to Beta 0.14.0 from Beta 0.13.1

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.13.1";
+ref = "refs/tags/v0.14.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## IHP Background Jobs

IHP jobs now can be scheduled to run at a specific time with `runAt`. For that every table that acts as a job queue in your application needs to be migration.

1. Create a new migration using `new-migration`.
2. For every table ending with `_jobs` do this:
    ```sql
    alter table $TABLE add column run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;
    update $TABLE set run_at = created_at;
    ```
    where `$TABLE` should be replaced with the jobs table.

    The line `update $TABLE set run_at = created_at;` sets the right `run_at` value for all existing jobs.

After that apply this migration to all your IHP instances running on `v.0.14.0`.

# Upgrade to Beta 0.13.1 from Beta 0.13.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.13.0";
+ref = "refs/tags/v0.13.1";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

# Upgrade to Beta 0.13.0 from Beta 0.12.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.12.0";
+ref = "refs/tags/v0.13.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Updating nixpkgs

If you have custom nix package definitions in your project, you will likely get an error that `stdenv` doesn't exist in expressions like `stdenv.lib.SOMETHING`.

nixpkgs has moved `stdenv.lib` to just `lib`. So you need to replace all mentions of `stdenv.lib.` with `lib.`. You might also need to change import statements that import `stdenv` to instead import `lib` directly.

[To get a better understanding of this, take a look a the upgrade commit to see what changes we did to the custom package definitions included with IHP.](https://github.com/digitallyinduced/ihp/commit/cfc8ceb4918749e833f79ba3d362082d0010f1b4)

### Postgres: v11 -> v13

With the nixpkgs update the development postgres server has been updated. Make sure that you run `make clean` during the update process. Otherwise the local database might not start as expected. When you get an error like `libpq: failed (could not connect to server: No such file or directory`, likely postgres is not starting up because your database state is still on v11, while the new postgres is v13.

# Upgrade to Beta 0.12.0 from Beta 0.11.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.11.0";
+ref = "refs/tags/v0.12.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Updating jQuery

The jQuery version used by IHP has changed. We're switching from `3.2.1` to `3.6.0`. jQuery `3.2.1` has some known security vulnerabilities, so it's recommended that you follow these update steps.

To update your application looks search for the string `/vendor/jquery-3.2.1.slim.min.js` in your code base and replace it with `/vendor/jquery-3.6.0.slim.min.js`. Likely the only mention is inside the `Web/View/Layout.hs`.

# Upgrade to Beta 0.11.0 from Beta 0.10.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.10.0";
+ref = "refs/tags/v0.11.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Important Login Changes

Important if you use IHP's Login: IHP's built-in sessions controller used for the built-in login now uses case-insensitive lookup for the email addresses at login. This will improve user experience for users that create their account with `Firstname.Lastname@example.com` and then try to log in using `firstname.lastname@example.com`.


# Upgrade to Beta 0.10.0 from Beta 0.9.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.9.0";
+ref = "refs/tags/v0.10.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Upgrade IHP.HtmlSupport

If you got an type error related to `IHP.HtmlSupport`, follow this step:

This error like is related to the rename of all `IHP.HtmlSupport.*` modules to `IHP.HSX.*`. You can fix this error by replacing all mentions of `IHP.HtmlSupport.` with `IHP.HSX.` in your code base.

## Important `data-` attribute changes

Boolean data attributes like `<div data-is-active={True}>` were rendered like `<div data-is-active="data-is-active">` or `<div>` (if `False`) in previous versions of IHP.

These boolean data attributes are now rendered like `<div data-is-active="true">` and `<div data-is-active="false">`. If you have JS code consuming your data attributes, make sure that you update the JS code.

Other non-data attributes like `<input disabled={True}>` are not affected by this change and will continue to render as `<input disabled="disabled"/>`.

# Upgrade to Beta 0.9.0 from Beta 0.8.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.8.0";
+ref = "refs/tags/v0.9.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


## Upgrade AutoRoute

If you got an type error related to AutoRoute's `parseArgument` function, you need to also follow this step.

The `parseArgument` interface has been removed as this works out of the box now. To upgrade remove your definitions of `parseArgument`.

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
