# Scripts

```toc

```

## Introduction

Scripts provide a way to run simple scripts inside the framework context, but outside of the usual web request/response lifecycle.

Common use-cases include:

-   Sending periodic email reminders
-   Sending invoices
-   Anything to be run as a cronjob
-   Background job-queue processing

## Creating a new script

Scripts are located in the `Application/Script/` directory. You can create a new script by running e.g. `new-script HelloWorldToAllUsers`. This will create a file at `Application/Script/HelloWorldToAllUsers.hs` like this:

```haskell
#!/usr/bin/env run-script
module Application.Script.HelloWorldToAllUsers where

import Application.Script.Prelude

run :: Script
run = do
```

The `run` function is our entry point. There we can write our logic, just like inside an action. This means we can call other framework functions, access the database using the usual way, send emails, render views, etc.

Let's print out a hello world to all our users in the console:

```haskell
#!/usr/bin/env run-script
module Application.Script.HelloWorldToAllUsers where

import Application.Script.Prelude

run :: Script
run = do
    users <- query @User |> fetch
    forEach users \user -> do
        putStrLn $ "Hello World, " <> user.firstname <> "!"
```

This will fetch all users and then print out "Hello World, Firstname!".

## Running a script

When running a script locally, ensure the IHP server is running in the background `devenv up`. This is necessary because the script will use the same database connection and other resources as your IHP application.

Scripts are executable by default. You can just run them like a bash script:

```bash
./Application/Script/HelloWorldToAllUsers.hs
...
Hello World, A!
Hello World, B!
Hello World, C!
```

This is made possible because of the [she-bang line](https://en.wikipedia.org/wiki/Shebang_%28Unix%29) `#!/usr/bin/env run-script` at the top of the task file.

In case you get a permission error, try to add the executable flag via `chmod +x Application/Script/HelloWorldToAllUsers.hs`.

## Running a script from ghci

You can run scripts interactively from an already-running GHCi session using `runDevScript`. This uses the default IHP config (reading `DATABASE_URL` from the environment set by devenv):

```haskell
-- Load and run a script file:
ghci> import IHP.ScriptSupport
ghci> :l Application/Script/HelloWorldToAllUsers.hs
ghci> runDevScript run
```

You can also run inline script code directly:

```haskell
ghci> import IHP.ScriptSupport
ghci> runDevScript do { users <- query @User |> fetch; forEach users \user -> putStrLn user.name }
```

### Using a custom config

If you need a custom configuration (e.g. for staging, custom logging, or API keys), use `runScript` with your own `ConfigBuilder` instead:

```haskell
ghci> :l Application.Script.TestScript
ghci> runScript appConfig run
```

You can define custom configurations in your `Config.hs`:

```haskell
-- Config.hs
import IHP.Log.Types

appConfig :: ConfigBuilder
appConfig = do
    option Development
    option $ SES
        { accessKey = "myAccessKey"
        , secretKey = "mySecretAccessKey"
        , region = "eu-west-1"
        }
```

## Building a script

In production, you might want to build a script to a binary for performance reasons. Use nix build like this:

```bash
# Build all scripts along with the application
nix build .#optimized-prod-server

# Or for faster, unoptimized builds
nix build .#unoptimized-prod-server
```

This will produce binaries in the `result/bin/` directory. For example, a script at `Application/Script/HelloWorldToAllUsers.hs` will produce a binary at `result/bin/HelloWorldToAllUsers`.

You can then run the script with:

```bash
result/bin/HelloWorldToAllUsers
```
