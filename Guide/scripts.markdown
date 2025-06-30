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

You can also open a ghci prompt to test features in scripts interactively:

```bash
make ghci
```

Then you can load your script into the interpreter:
```
:l Application.Script.TestScript
```

and run the script from the IHP ghci command line:

```haskell
IHP> runScript ihpDefaultConfig run
```

The `ihpDefaultConfig` is made available from the `Application.Script.Prelude` import but can be substituted
with your own configuration data structure defined in `Config`.

The configuration type is `ConfigBuilder` which is an IHP internal data structure. It provides
a number of configuration parameters stored as a record that tells IHP about your app's configuration:
e.g. where to look for your database, or a place to store API keys.

This is particularly useful for adjusting
logging levels or testing new APIs.

You can also define custom configurations in your Config.hs, e.g. for staging,
local development, or simply use your production application configuration:

```haskell
-- Config.hs
import qualified IHP.Log as Log
import Config.hs
import IHP.Log.Types

appConfig :: ConfigBuilder
appConfig = do
    option Development

   -- option Production
   -- option (AppHostname "ihpapp.io")
   -- option (BaseUrl "https://ihpapp.io")

    option $ SES
        {
          accessKey = "myAccessKey"
        , secretKey = "mySecretAccessKey"
        , region = "eu-west-1" -- YOUR REGION
        }


testConfig :: ConfigBuilder
testConfig = do
    option Development

    logger <- liftIO $ newLogger def {
        level = Debug,
        formatter = withTimeAndLevelFormatter,
        destination = File "Log/App.log" (SizeRotate (Bytes (4 * 1024 * 1024)) 4) defaultBufSize
        }
    option logger

```

Your app's configuration file `Config.hs` can then be imported in your Script:

```haskell
-- Application/Script/TestScript
#!/usr/bin/env run-script
module Application.Script.TestScript where

import Application.Script.Prelude

import Config


run :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()
run = do
    user <- query @User |> filterWhere(#name, "Php") |> fetch
    user |> set #name "Ihp" |> updateRecord
    pure ()

```

and then run the script from ghci:

```haskell
IHP> :l Application.Script.TestScript
IHP> runScript appConfig run
```

## Building a script

In production, you might want to build a script to a binary for performance reasons. Use make like this:

```bash
make build/bin/Script/HelloWorldToAllUsers
```

This will produce a binary `build/bin/Script/HelloWorldToAllUsers` from the source file `Application/Script/HelloWorldToAllUsers.hs`.
