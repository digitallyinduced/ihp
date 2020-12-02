# Scripts

```toc
```

## Introduction

Scripts provide a way to run simple scripts inside the framework context, but outside of the usual web request response lifecycle.

Common use-cases include:

- Sending periodic email reminders
- Sending invoices
- Anything to be run as a cronjob
- Background job-queue processing

## Creating a new script

Scripts are located in the `Application/Script/` directory. You can create a new script by running e.g. `new-script HelloWorldToAllUsers`. This will create a file at `Application/Script/HelloWorldToAllUsers.hs` like this:

```haskell
#!/usr/bin/env run-script
module Application.Script.HelloWorldToAllUsers where

import Application.Script.Prelude

run :: Script
run = do
```

The `run` function is our entrypoint. There we can write our logic, just like inside an action. This means we can call other framework functions, access the database using the usual way, send emails, render views, etc.

Let's print out a hello world to all our users in the console:


```haskell
#!/usr/bin/env run-script
module Application.Script.HelloWorldToAllUsers where

import Application.Script.Prelude

run :: Script
run = do
    users <- query @User |> fetch
    forEach users \user -> do
        putStrLn $ "Hello World, " <> get #firstname user <> "!"
```

This will fetch all users and then print out "Hello World, Firstname!".

## Running a script

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

## Building a script

In production you might want to build a script to a binary for performance reasons. Use make like this:

```bash
make build/bin/Script/HelloWorldToAllUsers
```

This will produce a binary `build/bin/Script/HelloWorldToAllUsers` from the source file `Application/Script/HelloWorldToAllUsers.hs`.
