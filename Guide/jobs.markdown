# Jobs

```toc

```

## Introduction

IHP has built-in functionality for creating and running background jobs. Jobs are perfect for any tasks that can be split up into small units and run in parallel, such periodically cleaning the database, sending emails, and scraping data.

### Creating a job

In the codegen tool in the IHP IDE, use the "Background Job" option to generate the code for a new job. To illustrate the features of jobs, let's
create a job to send an email to our application's customers.

### Implementing the job

The job file is created at `<Application>/Job/<Name>.hs`, and looks like:

```haskell
module Web.Job.EmailCustomers where
import Web.Controller.Prelude

instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
        putStrLn "Hello World!"
```

The type `EmailCustomersJob` is a generated type based on the table the background job codegen added to `Schema.sql`. If you want to include additional data in the job, you can add rows to the table in the IHP IDE just like any custom table.

A pseudocode implementation of a job that sends a marketing email to all of the customers:

```haskell
module Web.Job.EmailCustomers where
import Web.Controller.Prelude

instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
      customers <- query @Customer |> fetch
      forEach customers sendToCustomer
      where
        sendToCustomer customer = sendMail (MarketingMail customer)
```

### Running the job

IHP watches the job table in the database for any new records and automatically run the job asynchronously when a new job is added. So to run a job, simply create a new record:

```haskell
newRecord @EmailCustomersJob |> create
```

This can be done in a controller action or in a script as will be shown below.

#### Development vs. Production

In development mode, these watchers are started with the dev server. In production however, use `make build/bin/RunJobs` to build a binary that you can deploy along side your IHP app to watch for added jobs and run them.

**NOTE: Jobs running automatically in development are currently broken on v0.9. Please run on master if you need this until the next release**

### Viewing job status

A benefit of jobs compared to just running scripts is info about the jobs is stored persistently in the database. To see the status of a job, inspect it's `#status` field. If the job failed, you can see the error that caused it to fail in the field `#lastError`.

### Scheduling jobs with cron

IHP does not (yet) have built-in functionality to schedule jobs to run on a regular basis. Instead we can build a binary that will start a job with an IHP script and use `cron` to schedule the binary to run daily, hourly, or whatever your use case requires.

Generate a new script with the "Script" option in the IHP IDE codegen tool. In the generated file `Application/Script/<Script Name>.hs`, simply create a job record as described above:

```haskell
#!/usr/bin/env run-script
module Application.Script.RunEmailCustomersJob where

import Application.Script.Prelude

run :: Script
run = do
  newRecord @EmailCustomersJob |> create
  pure ()
```

Build this script into a binary with `make build/bin/Script/RunEmailCustomersJob` as described in the [scripts documentation](/Guide/scripts.html).

We can then create a cron entry such as

```
0 5 * * *  root bin/Script/RunEmailCustomersJob
```

to schedule a job to be run every day at 5am. See [this article by DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-cron-to-automate-tasks-ubuntu-1804) for more information on `cron`.


