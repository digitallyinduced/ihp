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


## Jobs Dashboard

IHP comes with a built in jobs dashboard for running and viewing your jobs. To define which jobs
you want displayed in the dashboard, you construct a custom JobDashboard type that IHP will use to determine the dashboard's behavior.

### Quick Start

```haskell
import IHP.Job.Dashboard

type MyJobDashboard = JobsDashboardController
    (BasicAuthStatic "jobs" "jobs")
    [UpdateUserJob, CleanDatabaseJob, OtherRandomJob]
```

To add the dashboard to your application, you need to add a `parseRoute` call in one of your FrontControllers.
For example, in `Web/FrontController.hs`, use this type as follows:

```haskell
type MyJobDashboard = JobsDashboardController
    (BasicAuthStatic "jobs" "jobs")
    [UpdateUserJob, CleanDatabaseJob, OtherRandomJob]

instance FrontController WebApplication where
    controllers =
        [ startPage HomeView
        , parseRoute @ProductsController
        , parseRoute @MyJobDashboard
        ]
```

This will mount a jobs dashboard under `/jobs/`.

### Authentication

The second type parameter to `JobsDashboardController` is a type that defines how users should be authenticated when visiting any of the jobs pages.
IHP provides three types for common use cases.
- `NoAuth`: No authentication
- `BasicAuth`: HTTP Basic Auth using environment variables
- `BasicAuthStatic`: HTTP Basic Auth using static values

To use your own authentication, create an empty type and define an instance of `AuthenticationMethod` for it. Example:

```haskell
data CustomAuth
instance AuthenticationMethod CustomAuth where
    authenticate = do
        myCustomAuthenticateFunction
```

Then use it in the dashboard type definition.

```haskell
type MyJobDashboard = JobsDashboardController
    CustomAuth
    [UpdateUserJob, CleanDatabaseJob, OtherRandomJob]
```

### Jobs to include

The third type parameter to `JobsDashboardController` is a list of job types that the dashboard will display. If you create a job type through the IHP IDE,
you can just add the name of the generated type to the list and it will be added to the dashboard with a default implementation.

Any type that conforms to `DisplayableJob` can be added to the dashboard. See the documentation for `IHP.Job.Dashboard` for details.

### Customize views

Most views in the dashboard can be customized by providing a custom implementation of `DisplayableJob` for your job type.
These methods can be overriden to allow for custom behavior:

```haskell
makeSection :: (?modelContext :: ModelContext) => IO SomeView
```

How this job's section should be displayed in the dashboard. By default it's displayed as a table,
but this can be any arbitrary view! Make some cool graphs :)

```haskell
makeDetailView :: (?modelContext :: ModelContext) => job -> IO SomeView
```
The content of the page that will be displayed for a detail view of this job.
By default, the ID, Status, Created/Updated at times, and last error are displayed.
Can be defined as any arbitrary view.

```haskell
makeNewJobView :: (?modelContext :: ModelContext) => IO SomeView
```
The content of the page that will be displayed for the "new job" form of this job.
By default, only the submit button is rendered. For additonal form data, define your own implementation.
See 'GenericNewJobView' for a guide on how it can look.look
Can be defined as any arbitrary view, but it should be a form.

```haskell
createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
```
The action run to create and insert a new value of this job into the database.
By default, create an empty record and insert it.
To add more data, define your own implementation.

### TableViewable

The jobs dashboard provides a `TableViewable` type class that makes it easier to customize the appearance of jobs in the dashboard.
The typeclass defines behavior for how the type should be rendered as a table.

```haskell
class TableViewable a where
    tableTitle :: Text
    tableHeaders :: [Text]
    createNewForm :: Html
    renderTableRow :: a -> Html
    renderTable :: [a] -> Html
```

See the below case study for how to implement your own instance.

### Case study: Attics app

One common use to to show some data from a record the job belongs to. Here's how it's done in the Attics app, for example:

``` haskell
instance TableViewable (IncludeWrapper "bandId" InitialScrapeJob) where
    tableTitle = "Initial Scrape Job"
    tableHeaders = ["Band", "Updated at", "Status", ""]
    createNewForm = newJobFormForTableHeader @InitialScrapeJob
    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @InitialScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job}</td>
            <td>{statusToBadge $ get #status job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
        </tr>
    |]
```

Note the use of `IncludeWrapper` instead of the normal `Include`. This gets around an unfortunate limitation in Haskell's type system.
This instance is then used by a custom `DisplayableJob` instance that handles the logic of fetching the parent records.

```haskell
instance {-# OVERLAPS #-} DisplayableJob InitialScrapeJob where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        jobsWithBand <- query @InitialScrapeJob
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map (IncludeWrapper @"bandId" @InitialScrapeJob)
        pure (SomeView (TableView jobsWithBand))

    makeDetailView :: (?modelContext :: ModelContext) => InitialScrapeJob -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ InitialScrapeJobForm job

    makeNewJobView = do
        bands <- query @Band |> fetch
        pure $ SomeView $ HtmlView $ form newRecord bands
        where
            form :: InitialScrapeJob -> [Band] -> Html
            form job bands = formFor' job "/jobs/CreateJob" [hsx|
                {selectField #bandId bands}
                <input type="hidden" id="tableName" name="tableName" value={getTableName job}>
                <button type="submit" class="btn btn-primary">Run again</button>
            |]

    createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    createNewJob = do
        let bandId = param "bandId"
        newRecord @InitialScrapeJob |> set #bandId bandId |> create
        pure ()
```
