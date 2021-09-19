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

### Viewing job status

A benefit of jobs compared to just running scripts is info about the jobs is stored persistently in the database. To see the status of a job, inspect its `#status` field. If the job failed, you can see the error that caused it to fail in the field `#lastError`.

### Configuring jobs

Every job has two options you can configure:
- maximum number of attempts
- timeout

#### Attempts

When a job fails, it is automatically retried up to 10 times, or until it succeeds. If you want to configure this number, you can set `maxAttempts` to a custom value, like in this example:

```haskell
instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
      customers <- query @Customer |> fetch
      forEach customers sendToCustomer
      where
        sendToCustomer customer = sendMail (MarketingMail customer)

    maxAttempts = 3
```

#### Timeout

Sometimes you might want a job to stop if its runtime exceeds some threshold. For that case, there's a `timeoutInMicroseconds` option which you can set for you job. The default is no timeout. If you want your job to time out after some time, set it to a `Just Int` value, like in the following example, which causes the job to time out after one minute:

```haskell
instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
      customers <- query @Customer |> fetch
      forEach customers sendToCustomer
      where
        sendToCustomer customer = sendMail (MarketingMail customer)

    timeoutInMicroseconds = Just $ 1000000 * 60
```

A timed out job will be retried, just as if it failed. If you want to prevent that, set its `maxAttempts` to `0`, as shown above.

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

In one of the `FrontController.hs` files for your project (for example, `Web/FrontController.hs` by default), define a type for your dashboard.

```haskell
import IHP.Job.Dashboard

type MyJobDashboard = JobsDashboardController
    (BasicAuthStatic "jobs" "jobs")
    '[]
```

With the empty list type `'[]`, the job dashboard will render a default dashboard for all the tables in the database ending in `_jobs`. See below for a guide on adding custom dashboards for your job types.

To add the dashboard to your application, you need to add a `parseRoute` call to this type.

```haskell
type MyJobDashboard = JobsDashboardController
    (BasicAuthStatic "jobs" "jobs")
    '[UpdateUserJob, CleanDatabaseJob, OtherRandomJob]

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
    '[UpdateUserJob, CleanDatabaseJob, OtherRandomJob]
```

### Jobs to include

The third type parameter to `JobsDashboardController` is a list of job types that the dashboard will display using their custom `DisplayableJob` implementations. See the documentation for `IHP.Job.Dashboard` for details.

### Customize views

Most views in the dashboard can be customized by providing a custom implementation of `DisplayableJob` for your job type.
These methods can be overriden to allow for custom behavior:

```haskell
makeDashboardSection :: (?context::ControllerContext, ?modelContext::ModelContext) => IO SomeView
```

How this job's section should be displayed in the dashboard. By default it's displayed as a table,
but this can be any arbitrary view! Make some cool graphs :)

```haskell
makePageView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO SomeView
```
A paginated list view of jobs. First parameter is the page number, second is the page size.

```haskell
makeDetailView :: (?context::ControllerContext, ?modelContext::ModelContext) => job -> IO SomeView
```
The content of the page that will be displayed for a detail view of this job.
By default, the ID, Status, Created/Updated at times, and last error are displayed.
Can be defined as any arbitrary view.

```haskell
makeNewJobView :: (?context::ControllerContext, ?modelContext::ModelContext) => IO SomeView
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
To add more data, define your own implementation. See the case study below for a real world example.

### TableViewable

The jobs dashboard provides a `TableViewable` type class that makes it easier to customize the appearance of jobs in the dashboard.
The typeclass defines behavior for how the type should be rendered as a table.

```haskell
class TableViewable a where
    tableTitle :: Text -- human readable title displayed on the table
    modelTableName :: Text -- database table backing the view
    tableHeaders :: [Text]
    renderTableRow :: a -> Html
    newJobLink :: Html -- link used in the table to send user to new job form
    getIndex :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO [a]
    getPage :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO [a]
```

See the below case study for how to implement your own instance.

### Case study: Attics app

One common use to to show some data from a record the job belongs to. Here's how it's done in the Attics app, for example.
In this case, each "Initial Scrape Job" is run for a single band. I wanted to display the name of the band in the table for the job to
make the table more useful. So I defined a TableViewable instance for `InitialScrapeJob` with its band info included.

``` haskell
instance TableViewable (IncludeWrapper "bandId" InitialScrapeJob) where
    modelTableName = tableName @InitialScrapeJob
    tableTitle = tableName @InitialScrapeJob |> columnNameToFieldLabel
    tableHeaders = ["Band", "Updated at", "Status", ""]

    getIndex =
        query @InitialScrapeJob
            |> limit 10
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    getPage page pageSize = do
        query @InitialScrapeJob
            |> offset (page * pageSize)
            |> limit pageSize
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @InitialScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
            link = "/jobs/CreateJob?tableName=" <> tableName @InitialScrapeJob  <> "&bandId=" <> get #bandId job |> get #id |> tshow
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job |> timeAgo}</td>
            <td>{statusToBadge $ get #status job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
            <td>
                <form action={link} method="POST">
                    <button type="submit" style={retryButtonStyle} class="btn btn-link text-secondary">Retry</button>
                </form>
            </td>
        </tr>
    |]

    newJobLink = let
            table = tableName @InitialScrapeJob
        in [hsx|
            <form action={CreateJobAction table}>
                <button type="submit" class="btn btn-primary btn-sm">+ New Job</button>
            </form>
        |]

```

Note the use of `IncludeWrapper` instead of the normal `Include`. This gets around an unfortunate limitation in Haskell's type system.
This instance is then used by a custom `DisplayableJob` instance that uses some helpers from `IHP.Job.Dashboard.View` that render the dashboard section and list pages using any `TableViewable` instance.

```haskell
instance DisplayableJob InitialScrapeJob where
    makeDashboardSection = makeDashboardSectionFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makePageView = makeListPageFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makeDetailView job = do
        let table = tableName @InitialScrapeJob
        withRelated <- fetchRelated #bandId job
        pure $ SomeView $ HtmlView $ [hsx|
            <br>
                <h5>Viewing Job {get #id job} in {table}</h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Band</th>
                        <td>{get #bandId withRelated |> get #name}</td>
                    </tr>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job |> timeAgo}</td>
                    </tr>
                    <tr>
                        <th>Status</th>
                        <td>{statusToBadge (get #status job)}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>

            <div class="d-flex flex-row">
                <form class="mr-2" action="/jobs/DeleteJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="id" name="id" value={tshow $ get #id job}>
                    <button type="submit" class="btn btn-danger">Delete</button>
                </form>
                <form action="/jobs/CreateJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="bandId" name="bandId" value={get #bandId job |> tshow}>
                    <button type="submit" class="btn btn-primary">Run again</button>
                </form>
            </div>
        |]

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

Then, we can add the `InitialScrapeJob` type to our Dashboard type list. In `Admin/FrontController.hs`,

```haskell
type AtticsJobDashboard = JobsDashboardController
    (BasicAuthStatic "<redacted>" "<redacted>")
    '[InitialScrapeJob]

instance FrontController AdminApplication where
    controllers =
        [ startPage BandsAction
        -- Generator Marker
        , parseRoute @BandsController
        , parseRoute @AtticsJobDashboard
        ]
```

If you use many of these custom jobs, it'll be worth abstracting some of the HTML in these functions to avoid excessive duplication.
