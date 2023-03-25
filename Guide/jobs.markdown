# Jobs

```toc

```

## Introduction

IHP has built-in functionality for creating and running background jobs. Jobs are perfect for any tasks that can be split up into small units and run in parallel, such as periodically cleaning the database, sending emails, and scraping data.

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

IHP watches the job table in the database for any new records and automatically runs the job asynchronously when a new job is added. So to run a job, simply create a new record:

```haskell
newRecord @EmailCustomersJob |> create
```

This can be done in a controller action or in a script as will be shown below.

#### Development vs. Production

In development mode, these watchers are started with the dev server. In production however, use `make build/bin/RunJobs` to build a binary that you can deploy along side your IHP app to watch for added jobs and run them.

### Viewing job status

A benefit of jobs compared to just running scripts is getting info about the jobs stored persistently in the database. To see the status of a job, inspect its `#status` field. If the job failed, you can see the error that caused it to fail in the field `#lastError`.

### Configuring jobs

Every job has a few options you can configure:
- maximum number of attempts
- timeout
- maximum number of concurrent processing

#### Attempts

When a job fails, it is automatically retried up to 10 times, or until it succeeds. If you want to configure this number, you can set [`maxAttempts`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Types.html#v:maxAttempts) to a custom value, like in this example:

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

Sometimes you might want a job to stop if its runtime exceeds some threshold. For that case, there's a [`timeoutInMicroseconds`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Types.html#v:timeoutInMicroseconds) option which you can set for your job. The default is no timeout. If you want your job to time out after some time, set it to a `Just Int` value, like in the following example, which causes the job to time out after one minute:

```haskell
instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
      customers <- query @Customer |> fetch
      forEach customers sendToCustomer
      where
        sendToCustomer customer = sendMail (MarketingMail customer)

    timeoutInMicroseconds = Just $ 1000000 * 60
```

A timed out job will be retried, just as if it failed. If you want to prevent that, set its [`maxAttempts`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Types.html#v:maxAttempts) to `0`, as shown above.


### Concurrency

Sometimes you would like to change the number of jobs that can be processed at the same time. By default the value is set to 16. A possible use case would be changing the number to 1 in order to make sure jobs are processed one after the other. This means jobs can be useful when there's a need for an ordered queue:


```haskell
instance Job EmailCustomersJob where
    perform EmailCustomersJob { .. } = do
      -- ...

    maxConcurrency = 1
```


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

We can then create a cron entry such as:

```
0 5 * * *  root bin/Script/RunEmailCustomersJob
```

to schedule a job to be run every day at 5am. See [this article by DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-cron-to-automate-tasks-ubuntu-1804) for more information on `cron`.


## Jobs Dashboard

IHP comes with a built in jobs dashboard for running and viewing your jobs. To define which jobs
you want displayed in the dashboard, you construct a custom `JobDashboard` type that IHP will use to determine the dashboard's behavior.

### Quick Start

In one of the `FrontController.hs` files for your project (for example, `Web/FrontController.hs` by default), define a type for your dashboard:

```haskell
import IHP.Job.Dashboard

type MyJobDashboard = JobsDashboardController
    (BasicAuthStatic "jobs" "jobs")
    '[]
```

With the empty list type `'[]`, the job dashboard will render a default dashboard for all the tables in the database ending in `_jobs`. See below for a guide on adding custom dashboards for your job types.

To add the dashboard to your application, you need to add a [`parseRoute`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseRoute) call to this type:

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

If you would like to link to the Jobs page you could add for example on `Layout.hs`:

```haskell
import IHP.Job.Dashboard

navbar :: Html
navbar = [hsx|
    <nav>
        <!-- ... -->
        <a href={ListJobsAction}>Jobs</a>
    </nav>
|]
```

### Authentication

The second type parameter to [`JobsDashboardController`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:JobsDashboardController) is a type that defines how users should be authenticated when visiting any of the jobs pages.
IHP provides three types for common use cases.
- `NoAuth`: No authentication
- `BasicAuth`: HTTP Basic Auth using environment variables
- `BasicAuthStatic`: HTTP Basic Auth using static values

To use your own authentication, create an empty type and define an instance of [`AuthenticationMethod`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Auth.html#t:AuthenticationMethod) for it:

```haskell
data CustomAuth
instance AuthenticationMethod CustomAuth where
    authenticate = do
        myCustomAuthenticateFunction
```

Then use it in the dashboard type definition:

```haskell
type MyJobDashboard = JobsDashboardController
    CustomAuth
    '[UpdateUserJob, CleanDatabaseJob, OtherRandomJob]
```

### Jobs to include

The third type parameter to [`JobsDashboardController`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:JobsDashboardController) is a list of job types that the dashboard will display using their custom [`DisplayableJob`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard.html#t:DisplayableJob) implementations. See the documentation for [`IHP.Job.Dashboard`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard.html) for details.

### Customize views

Most views in the dashboard can be customized by providing a custom implementation of [`DisplayableJob`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard.html#t:DisplayableJob) for your job type.
These methods can be overriden to allow for custom behavior:

```haskell
makeDashboardSection :: (?context::ControllerContext, ?modelContext::ModelContext) => IO SomeView
```

How this job's section should be displayed in the dashboard? By default it's displayed as a table,
but this can be any arbitrary view! Make some cool graphs :)

```haskell
makePageView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO SomeView
```
A paginated list view of jobs. First parameter is the page number, second is the page size:

```haskell
makeDetailView :: (?context::ControllerContext, ?modelContext::ModelContext) => job -> IO SomeView
```
The content of the page that will be displayed for a detail view of this job.
By default, the ID, Status, Created-/Updated-at times, and last error are displayed.
Can be defined as any arbitrary view:

```haskell
makeNewJobView :: (?context::ControllerContext, ?modelContext::ModelContext) => IO SomeView
```
The content of the page that will be displayed for the "new job" form of this job.
By default, only the submit button is rendered. For additonal form data, define your own implementation.
See `GenericNewJobView` for a guide on how it can look like.
It can be defined as any arbitrary view, but it should be a form:

```haskell
createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
```
The action run to create and insert a new value of this job into the database.
By default, create an empty record and insert it.
To add more data, define your own implementation. See the case study below for a real world example.

### TableViewable

The jobs dashboard provides a [`TableViewable`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:TableViewable) type class that makes it easier to customize the appearance of jobs in the dashboard.
The typeclass defines behavior for how the type should be rendered as a table:

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

One common use case is to show some data from a record the job belongs to. Here's how it's done in the Attics app, for example.
In this case, each "Initial Scrape Job" is run for a single band. I wanted to display the name of the band in the table for the job to
make the table more useful. So I defined a `TableViewable` instance for `InitialScrapeJob` with its band info included:

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
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow job.id
            link = "/jobs/CreateJob?tableName=" <> tableName @InitialScrapeJob  <> "&bandId=" <> job.bandId.id |> tshow
        in [hsx|
        <tr>
            <td>{job.bandId.name}</td>
            <td>{job.updatedAt |> timeAgo}</td>
            <td>{statusToBadge job.status}</td>
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

Note the use of [`IncludeWrapper`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:IncludeWrapper) instead of the normal [`Include`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:IncludeWrapper). This gets around an unfortunate limitation in Haskell's type system.
This instance is then used by a custom [`DisplayableJob`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard.html#t:DisplayableJob) instance that uses some helpers from `IHP.Job.Dashboard.View` that will render the dashboard section and list pages using any [`TableViewable`](https://ihp.digitallyinduced.com/api-docs/IHP-Job-Dashboard-Types.html#t:TableViewable) instance:

```haskell
instance DisplayableJob InitialScrapeJob where
    makeDashboardSection = makeDashboardSectionFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makePageView = makeListPageFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makeDetailView job = do
        let table = tableName @InitialScrapeJob
        withRelated <- fetchRelated #bandId job
        pure $ SomeView $ HtmlView $ [hsx|
            <br>
                <h5>Viewing Job {job.id} in {table}</h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Band</th>
                        <td>{withRelated.bandId.name}</td>
                    </tr>
                    <tr>
                        <th>Updated At</th>
                        <td>{job.updatedAt}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{job.createdAt |> timeAgo}</td>
                    </tr>
                    <tr>
                        <th>Status</th>
                        <td>{statusToBadge job.status}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" job.lastError}</td>
                    </tr>
                </tbody>
            </table>

            <div class="d-flex flex-row">
                <form class="mr-2" action="/jobs/DeleteJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="id" name="id" value={tshow job.id}>
                    <button type="submit" class="btn btn-danger">Delete</button>
                </form>
                <form action="/jobs/CreateJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="bandId" name="bandId" value={job.bandId |> tshow}>
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

Then, we can add the `InitialScrapeJob` type to our Dashboard type list. In `Admin/FrontController.hs`:

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
