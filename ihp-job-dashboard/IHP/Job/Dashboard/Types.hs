{-|
Module: IHP.Job.Dashboard.Types
Description:  Types for Job dashboard
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Dashboard.Types (
    BaseJob(..),
    JobsDashboardController(..),
    TableViewable(..),
    IncludeWrapper(..),
    baseJobDecoder,
    sqlIdentifier,
) where

import IHP.Prelude
import IHP.ControllerPrelude
import IHP.ViewPrelude (Html)
import IHP.RouterPrelude hiding (get, tshow, error, map, putStrLn, elem)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import IHP.Job.Queue () -- get FromField definition for JobStatus
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Data.Text as Text


data BaseJob = BaseJob {
    table :: Text
  , id :: UUID
  , status :: JobStatus
  , updatedAt :: UTCTime
  , createdAt :: UTCTime
  , lastError :: Maybe Text
} deriving (Show)

class TableViewable a where
    -- | Human readable title displayed on the table
    tableTitle :: Text

    -- | Database table backing the view
    modelTableName :: Text

    tableHeaders :: [Text]
    renderTableRow :: a -> Html

    -- | Link used in the table to send user to new job form
    newJobLink :: Html

    -- | Gets records for displaying in the dashboard index page
    getIndex :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO [a]

    -- | Gets paginated records for displaying in the list page
    getPage :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Int -> Int -> IO [a]

instance FromRow BaseJob where
    fromRow = BaseJob <$> field <*> field <*> field <*> field <*> field <*> field

jobStatusDecoder :: Decoders.Value JobStatus
jobStatusDecoder = Decoders.enum (Just "public") "job_status" \case
    "job_status_not_started" -> Just JobStatusNotStarted
    "job_status_running" -> Just JobStatusRunning
    "job_status_failed" -> Just JobStatusFailed
    "job_status_timed_out" -> Just JobStatusTimedOut
    "job_status_succeeded" -> Just JobStatusSucceeded
    "job_status_retry" -> Just JobStatusRetry
    _ -> Nothing

baseJobDecoder :: Decoders.Row BaseJob
baseJobDecoder = BaseJob
    <$> Decoders.column (Decoders.nonNullable Decoders.text)        -- table
    <*> Decoders.column (Decoders.nonNullable Decoders.uuid)        -- id
    <*> Decoders.column (Decoders.nonNullable jobStatusDecoder)     -- status
    <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz) -- updatedAt
    <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz) -- createdAt
    <*> Decoders.column (Decoders.nullable Decoders.text)           -- lastError

-- | Safely quote a SQL identifier (table name) by escaping double quotes.
sqlIdentifier :: Text -> Snippet.Snippet
sqlIdentifier name = Snippet.sql ("\"" <> Text.replace "\"" "\"\"" name <> "\"")

-- | Often, jobs are related to some model type. These relations are modeled through the type system.
-- For example, the type 'Include "userId" UpdateUserJob' models an 'UpdateUserJob' type that can access
-- the 'User' it belongs to through the 'userId' field.
-- For some reason, GHC doesn't allow us to create implementations of type family applications, so the following doesn't work:
--
-- > instance DisplayableJob (Include "userId" UpdateUserJob) where
--
-- However, if we wrap this in a concrete type, it works fine. That's what this wrapper is for.
-- To get the same behavior as above, just define
--
-- > instance DisplayableJob (IncludeWrapper "userId" UpdateUserJob) where
--
-- and wrap the values as so:
--
-- > jobsWithUsers <- query @UpdateUserJob
-- >    |> fetch
-- >    >>= mapM (fetchRelated #userId)
-- >    >>= pure . map (IncludeWrapper @"userId" @UpdateUserJob)
newtype IncludeWrapper (id :: Symbol) job = IncludeWrapper (Include id job)

-- | Defines controller actions for acting on a dashboard made of some list of types.
-- Later functions and typeclasses introduce constraints on the types in this list,
-- so you'll get a compile error if you try and include a type that is not a job.
data JobsDashboardController authType (jobs :: [Type])
    = ListJobsAction
    | ListJobAction { jobTableName :: Text, page :: Int }
    -- These actions are used for 'pathTo'. Need  to pass the parameters explicity to know how to build the path
    | ViewJobAction { jobTableName :: Text, jobId :: UUID }
    | CreateJobAction { jobTableName :: Text }
    | DeleteJobAction { jobTableName :: Text, jobId :: UUID }
    | RetryJobAction { jobTableName :: Text, jobId :: UUID }

    -- These actions are used for interal routing. Parameters are extracted dynamically in the action based on types
    | ListJobAction'
    | ViewJobAction'
    | CreateJobAction'
    | DeleteJobAction'
    | RetryJobAction'
    deriving (Show, Eq, Data)


instance HasPath (JobsDashboardController authType jobs) where
    pathTo ListJobsAction = "/jobs/ListJobs"
    pathTo ListJobAction   { .. } = "/jobs/ListJob?tableName=" <> jobTableName <> "&page=" <> tshow page
    pathTo ViewJobAction   { .. } = "/jobs/ViewJob?tableName=" <> jobTableName <> "&id=" <> tshow jobId
    pathTo CreateJobAction { .. } = "/jobs/CreateJob?tableName=" <> jobTableName
    pathTo DeleteJobAction { .. } = "/jobs/DeleteJob?tableName=" <> jobTableName <> "&id=" <> tshow jobId
    pathTo RetryJobAction  { .. } = "/jobs/RetryJob?tableName=" <> jobTableName <> "&id=" <> tshow jobId
    pathTo _ = error "pathTo for internal JobsDashboard functions not supported. Use non-backtick action and pass necessary parameters to use pathTo."

instance CanRoute (JobsDashboardController authType jobs) where
    parseRoute' = do
        (string "/jobs" <* endOfInput >> pure ListJobsAction)
        <|> (string "/jobs/" <* endOfInput >> pure ListJobsAction)
        <|> (string "/jobs/ListJobs" <* endOfInput >> pure ListJobsAction)
        <|> (string "/jobs/ListJob" <* endOfInput >> pure ListJobAction')
        <|> (string "/jobs/ViewJob" <* endOfInput >> pure ViewJobAction')
        <|> (string "/jobs/CreateJob" <* endOfInput >> pure CreateJobAction')
        <|> (string "/jobs/DeleteJob" <* endOfInput >> pure DeleteJobAction')
        <|> (string "/jobs/RetryJob" <* endOfInput >> pure RetryJobAction')

