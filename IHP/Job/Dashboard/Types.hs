{-|
Module: IHP.Job.Dashboard.Types
Description:  Types for Job dashboard
-}
module IHP.Job.Dashboard.Types (
    BaseJob(..),
    JobsDashboardController(..),
) where

import IHP.Prelude
import IHP.ControllerPrelude
import GHC.TypeLits
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import IHP.RouterPrelude hiding (get, tshow, error, map, putStrLn, elem)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import IHP.Job.Queue () -- get FromField definition for JobStatus

import IHP.Job.Dashboard.Auth

data BaseJob = BaseJob {
    table :: Text
  , id :: UUID
  , status :: JobStatus
  , updatedAt :: UTCTime
  , createdAt :: UTCTime
  , lastError :: Maybe Text
} deriving (Show)

instance FromRow BaseJob where
    fromRow = BaseJob <$> field <*> field <*> field <*> field <*> field <*> field

-- | Defines controller actions for acting on a dashboard made of some list of types.
-- Later functions and typeclasses introduce constraints on the types in this list,
-- so you'll get a compile error if you try and include a type that is not a job.
data JobsDashboardController authType (jobs :: [*])
    = ListJobsAction
    | ListJobAction { jobTableName :: Text, page :: Int }
    -- These actions are used for 'pathTo'. Need  to pass the parameters explicity to know how to build the path
    | ViewJobAction { jobTableName :: Text, jobId :: UUID }
    | CreateJobAction { jobTableName :: Text }
    | DeleteJobAction { jobTableName :: Text, jobId :: UUID }

    -- These actions are used for interal routing. Parameters are extracted dynamically in the action based on types
    | ListJobAction'
    | ViewJobAction'
    | CreateJobAction'
    | DeleteJobAction'
    deriving (Show, Eq, Data)


instance HasPath (JobsDashboardController authType jobs) where
    pathTo ListJobsAction = "/jobs/ListJobs"
    pathTo ListJobAction   { .. } = "/jobs/ListJob?tableName=" <> jobTableName <> "&page=" <> tshow page
    pathTo ViewJobAction   { .. } = "/jobs/ViewJob?tableName=" <> jobTableName <> "&id=" <> tshow jobId
    pathTo CreateJobAction { .. } = "/jobs/CreateJob?tableName=" <> jobTableName
    pathTo DeleteJobAction { .. } = "/jobs/DeleteJob?tableName=" <> jobTableName <> "&id=" <> tshow jobId
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

