module IHP.Job.Types
  ( Job (..),
    JobWorkerArgs (..),
    JobWorker (..),
    JobStatus (..),
    Worker (..),
  )
where

import qualified Control.Concurrent.Async as Async
import IHP.FrameworkConfig
import IHP.Prelude

class Job job where
  perform :: (?modelContext :: ModelContext, ?frameworkConfig :: FrameworkConfig) => job -> IO ()

  maxAttempts :: (?job :: job) => Int
  maxAttempts = 10

class Worker application where
  workers :: application -> [JobWorker]

data JobWorkerArgs = JobWorkerArgs
  { allJobs :: IORef [Async.Async ()],
    workerId :: UUID,
    modelContext :: ModelContext,
    frameworkConfig :: FrameworkConfig
  }

newtype JobWorker = JobWorker (JobWorkerArgs -> IO (Async.Async ()))

-- | Mapping for @JOB_STATUS@. The DDL statement for this can be found in IHPSchema.sql:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
data JobStatus
  = JobStatusNotStarted
  | JobStatusRunning
  | JobStatusFailed
  | JobStatusSucceeded
  | JobStatusRetry
  deriving (Eq, Show, Read, Enum)
