{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Types
( Job (..)
, JobWorkerArgs (..)
, JobWorker (..)
, JobStatus (..)
, Worker (..)
, JobWorkerProcess (..)
, JobWorkerProcessMessage (..)
, BackoffStrategy (..)
)
where

import IHP.Prelude
import IHP.FrameworkConfig
import qualified IHP.PGListener as PGListener
import qualified Control.Concurrent as Concurrent
import Control.Monad.Trans.Resource

data BackoffStrategy
    = LinearBackoff { delayInSeconds :: !Int }
    | ExponentialBackoff { delayInSeconds :: !Int }
    deriving (Eq, Show)

class Job job where
    perform :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => job -> IO ()

    maxAttempts :: (?job :: job) => Int
    maxAttempts = 10

    timeoutInMicroseconds :: Maybe Int
    timeoutInMicroseconds = Nothing

    -- | While jobs are typically fetch using pg_notiy, we have to poll the queue table
    -- periodically to catch jobs with a @run_at@ in the future
    --
    -- By default we only poll every minute
    queuePollInterval :: Int
    queuePollInterval = 60 * 1000000

    -- | How many jobs of this type can be executed at the same time
    --
    -- This limit only applies to the running haskell process. If you run @N@ multiple
    -- independent processes of the job runner, the limit will be @N * maxConcurrency@
    maxConcurrency :: Int
    maxConcurrency = 16

    backoffStrategy :: BackoffStrategy
    backoffStrategy = LinearBackoff { delayInSeconds = 30 }

class Worker application where
    workers :: application -> [JobWorker]

data JobWorkerArgs = JobWorkerArgs
    { workerId :: UUID
    , modelContext :: ModelContext
    , frameworkConfig :: FrameworkConfig
    , pgListener :: PGListener.PGListener
    }

newtype JobWorker = JobWorker (JobWorkerArgs -> ResourceT IO JobWorkerProcess)

-- | Mapping for @JOB_STATUS@. The DDL statement for this can be found in IHPSchema.sql:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
data JobStatus
    = JobStatusNotStarted
    | JobStatusRunning
    | JobStatusFailed
    | JobStatusTimedOut
    | JobStatusSucceeded
    | JobStatusRetry
    deriving (Eq, Show, Read, Enum)

data JobWorkerProcess
    = JobWorkerProcess
    { runners :: [(ReleaseKey, Async ())]
    , subscription :: PGListener.Subscription
    , pollerReleaseKey :: ReleaseKey
    , action :: Concurrent.MVar JobWorkerProcessMessage
    }

data JobWorkerProcessMessage
    = JobAvailable
    | Stop
