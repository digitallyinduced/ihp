module IHP.Job.Types.Status
( JobStatus (..)
) where

import IHP.Prelude

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
