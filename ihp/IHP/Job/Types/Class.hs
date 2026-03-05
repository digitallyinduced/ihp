{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Types.Class
( Job (..)
) where

import IHP.Prelude
import IHP.FrameworkConfig.Types (FrameworkConfig)
import IHP.ModelSupport.Types (ModelContext)
import IHP.Job.Types.BackoffStrategy (BackoffStrategy (..))

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

    -- | How long a job can be in 'JobStatusRunning' before it's considered stale
    -- and recovered. Set to 'Nothing' to disable stale job recovery.
    --
    -- Default: 10 minutes
    staleJobTimeout :: Maybe NominalDiffTime
    staleJobTimeout = Just 600
