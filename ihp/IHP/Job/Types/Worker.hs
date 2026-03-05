module IHP.Job.Types.Worker
( Worker (..)
, JobWorkerArgs (..)
, JobWorker (..)
, JobWorkerProcess (..)
, JobWorkerProcessMessage (..)
) where

import IHP.Prelude
import IHP.FrameworkConfig.Types (FrameworkConfig)
import IHP.ModelSupport.Types (ModelContext)
import qualified IHP.PGListener as PGListener
import Control.Monad.Trans.Resource (ResourceT, ReleaseKey)
import Control.Concurrent.STM (TBQueue, TVar)
import Control.Concurrent.Async (Async)

class Worker application where
    workers :: application -> [JobWorker]

data JobWorkerArgs = JobWorkerArgs
    { workerId :: UUID
    , modelContext :: ModelContext
    , frameworkConfig :: FrameworkConfig
    , pgListener :: PGListener.PGListener
    }

newtype JobWorker = JobWorker (JobWorkerArgs -> ResourceT IO JobWorkerProcess)

data JobWorkerProcess
    = JobWorkerProcess
    { dispatcher :: (ReleaseKey, Async ())
    , subscription :: PGListener.Subscription
    , pollerReleaseKey :: ReleaseKey
    , action :: TBQueue JobWorkerProcessMessage
    , staleRecoveryReleaseKey :: Maybe ReleaseKey
    , activeCount :: TVar Int
    }

data JobWorkerProcessMessage
    = JobAvailable
    | Stop
