module IHP.Job.Queue.STM
( tryWriteTBQueue
) where

import IHP.Prelude
import Control.Concurrent.STM (TBQueue, writeTBQueue, STM)
import Control.Concurrent.STM.TBQueue (isFullTBQueue)

-- | Non-blocking write to a TBQueue. Returns True if the value was written,
-- False if the queue was full.
tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue queue value = do
    full <- isFullTBQueue queue
    if full
        then pure False
        else do
            writeTBQueue queue value
            pure True
