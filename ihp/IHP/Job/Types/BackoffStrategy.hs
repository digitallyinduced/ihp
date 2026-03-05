module IHP.Job.Types.BackoffStrategy
( BackoffStrategy (..)
) where

import IHP.Prelude

data BackoffStrategy
    = LinearBackoff { delayInSeconds :: !Int }
    | ExponentialBackoff { delayInSeconds :: !Int }
    deriving (Eq, Show)
