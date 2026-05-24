{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.Job.Runner
( runJobWorkers
, dedicatedProcessMainLoop
, installSignalHandlers
, stopExitHandler
, worker
, jobWorkerFetchAndRunLoop
) where

import IHP.Job.Runner.MainLoop
    ( runJobWorkers
    , dedicatedProcessMainLoop
    , installSignalHandlers
    , stopExitHandler
    )
import IHP.Job.Runner.WorkerLoop
    ( worker
    , jobWorkerFetchAndRunLoop
    )
