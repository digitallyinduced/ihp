module IHP.IDE.Postgres (waitPostgres) where

import IHP.IDE.Types
import IHP.Prelude
import Control.Concurrent (threadDelay)

import System.Log.FastLogger (toLogStr)
import qualified IHP.EnvVar as EnvVar

import qualified System.Process as Process
import System.Exit (ExitCode(..))

waitPostgres :: (?context :: Context) => IO ()
waitPostgres = do
    let isDebugMode = ?context.isDebugMode
    socketDir <- EnvVar.env @String "PGHOST"

    -- pg_isready returns exit code 0 when ready, non-zero otherwise
    exitCode <- Process.rawSystem "pg_isready" ["-h", socketDir, "-q"]
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
            when isDebugMode (?context.logger (toLogStr ("Waiting for postgres to start" :: Text) <> "\n"))
            threadDelay 100000  -- 100ms between checks
            waitPostgres
