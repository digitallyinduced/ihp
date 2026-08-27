module IHP.IDE.Postgres (waitPostgres, waitPostgresWith, isPostgresReady) where

import IHP.IDE.Types
import IHP.Prelude
import Control.Concurrent (threadDelay)

import System.Log.FastLogger (toLogStr)
import qualified IHP.EnvVar as EnvVar

import qualified Control.Exception.Safe as Exception
import qualified GHC.Clock as Clock
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Error as IOError
import qualified System.Process as Process
import System.Exit (ExitCode(..))

-- | Blocks until Postgres can serve queries against the application schema.
--
-- Call this right before the app connects: the compile overlaps with Postgres
-- starting up, so by then it usually returns immediately.
waitPostgres :: (?context :: Context) => IO ()
waitPostgres = waitPostgresWith (?context.logger . toLogStr)

-- | 'waitPostgres' for processes that have no dev server 'Context' to log through.
--
-- Gives up after 'waitTimeout' rather than blocking forever, so a Postgres that
-- never comes up surfaces the app's own connection error instead of a dev server
-- that looks frozen.
waitPostgresWith :: (Text -> IO ()) -> IO ()
waitPostgresWith log = do
    start <- Clock.getMonotonicTime
    let
        -- Wall clock, not accumulated sleep: every poll also spends time spawning
        -- pg_isready, so counting the delays would let the timeout drift.
        elapsed = do
            now <- Clock.getMonotonicTime
            pure (now - start)

        go logAgainAt = do
            ready <- isPostgresReady
            unless ready do
                waited <- elapsed
                if waited >= waitTimeout
                    then log ("Postgres is still not ready after " <> tshow (round waitTimeout :: Int) <> "s, starting anyway")
                    else do
                        -- Logged on the first wait and then every 'logInterval', so the
                        -- common case (postgres came up while the app was compiling) stays
                        -- quiet while a real problem doesn't look like a silent hang.
                        nextLogAt <- if waited < logAgainAt
                            then pure logAgainAt
                            else do
                                log "Waiting for postgres to become ready (pg_isready, $PGDATA/.devenv_initialized)"
                                pure (logAgainAt + logInterval)

                        threadDelay pollInterval
                        go nextLogAt

    go 0
    where
        pollInterval = 100000 :: Int -- 100ms
        logInterval = 5 :: Double -- seconds
        waitTimeout = 60 :: Double -- seconds

-- | Whether Postgres can serve queries against the application schema.
--
-- Accepting connections is not enough: devenv's postgres is up while it still
-- imports the schema, and a query hitting the half-imported schema poisons the
-- pooled connection's prepared-statement cache. devenv writes
-- @$PGDATA/.devenv_initialized@ once the import finished, so that marker is
-- checked too inside a devenv shell.
--
-- Reports ready when there is nothing to wait for: no @PGHOST@ (the database
-- isn't managed by the dev environment), or no @pg_isready@ to ask.
isPostgresReady :: IO Bool
isPostgresReady = do
    socketDir :: Maybe String <- EnvVar.envOrNothing "PGHOST"
    case socketDir of
        Nothing -> pure True
        Just socketDir -> do
            initialized <- databasesInitialized
            if initialized
                then acceptsConnections socketDir
                else pure False
    where
        acceptsConnections socketDir = do
            result <- Exception.tryAny (Process.rawSystem "pg_isready" ["-h", socketDir, "-q"])
            pure case result of
                Right ExitSuccess -> True
                Right (ExitFailure _) -> False
                -- No pg_isready to ask, so there is nothing to wait for. Anything else
                -- (a fork failing under load, say) is transient: report not ready and
                -- let the caller poll again.
                Left exception -> binaryIsMissing exception

        binaryIsMissing exception = case Exception.fromException exception of
            Just ioError -> IOError.isDoesNotExistError ioError
            Nothing -> False

        -- devenv's postgres writes this marker after importing @initialDatabases@.
        -- Outside a devenv shell nobody writes it, so there is nothing to wait for.
        databasesInitialized = do
            isDevenv <- EnvVar.hasEnvVar "IHP_DEVENV"
            pgData :: Maybe String <- EnvVar.envOrNothing "PGDATA"
            case (isDevenv, pgData) of
                (True, Just pgData) -> Directory.doesFileExist (pgData </> ".devenv_initialized")
                _ -> pure True
