module IHP.IDE.Postgres (waitPostgres, isPostgresReady) where

import IHP.IDE.Types
import IHP.Prelude
import Control.Concurrent (threadDelay)

import System.Log.FastLogger (toLogStr)
import qualified IHP.EnvVar as EnvVar

import qualified Control.Exception.Safe as Exception
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Process as Process
import System.Exit (ExitCode(..))

-- | Blocks until Postgres can serve queries against the application schema.
--
-- Call this right before the app connects, not before it is compiled: the
-- compile runs in parallel with Postgres starting up, so by then this usually
-- returns immediately.
waitPostgres :: (?context :: Context) => IO ()
waitPostgres = go False
    where
        go loggedWaiting = do
            ready <- isPostgresReady
            unless ready do
                -- Mentioned once, and only when we actually have to wait, to keep the
                -- common case (postgres came up while the app was compiling) quiet.
                unless loggedWaiting do
                    ?context.logger (toLogStr ("Waiting for postgres to become ready" :: Text))

                threadDelay 100000 -- 100ms between checks
                go True

-- | Whether Postgres can serve queries against the application schema.
--
-- Accepting connections is not enough. In a devenv shell the postgres process is
-- already up while it still imports IHPSchema.sql, Application/Schema.sql and
-- Application/Fixtures.sql. A query issued during that window hits a schema that
-- is only half there; the resulting error leaves the failed statement in the
-- connection's prepared-statement cache, so the pooled connection answers every
-- later request with @prepared statement "…" does not exist@ until the app is
-- restarted.
--
-- devenv writes @$PGDATA/.devenv_initialized@ once the import has finished — the
-- same marker its own readiness probe uses — so inside a devenv shell that file
-- is checked in addition to @pg_isready@.
--
-- Reports ready when there is nothing to wait for: when @PGHOST@ is unset because
-- the app talks to a database the dev environment doesn't manage, or when
-- @pg_isready@ isn't available to ask.
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
            -- pg_isready returns exit code 0 when ready, non-zero otherwise. When the
            -- binary is missing there's nothing to ask, so we report ready instead of
            -- blocking the dev server forever.
            result <- Exception.tryAny (Process.rawSystem "pg_isready" ["-h", socketDir, "-q"])
            pure $ case result of
                Right ExitSuccess -> True
                Right (ExitFailure _) -> False
                Left _ -> True

        -- devenv's postgres process creates this marker after `initialDatabases` has
        -- been imported. Outside a devenv shell nobody writes it, so there's nothing
        -- to wait for.
        databasesInitialized = do
            isDevenv <- EnvVar.envOrDefault "IHP_DEVENV" False
            pgData :: Maybe String <- EnvVar.envOrNothing "PGDATA"
            case (isDevenv, pgData) of
                (True, Just pgData) -> Directory.doesFileExist (pgData </> ".devenv_initialized")
                _ -> pure True
