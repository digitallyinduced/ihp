{-|
Module: IHP.SchemaMigration
Description: Managing Database Migrations
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.SchemaMigration where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ModelSupport hiding (withTransaction)
import qualified Data.Time.Clock.POSIX as POSIX
import qualified IHP.NameSupport as NameSupport
import qualified Data.Char as Char
import IHP.Log.Types

data Migration = Migration
    { revision :: Int
    , migrationFile :: Text
    } deriving (Show, Eq)

data MigrateOptions = MigrateOptions
    { minimumRevision :: !(Maybe Int) -- ^ When deploying a fresh install of an existing app that has existing migrations, it might be useful to ignore older migrations as they're already part of the existing schema
    }

-- | Migrates the database schema to the latest version
migrate :: (?modelContext :: ModelContext) => MigrateOptions -> IO ()
migrate options = do
    createSchemaMigrationsTable

    let minimumRevision = options
            |> get #minimumRevision
            |> fromMaybe 0

    openMigrations <- findOpenMigrations minimumRevision
    forEach openMigrations runMigration

-- | The sql statements contained in the migration file are executed. Then the revision is inserted into the @schema_migrations@ table.
--
-- All queries are executed inside a database transaction to make sure that it can be restored when something goes wrong.
runMigration :: (?modelContext :: ModelContext) => Migration -> IO ()
runMigration migration@Migration { revision, migrationFile } = do
    migrationSql <- Text.readFile (cs $ migrationPath migration)

    let fullSql = [trimming|
        BEGIN;
            ${migrationSql}
            INSERT INTO schema_migrations (revision) VALUES (?);
        COMMIT;
    |]
    sqlExec (fromString . cs $ fullSql) [revision]

    pure ()

-- | Creates the @schema_migrations@ table if it doesn't exist yet
createSchemaMigrationsTable :: (?modelContext :: ModelContext) => IO ()
createSchemaMigrationsTable = do
    -- Hide this query from the log
    let modelContext = ?modelContext
    let ?modelContext = modelContext { logger = (get #logger modelContext) { write = \_ -> pure ()} }

    -- We don't use CREATE TABLE IF NOT EXISTS as adds a "NOTICE: relation schema_migrations already exists, skipping"
    -- This sometimes confuses users as they don't know if the this is an error or not (it's not)
    -- https://github.com/digitallyinduced/ihp/issues/818
    maybeTableName :: Maybe Text <- sqlQueryScalar "SELECT (to_regclass('schema_migrations')) :: text" ()
    let schemaMigrationTableExists = isJust maybeTableName

    unless schemaMigrationTableExists do
        let ddl = "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)"
        _ <- sqlExec ddl ()
        pure ()

-- | Returns all migrations that haven't been executed yet. The result is sorted so that the oldest revision is first.
findOpenMigrations :: (?modelContext :: ModelContext) => Int -> IO [Migration]
findOpenMigrations !minimumRevision = do
    let modelContext = ?modelContext
    let ?modelContext = modelContext { logger = (get #logger modelContext) { write = \_ -> pure ()} }

    migratedRevisions <- findMigratedRevisions
    migrations <- findAllMigrations
    migrations
        |> filter (\Migration { revision } -> not (migratedRevisions |> includes revision))
        |> filter (\Migration { revision } -> revision > minimumRevision)
        |> pure

-- | Returns all migration revisions applied to the database schema
--
-- >>> findMigratedRevisions
-- [ 1604850570, 1604850660 ]
--
findMigratedRevisions :: (?modelContext :: ModelContext) => IO [Int]
findMigratedRevisions = map (\[revision] -> revision) <$> sqlQuery "SELECT revision FROM schema_migrations ORDER BY revision" ()

-- | Returns all migrations found in @Application/Migration@
--
-- >>> findAllMigrations
-- [ Migration { revision = 1604850570, migrationFile = "Application/Migration/1604850570-create-projects.sql" } ]
--
-- The result is sorted so that the oldest revision is first.
findAllMigrations :: IO [Migration]
findAllMigrations = do
    directoryFiles <- Directory.listDirectory "Application/Migration"
    directoryFiles
        |> map cs
        |> filter (\path -> ".sql" `isSuffixOf` path)
        |> mapMaybe pathToMigration
        |> sortBy (comparing revision)
        |> pure

-- | Given a path such as Application/Migrate/00-initial-migration.sql it returns a Migration
--
-- Returns Nothing if the path is not following the usual migration file path convention.
--
-- >>> pathToMigration "Application/Migration/1604850570-create-projects.sql"
-- Migration { revision = 1604850570, migrationFile = "Application/Migration/1604850570-create-projects.sql" }
--
pathToMigration :: Text -> Maybe Migration
pathToMigration fileName = case revision of
        Just revision -> Just Migration { migrationFile = fileName, revision }
        Nothing -> Nothing
    where
        revision :: Maybe Int
        revision = fileName
                |> Text.split (not . Char.isDigit)
                |> head
                |> fmap textToInt
                |> join

migrationPath :: Migration -> Text
migrationPath Migration { migrationFile } = "Application/Migration/" <> migrationFile