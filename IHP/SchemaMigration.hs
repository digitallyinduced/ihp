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
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.Time.Clock.POSIX as POSIX
import qualified IHP.NameSupport as NameSupport

data Migration = Migration
    { revision :: Int
    , migrationFile :: Text
    } deriving (Show, Eq)

-- | Migrates the database schema to the latest version
migrate :: (?modelContext :: ModelContext) => IO ()
migrate = do
    createSchemaMigrationsTable

    -- Print out all sql queries during the migration. This might be set to false in it's called inside a production env
    let modelContext = ?modelContext
    let ?modelContext = modelContext { queryDebuggingEnabled = True }

    openMigrations <- findOpenMigrations
    forEach openMigrations runMigration

-- | The sql statements contained in the migration file are executed. Then the revision is inserted into the @schema_migrations@ table.
--
-- All queries are executed inside a database transaction to make sure that it can be restored when something goes wrong.
runMigration :: (?modelContext :: ModelContext) => Migration -> IO ()
runMigration migration@Migration { revision, migrationFile } = do
    migrationSql <- Text.readFile (cs $ migrationPath migration)
    
    withTransaction do
        sqlExec (fromString . cs $ migrationSql) ()
        sqlExec "INSERT INTO schema_migrations (revision) VALUES (?)" [revision]

    pure ()

withTransaction :: (?modelContext :: ModelContext) => IO a -> IO a
withTransaction block = do
    _ <- sqlExec "BEGIN" ()
    result <- block
    _ <- sqlExec "COMMIT" ()
    pure result

-- | Creates the @schema_migrations@ table if it doesn't exist yet
createSchemaMigrationsTable :: (?modelContext :: ModelContext) => IO ()
createSchemaMigrationsTable = do
    let ddl = "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)"
    _ <- sqlExec ddl ()
    pure ()

-- | Returns all migrations that haven't been executed yet. The result is sorted so that the oldest revision is first.
findOpenMigrations :: (?modelContext :: ModelContext) => IO [Migration]
findOpenMigrations = do
    migratedRevisions <- findMigratedRevisions
    migrations <- findAllMigrations
    migrations
        |> filter (\Migration { revision } -> not (migratedRevisions |> includes revision))
        |> sortBy (comparing revision)
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
findAllMigrations :: IO [Migration]
findAllMigrations = do
    directoryFiles <- Directory.listDirectory "Application/Migration"
    directoryFiles
        |> map cs
        |> filter (\path -> ".sql" `isSuffixOf` path)
        |> mapMaybe pathToMigration
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
                |> Text.split (== '-')
                |> head
                |> fmap textToInt
                |> join

migrationPath :: Migration -> Text
migrationPath Migration { migrationFile } = "Application/Migration/" <> migrationFile

-- | Generates a new migration @.sql@ file in @Application/Migration@
createMigration :: Text -> IO Migration
createMigration description = do
    revision <- round <$> POSIX.getPOSIXTime 
    let slug = NameSupport.toSlug description
    let migrationFile = tshow revision <> (if isEmpty slug then "" else "-" <> slug) <> ".sql"
    Text.writeFile ("Application/Migration/" <> cs migrationFile) "-- Write your SQL migration code in here\n"
    pure Migration { .. }