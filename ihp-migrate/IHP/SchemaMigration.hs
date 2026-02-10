{-|
Module: IHP.SchemaMigration
Description: Managing Database Migrations
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.SchemaMigration where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe, mapMaybe, isJust, listToMaybe)
import Data.List (sortBy, filter, isSuffixOf)
import Data.Ord (comparing)
import Data.Function ((&))
import Control.Monad (unless, forM_, join)
import Control.Exception (throwIO, Exception, bracket)
import Data.Int (Int64)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Char as Char

import qualified System.Directory.OsPath as Directory
import System.OsPath (OsPath, encodeUtf, decodeUtf)

import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Sessions
import qualified Hasql.Errors as Errors

data Migration = Migration
    { revision :: Int
    , migrationFile :: Text
    } deriving (Show, Eq)

data MigrateOptions = MigrateOptions
    { minimumRevision :: !(Maybe Int) -- ^ When deploying a fresh install of an existing app that has existing migrations, it might be useful to ignore older migrations as they're already part of the existing schema
    }

-- | Exception type for hasql errors
newtype HasqlError = HasqlError Errors.SessionError
    deriving (Show)

instance Exception HasqlError

-- | Run a session on the bare connection, throwing on error
runSession :: (?connection :: Connection.Connection) => Session.Session a -> IO a
runSession session = Connection.use ?connection session >>= either (throwIO . HasqlError) pure

-- | Run a dynamic snippet on the bare connection, throwing on error
runSnippet :: (?connection :: Connection.Connection) => Snippet.Snippet -> Decoders.Result a -> IO a
runSnippet snippet decoder = runSession (Session.statement () (Snippet.toStatement snippet decoder))

-- | Migrates the database schema to the latest version
migrate :: (?connection :: Connection.Connection) => MigrateOptions -> IO ()
migrate options = do
    createSchemaMigrationsTable

    let minimumRevision = fromMaybe 0 options.minimumRevision

    openMigrations <- findOpenMigrations minimumRevision
    forM_ openMigrations runMigration

-- | The sql statements contained in the migration file are executed. Then the revision is inserted into the @schema_migrations@ table.
--
-- All queries are executed inside a database transaction to make sure that it can be restored when something goes wrong.
runMigration :: (?connection :: Connection.Connection) => Migration -> IO ()
runMigration migration@Migration { revision, migrationFile } = do
    -- | User can specify migrations directory as environment variable (defaults to /Application/Migrations/...)
    migrationFilePath <- migrationPath migration
    migrationSql <- Text.readFile (cs migrationFilePath)

    let transaction = do
            Transaction.sql (cs migrationSql)
            Transaction.statement (fromIntegral revision :: Int64) insertRevisionStatement
    runSession (Sessions.transaction Sessions.ReadCommitted Sessions.Write transaction)

insertRevisionStatement :: Statement.Statement Int64 ()
insertRevisionStatement =
    Statement.preparable
        "INSERT INTO schema_migrations (revision) VALUES ($1)"
        (Encoders.param (Encoders.nonNullable Encoders.int8))
        Decoders.noResult

-- | Creates the @schema_migrations@ table if it doesn't exist yet
createSchemaMigrationsTable :: (?connection :: Connection.Connection) => IO ()
createSchemaMigrationsTable = do
    -- We don't use CREATE TABLE IF NOT EXISTS as adds a "NOTICE: relation schema_migrations already exists, skipping"
    -- This sometimes confuses users as they don't know if the this is an error or not (it's not)
    -- https://github.com/digitallyinduced/ihp/issues/818
    maybeTableName :: Maybe Text <- runSnippet (Snippet.sql "SELECT (to_regclass('schema_migrations')) :: text") (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text)))
    let schemaMigrationTableExists = isJust maybeTableName

    unless schemaMigrationTableExists do
        runSession (Session.script "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)")

-- | Returns all migrations that haven't been executed yet. The result is sorted so that the oldest revision is first.
findOpenMigrations :: (?connection :: Connection.Connection) => Int -> IO [Migration]
findOpenMigrations !minimumRevision = do
    migratedRevisions <- findMigratedRevisions
    migrations <- findAllMigrations
    migrations
        & filter (\Migration { revision } -> not (revision `elem` migratedRevisions))
        & filter (\Migration { revision } -> revision > minimumRevision)
        & pure

-- | Returns all migration revisions applied to the database schema
--
-- >>> findMigratedRevisions
-- [ 1604850570, 1604850660 ]
--
findMigratedRevisions :: (?connection :: Connection.Connection) => IO [Int]
findMigratedRevisions = do
    revisions <- runSnippet (Snippet.sql "SELECT revision FROM schema_migrations ORDER BY revision") (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8)))
    pure (map fromIntegral revisions)

-- | Returns all migrations found in @Application/Migration@
--
-- >>> findAllMigrations
-- [ Migration { revision = 1604850570, migrationFile = "Application/Migration/1604850570-create-projects.sql" } ]
--
-- The result is sorted so that the oldest revision is first.
findAllMigrations :: IO [Migration]
findAllMigrations = do
    migrationDir <- detectMigrationDir
    migrationDirOsPath <- encodeUtf (cs migrationDir)
    directoryFiles <- Directory.listDirectory migrationDirOsPath
    fileNames <- mapM decodeUtf directoryFiles
    let textNames = map (cs :: FilePath -> Text) fileNames
    textNames
        & filter (\path -> ".sql" `Text.isSuffixOf` path)
        & mapMaybe pathToMigration
        & sortBy (comparing revision)
        & pure

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
                & Text.split (not . Char.isDigit)
                & listToMaybe
                & fmap (readMaybe . Text.unpack)
                & join

migrationPath :: Migration -> IO Text
migrationPath Migration { migrationFile } = do
    migrationDir <- detectMigrationDir
    pure (migrationDir <> migrationFile)

detectMigrationDir :: IO Text
detectMigrationDir = do
    envValue <- lookupEnv "IHP_MIGRATION_DIR"
    pure (maybe "Application/Migration/" cs envValue)
