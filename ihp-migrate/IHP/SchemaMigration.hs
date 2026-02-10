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
import Data.Int (Int64)
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)
import qualified Data.Char as Char

import qualified System.Directory.OsPath as Directory
import System.OsPath (OsPath, encodeUtf, decodeUtf)

import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

data Migration = Migration
    { revision :: Int
    , migrationFile :: Text
    } deriving (Show, Eq)

data MigrateOptions = MigrateOptions
    { minimumRevision :: !(Maybe Int) -- ^ When deploying a fresh install of an existing app that has existing migrations, it might be useful to ignore older migrations as they're already part of the existing schema
    }

-- | Run a session on the bare connection, dying on error
runSession :: Connection.Connection -> Session.Session a -> IO a
runSession connection session = Connection.use connection session >>= either (die . show) pure

-- | Migrates the database schema to the latest version
migrate :: Connection.Connection -> MigrateOptions -> IO ()
migrate connection options = do
    createSchemaMigrationsTable connection

    let minimumRevision = fromMaybe 0 options.minimumRevision

    openMigrations <- findOpenMigrations connection minimumRevision
    forM_ openMigrations (runMigration connection)

-- | The sql statements contained in the migration file are executed. Then the revision is inserted into the @schema_migrations@ table.
--
-- All queries are executed inside a database transaction to make sure that it can be restored when something goes wrong.
--
-- We split the migration SQL into individual statements and execute each one
-- separately via 'Session.script'. This is necessary because hasql 1.10's
-- 'Session.script' uses @PQsendQuery@ + @singleResult@ internally, which only
-- handles a single result. Multi-statement SQL produces multiple results and
-- leaves unconsumed results on the connection, causing subsequent operations
-- to fail with "cannot enter pipeline mode, connection not idle".
runMigration :: Connection.Connection -> Migration -> IO ()
runMigration connection Migration { revision, migrationFile } = do
    migrationFilePath <- migrationPath Migration { revision, migrationFile }
    migrationSql <- Text.readFile (cs migrationFilePath)

    let statements = splitStatements migrationSql
    runSession connection do
        Session.script "BEGIN"
        forM_ statements Session.script
        Session.statement (fromIntegral revision :: Int64) insertRevisionStatement
        Session.script "COMMIT"

insertRevisionStatement :: Statement.Statement Int64 ()
insertRevisionStatement =
    Statement.preparable
        "INSERT INTO schema_migrations (revision) VALUES ($1)"
        (Encoders.param (Encoders.nonNullable Encoders.int8))
        Decoders.noResult

checkSchemaMigrationsExistsStatement :: Statement.Statement () (Maybe Text)
checkSchemaMigrationsExistsStatement =
    Statement.preparable
        "SELECT (to_regclass('schema_migrations')) :: text"
        Encoders.noParams
        (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text)))

selectMigratedRevisionsStatement :: Statement.Statement () [Int64]
selectMigratedRevisionsStatement =
    Statement.preparable
        "SELECT revision FROM schema_migrations ORDER BY revision"
        Encoders.noParams
        (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Creates the @schema_migrations@ table if it doesn't exist yet
createSchemaMigrationsTable :: Connection.Connection -> IO ()
createSchemaMigrationsTable connection = do
    -- We don't use CREATE TABLE IF NOT EXISTS as adds a "NOTICE: relation schema_migrations already exists, skipping"
    -- This sometimes confuses users as they don't know if the this is an error or not (it's not)
    -- https://github.com/digitallyinduced/ihp/issues/818
    maybeTableName <- runSession connection (Session.statement () checkSchemaMigrationsExistsStatement)
    let schemaMigrationTableExists = isJust maybeTableName

    unless schemaMigrationTableExists do
        runSession connection (Session.script "CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE)")

-- | Returns all migrations that haven't been executed yet. The result is sorted so that the oldest revision is first.
findOpenMigrations :: Connection.Connection -> Int -> IO [Migration]
findOpenMigrations connection !minimumRevision = do
    migratedRevisions <- findMigratedRevisions connection
    migrations <- findAllMigrations
    migrations
        & filter (\Migration { revision } -> not (revision `elem` migratedRevisions))
        & filter (\Migration { revision } -> revision > minimumRevision)
        & pure

-- | Returns all migration revisions applied to the database schema
--
-- >>> findMigratedRevisions connection
-- [ 1604850570, 1604850660 ]
--
findMigratedRevisions :: Connection.Connection -> IO [Int]
findMigratedRevisions connection = do
    revisions <- runSession connection (Session.statement () selectMigratedRevisionsStatement)
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

-- | Split SQL text into individual statements by unquoted semicolons.
--
-- Respects single-quoted string literals (with @''@ escapes) and
-- @--@ line comments so that semicolons inside those are not treated
-- as statement separators.
splitStatements :: Text -> [Text]
splitStatements = filter (not . Text.null) . map Text.strip . splitOnUnquotedSemicolons
  where
    splitOnUnquotedSemicolons :: Text -> [Text]
    splitOnUnquotedSemicolons sql = case nextSemicolon sql of
        Nothing -> [sql]
        Just (before, after) -> before : splitOnUnquotedSemicolons after

    -- | Find the next unquoted semicolon. Returns the text before it
    -- and the text after it, or Nothing if there is no unquoted semicolon.
    nextSemicolon :: Text -> Maybe (Text, Text)
    nextSemicolon = go ""
      where
        go acc sql
            | Text.null sql = Nothing
            | otherwise =
                -- Skip ahead to the next interesting character
                let (chunk, rest) = Text.break (\c -> c == ';' || c == '\'' || c == '-') sql
                    acc' = acc <> chunk
                in case Text.uncons rest of
                    Nothing -> Nothing
                    Just (';', rest') -> Just (acc', rest')
                    Just ('\'', rest') ->
                        let (str, after) = spanString rest'
                        in go (acc' <> "'" <> str <> "'") after
                    Just ('-', rest') -> case Text.uncons rest' of
                        Just ('-', rest'') ->
                            let (comment, after) = Text.break (== '\n') rest''
                            in go (acc' <> "--" <> comment) after
                        _ -> go (acc' <> "-") rest'
                    _ -> Nothing -- unreachable

    -- | Consume a single-quoted string body (after the opening quote).
    -- Returns the string contents (without quotes) and the remaining text
    -- after the closing quote.
    spanString :: Text -> (Text, Text)
    spanString sql =
        let (chunk, rest) = Text.break (== '\'') sql
        in case Text.uncons rest of
            Nothing -> (chunk, "") -- unterminated string
            Just ('\'', rest') -> case Text.uncons rest' of
                Just ('\'', rest'') ->
                    -- Escaped quote ''
                    let (more, after) = spanString rest''
                    in (chunk <> "''" <> more, after)
                _ -> (chunk, rest') -- end of string
            _ -> (chunk, rest) -- unreachable
