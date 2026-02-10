module IHP.IDE.SchemaDesigner.Controller.Migrations where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Migrations.Index
import IHP.IDE.SchemaDesigner.View.Migrations.New
import IHP.IDE.SchemaDesigner.View.Migrations.Edit

import IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout)

import qualified Data.Text.IO as Text

import qualified IHP.SchemaMigration as SchemaMigration
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import IHP.IDE.CodeGen.Controller
import IHP.IDE.ToolServer.Helper.Controller (openEditor, clearDatabaseNeedsMigration)
import qualified Control.Exception.Safe as Exception
import qualified System.Directory.OsPath as Directory
import qualified Database.PostgreSQL.Simple as PG
import System.OsPath (encodeUtf)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings

instance Controller MigrationsController where
    beforeAction = setLayout schemaDesignerLayout

    action MigrationsAction = do
        migrations <- findRecentMigrations
        migratedRevisions <- findMigratedRevisions

        migrationsWithSql <- forM migrations $ \migration -> do
                sql <- readSqlStatements migration
                pure (migration, sql)

        lastError <- getSessionAndClear "last_migraton_error"
        render IndexView { .. }


    action NewMigrationAction = do
        let description = paramOrDefault "" "description"
        (_, plan) <- MigrationGenerator.buildPlan description Nothing
        let runMigration = paramOrDefault True "runMigration"
        render NewView { .. }

    action CreateMigrationAction = do
        let description = paramOrDefault "" "description"
        let sqlStatements = paramOrNothing "sqlStatements"
        (revision, plan) <- MigrationGenerator.buildPlan description sqlStatements
        let path = MigrationGenerator.migrationPathFromPlan plan

        executePlan plan

        let createOnly = paramOrDefault False "createOnly"
        if createOnly
            then do
                setSuccessMessage ("Migration generated: " <> path)
                openEditor path 0 0
            else do
                result <- Exception.try (migrateAppDB revision)
                case result of
                    Left (exception :: SomeException) -> do
                        let errorMessage = case fromException exception of
                                Just (exception :: EnhancedSqlError) -> cs exception.sqlError.sqlErrorMsg
                                Nothing -> tshow exception

                        setErrorMessage errorMessage
                        redirectTo MigrationsAction
                    Right _ -> do
                        clearDatabaseNeedsMigration
                        redirectTo MigrationsAction

        redirectTo MigrationsAction

    action EditMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        sqlStatements <- readSqlStatements migration

        render EditView { .. }

    action UpdateMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        let sqlStatements = param "sqlStatements"
        migrationFilePath <- SchemaMigration.migrationPath migration
        Text.writeFile (cs migrationFilePath) sqlStatements

        redirectTo MigrationsAction

    action DeleteMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        path <- SchemaMigration.migrationPath migration
        osPath <- encodeUtf (cs path)

        Directory.removeFile osPath

        redirectTo MigrationsAction

    action RunMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId

        result <- Exception.try (migrateAppDB migrationId)
        case result of
            Left (exception :: SomeException) -> do
                let errorMessage = case fromException exception of
                        Just (exception :: EnhancedSqlError) -> cs exception.sqlError.sqlErrorMsg
                        Nothing -> tshow exception

                setErrorMessage errorMessage
                redirectTo MigrationsAction
            Right _ -> do
                clearDatabaseNeedsMigration
                redirectTo MigrationsAction

readSqlStatements :: SchemaMigration.Migration -> IO Text
readSqlStatements migration = do
    migrationFilePath <- (SchemaMigration.migrationPath migration)
    Text.readFile (cs migrationFilePath)

findRecentMigrations :: IO [SchemaMigration.Migration]
findRecentMigrations = take 20 . reverse <$> SchemaMigration.findAllMigrations

findMigrationByRevision :: Int -> IO SchemaMigration.Migration
findMigrationByRevision migrationRevision = do
    migrations <- findRecentMigrations
    let (Just migration) = migrations |> find (\SchemaMigration.Migration { revision } -> revision == migrationRevision)
    pure migration

migrateAppDB :: Int -> IO ()
migrateAppDB revision = withMigrateConnection do
    let minimumRevision = Just (revision - 1)
    SchemaMigration.migrate SchemaMigration.MigrateOptions { minimumRevision }

findMigratedRevisions :: IO [Int]
findMigratedRevisions = emptyListIfTablesDoesntExists (withMigrateConnection SchemaMigration.findMigratedRevisions)
    where
        -- The schema_migrations table might not have been created yet
        -- In that case there cannot be any migrations that have been run yet
        emptyListIfTablesDoesntExists operation = do
            result <- Exception.try operation
            case result of
                Left (exception :: SomeException)
                    | "schema_migrations" `isInfixOf` tshow exception -> pure []
                    | otherwise -> Exception.throwIO exception
                Right result -> pure result

withMigrateConnection :: ((?connection :: Connection.Connection) => IO result) -> IO result
withMigrateConnection inner = Exception.bracket acquire Connection.release use
    where
        acquire = do
            frameworkConfig <- buildFrameworkConfig (pure ())
            Connection.acquire (ConnectionSettings.connectionString (cs frameworkConfig.databaseUrl))
                >>= either (\e -> error ("DB connect failed: " <> show e)) pure
        use conn = let ?connection = conn in inner
