module IHP.IDE.SchemaDesigner.Controller.Migrations where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Migrations.Index
import IHP.IDE.SchemaDesigner.View.Migrations.New
import IHP.IDE.SchemaDesigner.View.Migrations.Edit

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout, findStatementByName, replace, findForeignKey, findTableIndex)
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations
import qualified IHP.SchemaMigration as SchemaMigration
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import IHP.IDE.CodeGen.Controller
import IHP.IDE.ToolServer.Helper.Controller (openEditor, clearDatabaseNeedsMigration)
import IHP.Log.Types
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

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
                migrateAppDB revision

        clearDatabaseNeedsMigration

        redirectTo MigrationsAction

    action EditMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        sqlStatements <- readSqlStatements migration

        render EditView { .. }

    action UpdateMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        let sqlStatements = param "sqlStatements"

        Text.writeFile (cs $ SchemaMigration.migrationPath migration) sqlStatements

        redirectTo MigrationsAction

    action DeleteMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId
        let path = cs $ SchemaMigration.migrationPath migration

        Directory.removeFile path

        redirectTo MigrationsAction

    action RunMigrationAction { migrationId } = do
        migration <- findMigrationByRevision migrationId

        result <- Exception.try (migrateAppDB migrationId)
        case result of
            Left (exception :: SomeException) -> do
                let errorMessage = case fromException exception of
                        Just (exception :: EnhancedSqlError) -> cs $ get #sqlErrorMsg (get #sqlError exception)
                        Nothing -> tshow exception
                
                setErrorMessage errorMessage
                redirectTo MigrationsAction
            Right _ -> do
                clearDatabaseNeedsMigration
                redirectTo MigrationsAction

readSqlStatements :: SchemaMigration.Migration -> IO Text
readSqlStatements migration = Text.readFile (cs $ SchemaMigration.migrationPath migration)

findRecentMigrations :: IO [SchemaMigration.Migration]
findRecentMigrations = take 20 . reverse <$> SchemaMigration.findAllMigrations

findMigrationByRevision :: Int -> IO SchemaMigration.Migration
findMigrationByRevision migrationRevision = do
    migrations <- findRecentMigrations
    let (Just migration) = migrations |> find (\SchemaMigration.Migration { revision } -> revision == migrationRevision)
    pure migration

migrateAppDB :: Int -> IO ()
migrateAppDB revision = withAppModelContext do
    let minimumRevision = Just (revision - 1)
    SchemaMigration.migrate SchemaMigration.MigrateOptions { minimumRevision }

findMigratedRevisions :: IO [Int]
findMigratedRevisions = withAppModelContext SchemaMigration.findMigratedRevisions

withAppModelContext :: ((?modelContext :: ModelContext) => IO result) -> IO result
withAppModelContext inner =
        Exception.bracket initModelContext cleanupModelContext callback
    where
        callback (frameworkConfig, logger, modelContext) = let ?modelContext = modelContext in inner
        initModelContext = do
            frameworkConfig <- buildFrameworkConfig (pure ())
            logger <- defaultLogger

            modelContext <- createModelContext
                (get #dbPoolIdleTime frameworkConfig)
                (get #dbPoolMaxConnections frameworkConfig)
                (get #databaseUrl frameworkConfig)
                logger

            pure (frameworkConfig, logger, modelContext)

        cleanupModelContext (frameworkConfig, logger, modelContext) = do
            logger |> cleanup