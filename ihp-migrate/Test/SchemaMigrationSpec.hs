{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import Test.Hspec
import Prelude
import IHP.SchemaMigration
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Temp.OsPath (withSystemTempDirectory)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.OsPath (encodeUtf, decodeUtf)
import System.Environment (lookupEnv)
import qualified Control.Exception as Exception
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

main :: IO ()
main = hspec do
    tests

tests = do
    describe "IHP.SchemaMigration" do
        describe "findAllMigrations" do
            it "should find all migrations" do
                withTempApp do
                    migrations <- findAllMigrations

                    migrations `shouldBe`
                            [ Migration { revision = 1605721927, migrationFile = "1605721927.sql"}
                            , Migration { revision = 1605721940, migrationFile = "1605721940-create-users.sql" }
                            ]

            it "should fail when multiple migrations use the same timestamp" do
                withTempMigrationFiles ["1605721927.sql", "1605721927-create-users.sql"] do
                    findAllMigrations `shouldThrow` \exception ->
                        let message = ioeGetErrorString exception
                        in isUserError exception
                            && "Multiple migrations use the same timestamp" `isInfixOf` message
                            && "1605721927.sql" `isInfixOf` message
                            && "1605721927-create-users.sql" `isInfixOf` message

        describe "classifyMigration" do
            it "classifies standalone extension migrations for the admin connection" do
                classifyMigration "-- Extension dependencies\nCREATE EXTENSION IF NOT EXISTS cube;\nCREATE EXTENSION IF NOT EXISTS \"earthdistance\";"
                    `shouldBe` Right CreateExtensionMigration

            it "classifies regular migrations for the application connection" do
                classifyMigration "CREATE TABLE users (id UUID PRIMARY KEY);"
                    `shouldBe` Right RegularMigration

            it "does not treat extension keywords inside strings as extension migrations" do
                classifyMigration "SELECT 'CREATE EXTENSION postgis'; SELECT $$ CREATE EXTENSION cube $$;"
                    `shouldBe` Right RegularMigration

            it "rejects extension migrations without IF NOT EXISTS" do
                classifyMigration "CREATE EXTENSION postgis;"
                    `shouldBe` Left "CREATE EXTENSION migrations must use IF NOT EXISTS"

            it "rejects migrations mixing extensions with other statements" do
                classifyMigration "CREATE EXTENSION IF NOT EXISTS postgis; CREATE TABLE places (id UUID);" `shouldSatisfy`
                    (\result -> case result of
                        Left message -> "must be standalone" `Text.isInfixOf` message
                        Right _ -> False
                    )

            it "accepts PostgreSQL extension options" do
                classifyMigration "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public VERSION '3.4' CASCADE;"
                    `shouldBe` Right CreateExtensionMigration

        describe "extractCreateExtensionsSql" do
            it "extracts extension statements from a complete schema" do
                extractCreateExtensionsSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\"; CREATE TABLE users ();"
                    `shouldBe` Right "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

            it "rejects non-idempotent extension declarations" do
                extractCreateExtensionsSql "CREATE EXTENSION postgis;"
                    `shouldBe` Left "Schema.sql CREATE EXTENSION statements must use IF NOT EXISTS"

        describe "database integration" do
            it "uses the admin only for extension creation and records revisions as the application role" do
                maybeDatabaseUrl <- lookupEnv "IHP_MIGRATE_TEST_DATABASE_URL"
                case maybeDatabaseUrl of
                    Nothing -> pendingWith "IHP_MIGRATE_TEST_DATABASE_URL is not configured for PostgreSQL integration tests"
                    Just databaseUrl -> withTempMigrationContents
                        [ ("1-create-extension.sql", "CREATE EXTENSION IF NOT EXISTS plpgsql;")
                        , ("2-regular.sql", "CREATE TABLE regular_migration_table (id BIGINT);")
                        ] do
                            withConnection databaseUrl \adminConnection ->
                                withApplicationDatabase adminConnection \(applicationDatabaseUrl, applicationAdminDatabaseUrl) ->
                                    withConnection applicationDatabaseUrl \applicationConnection ->
                                    withConnection applicationAdminDatabaseUrl \applicationAdminConnection -> do
                                        createSchemaMigrationsTable applicationConnection
                                        runTestSession applicationConnection (Session.script auditTriggerSql)

                                        extensionResult <- runMigrationWithAdminConnection applicationConnection (Just applicationAdminConnection)
                                            Migration { revision = 1, migrationFile = "1-create-extension.sql" }
                                        extensionResult `shouldBe` Right ()

                                        rolesAfterExtension <- runTestSession applicationConnection (Session.statement () auditRolesStatement)
                                        rolesAfterExtension `shouldBe` [applicationRole]

                                        let unavailableAdminConnection :: IO (Maybe Connection.Connection)
                                            unavailableAdminConnection = expectationFailure "regular migration acquired the admin connection" >> pure Nothing
                                        regularResult <- runMigrationWithAdminConnectionProvider applicationConnection unavailableAdminConnection
                                            Migration { revision = 2, migrationFile = "2-regular.sql" }
                                        regularResult `shouldBe` Right ()

                                        rolesAfterRegularMigration <- runTestSession applicationConnection (Session.statement () auditRolesStatement)
                                        rolesAfterRegularMigration `shouldBe` [applicationRole, applicationRole]


withTempApp :: IO a -> IO a
withTempApp =
    withTempMigrationFiles
        [ "1605721927.sql"
        , "1605721940-create-users.sql"
        ]

withTempMigrationFiles :: [FilePath] -> IO a -> IO a
withTempMigrationFiles migrationFiles =
    withTempMigrationContents (map (\migrationFile -> (migrationFile, "")) migrationFiles)

withTempMigrationContents :: [(FilePath, String)] -> IO a -> IO a
withTempMigrationContents migrationFiles action = do
    template <- encodeUtf "ihp-migrate-test"
    withSystemTempDirectory template \tmpOsPath -> do
        tmp <- decodeUtf tmpOsPath
        let appRoot      = tmp
        let migrationDir = appRoot </> "Application" </> "Migration"

        -- Create the directory structure
        Directory.createDirectoryIfMissing True migrationDir

        -- Create migration files and one non-migration file
        forM_ migrationFiles \(migrationFile, migrationSql) -> do
            writeFile (migrationDir </> migrationFile) migrationSql
        writeFile (migrationDir </> "not_a_migration") ""

        -- Now run findAllMigrations as if this were an IHP app root
        Directory.withCurrentDirectory appRoot action

applicationRole :: Text
applicationRole = "ihp_migrate_privilege_test_app"

applicationDatabase :: Text
applicationDatabase = "ihp_migrate_privilege_test_db"

withApplicationDatabase :: Connection.Connection -> ((String, String) -> IO a) -> IO a
withApplicationDatabase adminConnection action =
    Exception.bracket_ setup cleanup do
        pgHost <- lookupEnv "PGHOST" >>= maybe (expectationFailure "PGHOST is not configured" >> pure "") pure
        let applicationDatabaseUrl = "host=" <> pgHost <> " dbname=" <> cs applicationDatabase <> " user=" <> cs applicationRole
        let applicationAdminDatabaseUrl = "host=" <> pgHost <> " dbname=" <> cs applicationDatabase
        action (applicationDatabaseUrl, applicationAdminDatabaseUrl)
    where
        setup = runTestSession adminConnection do
            Session.script (cs ("DROP DATABASE IF EXISTS " <> applicationDatabase <> " WITH (FORCE);"))
            Session.script (cs ("DROP ROLE IF EXISTS " <> applicationRole <> ";"))
            Session.script (cs ("CREATE ROLE " <> applicationRole <> " LOGIN;"))
            Session.script (cs ("CREATE DATABASE " <> applicationDatabase <> " OWNER " <> applicationRole <> ";"))
        cleanup = runTestSession adminConnection do
            Session.script (cs ("DROP DATABASE IF EXISTS " <> applicationDatabase <> " WITH (FORCE);"))
            Session.script (cs ("DROP ROLE IF EXISTS " <> applicationRole <> ";"))

withConnection :: String -> (Connection.Connection -> IO a) -> IO a
withConnection databaseUrl action = do
    connectionResult <- Connection.acquire (ConnectionSettings.connectionString (cs databaseUrl))
    case connectionResult of
        Left error -> expectationFailure (show error) >> fail "Could not connect to the integration test database"
        Right connection -> Exception.finally (action connection) (Connection.release connection)

runTestSession :: Connection.Connection -> Session.Session a -> IO a
runTestSession connection session = do
    result <- Connection.use connection session
    case result of
        Left error -> expectationFailure (show error) >> fail "Database integration test failed"
        Right value -> pure value

auditTriggerSql :: Text
auditTriggerSql =
    "CREATE TABLE migration_revision_audit (role_name TEXT NOT NULL);\n"
    <> "CREATE FUNCTION audit_migration_revision_role() RETURNS TRIGGER\n"
    <> "LANGUAGE plpgsql AS $$\n"
    <> "BEGIN\n"
    <> "    INSERT INTO migration_revision_audit (role_name) VALUES (current_user);\n"
    <> "    RETURN NEW;\n"
    <> "END;\n"
    <> "$$;\n"
    <> "CREATE TRIGGER audit_migration_revision_role\n"
    <> "    AFTER INSERT ON schema_migrations\n"
    <> "    FOR EACH ROW EXECUTE FUNCTION audit_migration_revision_role();\n"

auditRolesStatement :: Statement.Statement () [Text]
auditRolesStatement =
    Statement.preparable
        "SELECT role_name FROM migration_revision_audit ORDER BY ctid"
        Encoders.noParams
        (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))
