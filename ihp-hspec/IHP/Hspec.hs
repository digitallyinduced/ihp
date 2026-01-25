module IHP.Hspec (withIHPApp) where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import IHP.IDE.CodeGen.MigrationGenerator (findIHPSchemaSql)

import qualified Data.Vault.Lazy as Vault
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))

import IHP.ControllerSupport (InitControllerContext)
import IHP.FrameworkConfig (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.ModelSupport (createModelContext)
import IHP.Log.Types
import IHP.Controller.Session (sessionVaultKey)

import qualified System.Process as Process
import IHP.Test.Mocking (MockContext(..), runTestMiddlewares)

withConnection databaseUrl = Exception.bracket (PG.connectPostgreSQL databaseUrl) PG.close

-- | Create contexts that can be used for mocking
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        let FrameworkConfig { dbPoolMaxConnections, dbPoolIdleTime } = frameworkConfig

        logger <- newLogger def { level = Warn } -- don't log queries

        withTestDatabase frameworkConfig.databaseUrl \testDatabaseUrl -> do
            modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections testDatabaseUrl logger

            -- Use the central test middleware stack
            let baseVault = Vault.insert sessionVaultKey mempty Vault.empty
            let baseRequest = defaultRequest { vault = baseVault }
            mockRequest <- runTestMiddlewares frameworkConfig modelContext [] baseRequest
            let mockRespond = const (pure ResponseReceived)

            hspecAction MockContext { .. }

withTestDatabase masterDatabaseUrl callback = do
    testDatabaseName <- randomDatabaseName

    withConnection masterDatabaseUrl \masterConnection ->
        Exception.bracket_
            (PG.execute masterConnection "CREATE DATABASE ?" [PG.Identifier testDatabaseName])
            (
                -- The WITH FORCE is required to force close open connections
                PG.execute masterConnection "DROP DATABASE ? WITH (FORCE)" [PG.Identifier testDatabaseName]
            )
            do
                let testDatabaseUrl = injectDatabaseName testDatabaseName masterDatabaseUrl
                importSql testDatabaseUrl
                callback testDatabaseUrl
    pure ()

-- | Imports the IHP Schema.sql and Application/Schema.sql
importSql :: ByteString -> IO ()
importSql databaseUrl = do
    -- Import IHP Schema
    ihpSchemaSql <- findIHPSchemaSql
    Process.callCommand ("psql " <> cs databaseUrl <> " < " <> ihpSchemaSql)

    -- Import Application Schema
    Process.callCommand ("psql " <> cs databaseUrl <> " < Application/Schema.sql")

    -- Import Application Fixtures (if any)
    Process.callCommand ("psql " <> cs databaseUrl <> " < Application/Fixtures.sql")

randomDatabaseName :: IO Text
randomDatabaseName = do
    uuid <- UUID.nextRandom
    let name = "test_db_" <> (uuid |> UUID.toText |> Text.replace "-" "_")
    pure name

injectDatabaseName :: Text -> ByteString -> ByteString
injectDatabaseName databaseName databaseUrl =
        databaseUrl
        |> cs
        -- Remove database name from url so we can connect to the default database
        |> Text.replace "/app" ("/" <> databaseName)
        |> cs
