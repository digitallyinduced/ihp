module IHP.Hspec (withIHPApp) where

import IHP.Prelude
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import IHP.IDE.CodeGen.MigrationGenerator (findIHPSchemaSql)

import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))

import IHP.ControllerSupport (InitControllerContext)
import IHP.FrameworkConfig (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.ModelSupport (createModelContext)
import IHP.Log.Types

import qualified System.Process as Process
import IHP.Test.Mocking (MockContext(..), runTestMiddlewares)

withConnection :: ByteString -> (Hasql.Connection -> IO a) -> IO a
withConnection databaseUrl = Exception.bracket acquireOrFail Hasql.release
    where
        acquireOrFail = do
            result <- Hasql.acquire (connectionSettingsFromDatabaseUrl databaseUrl)
            case result of
                Left err -> error ("withConnection: Failed to acquire connection: " <> show err)
                Right conn -> pure conn

-- | Create contexts that can be used for mocking
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        let FrameworkConfig { dbPoolMaxConnections, dbPoolIdleTime } = frameworkConfig

        logger <- newLogger def { level = Warn } -- don't log queries

        withTestDatabase frameworkConfig.databaseUrl \testDatabaseUrl -> do
            modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections testDatabaseUrl logger

            -- Use the central test middleware stack
            let baseRequest = defaultRequest
            mockRequest <- runTestMiddlewares frameworkConfig modelContext baseRequest
            let mockRespond = const (pure ResponseReceived)

            hspecAction MockContext { .. }

withTestDatabase masterDatabaseUrl callback = do
    testDatabaseName <- randomDatabaseName

    withConnection masterDatabaseUrl \masterConnection ->
        Exception.bracket_
            (runScript masterConnection ("CREATE DATABASE " <> quoteIdentifier testDatabaseName))
            (
                -- The WITH FORCE is required to force close open connections
                runScript masterConnection ("DROP DATABASE " <> quoteIdentifier testDatabaseName <> " WITH (FORCE)")
            )
            do
                let testDatabaseUrl = injectDatabaseName testDatabaseName masterDatabaseUrl
                importSql testDatabaseUrl
                callback testDatabaseUrl
    pure ()

runScript :: Hasql.Connection -> ByteString -> IO ()
runScript conn scriptText = do
    result <- Hasql.run (Hasql.sql scriptText) conn
    case result of
        Left err -> error ("runScript failed: " <> show err)
        Right () -> pure ()

-- | Quotes an identifier for use in SQL statements (e.g. database names)
quoteIdentifier :: Text -> ByteString
quoteIdentifier name = "\"" <> cs name <> "\""

-- | Imports the IHP Schema.sql and Application/Schema.sql
importSql :: ByteString -> IO ()
importSql databaseUrl = do
    -- Import IHP Schema
    ihpSchemaSql <- findIHPSchemaSql
    Process.callCommand ("psql " <> cs databaseUrl <> " < " <> cs (osPathToText ihpSchemaSql))

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
