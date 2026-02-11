module IHP.Hspec (withIHPApp) where

import IHP.Prelude
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
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
withConnection databaseUrl action = do
    connResult <- Hasql.acquire (HasqlSettings.connectionString (cs databaseUrl))
    case connResult of
        Right conn -> action conn `Exception.finally` Hasql.release conn
        Left err -> error (show err)

runSessionOnConnection :: Hasql.Connection -> Session.Session a -> IO ()
runSessionOnConnection conn session = do
    result <- Hasql.use conn session
    case result of
        Left err -> error (show err)
        Right _ -> pure ()

-- | Create contexts that can be used for mocking
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        logger <- newLogger def { level = Warn } -- don't log queries

        withTestDatabase frameworkConfig.databaseUrl \testDatabaseUrl -> do
            modelContext <- createModelContext testDatabaseUrl logger

            -- Use the central test middleware stack
            let baseRequest = defaultRequest
            mockRequest <- runTestMiddlewares frameworkConfig modelContext baseRequest
            let mockRespond = const (pure ResponseReceived)

            hspecAction MockContext { .. }

withTestDatabase :: ByteString -> (ByteString -> IO ()) -> IO ()
withTestDatabase masterDatabaseUrl callback = do
    testDatabaseName <- randomDatabaseName

    withConnection masterDatabaseUrl \masterConnection ->
        Exception.bracket_
            (runSessionOnConnection masterConnection (execDDL ("CREATE DATABASE " <> quoteIdentifier testDatabaseName)))
            (
                -- The WITH FORCE is required to force close open connections
                runSessionOnConnection masterConnection (execDDL ("DROP DATABASE " <> quoteIdentifier testDatabaseName <> " WITH (FORCE)"))
            )
            do
                let testDatabaseUrl = injectDatabaseName testDatabaseName masterDatabaseUrl
                importSql testDatabaseUrl
                callback testDatabaseUrl
    pure ()

-- | Execute a DDL statement (CREATE DATABASE, DROP DATABASE, etc.)
execDDL :: Text -> Session.Session ()
execDDL sql = Session.statement () (Statement.unpreparable (cs sql) Encoders.noParams Decoders.noResult)

-- | Quote a SQL identifier (e.g., database name) with double quotes
quoteIdentifier :: Text -> Text
quoteIdentifier name = "\"" <> Text.replace "\"" "\"\"" name <> "\""

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
