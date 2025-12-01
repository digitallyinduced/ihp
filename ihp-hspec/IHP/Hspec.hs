module IHP.Hspec (withIHPApp) where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import IHP.IDE.CodeGen.MigrationGenerator (findIHPSchemaSql)


import qualified Data.Vault.Lazy                           as Vault
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))


import qualified IHP.AutoRefresh.Types                     as AutoRefresh
import           IHP.Controller.RequestContext             (RequestBody (..), RequestContext (..))
import           IHP.ControllerSupport                     (InitControllerContext, Controller, runActionWithNewContext)
import           IHP.FrameworkConfig                       (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig                       as FrameworkConfig
import           IHP.ModelSupport                          (createModelContext, Id')
import           IHP.Prelude
import           IHP.Log.Types
import qualified IHP.PGListener as PGListener
import IHP.Controller.Session (sessionVaultKey)

import qualified System.Process as Process
import IHP.Test.Mocking

withConnection databaseUrl = Exception.bracket (PG.connectPostgreSQL databaseUrl) PG.close

-- | Create contexts that can be used for mocking
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        let FrameworkConfig { dbPoolMaxConnections, dbPoolIdleTime } = frameworkConfig

        logger <- newLogger def { level = Warn } -- don't log queries


        withTestDatabase frameworkConfig.databaseUrl \testDatabaseUrl -> do
            modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections testDatabaseUrl logger

            let sessionVault = Vault.insert sessionVaultKey mempty Vault.empty

            let requestContext = RequestContext
                 { request = defaultRequest {vault = sessionVault}
                 , requestBody = FormBody [] []
                 , respond = const (pure ResponseReceived)
                 , frameworkConfig = frameworkConfig }

            (hspecAction MockContext { .. })

withTestDatabase masterDatabaseUrl callback = do
    testDatabaseName <- randomDatabaseName

    withConnection masterDatabaseUrl \masterConnection ->
        Exception.bracket_
            (PG.execute masterConnection "CREATE DATABASE ?" [PG.Identifier testDatabaseName])
            (
                -- The WITH FORCE is required to force close open connections
                -- Otherwise the DROP DATABASE takes a few seconds to execute
                PG.execute masterConnection "DROP DATABASE ? WITH (FORCE)" [PG.Identifier testDatabaseName]
            )
            (do
                importSchema (testDatabaseUrl masterDatabaseUrl testDatabaseName)
                callback (testDatabaseUrl masterDatabaseUrl testDatabaseName)
            )

testDatabaseUrl :: ByteString -> Text -> ByteString
testDatabaseUrl masterDatabaseUrl testDatabaseName =
    masterDatabaseUrl
        |> cs
        |> Text.replace "postgresql:///app" ("postgresql:///" <> testDatabaseName)
        |> cs

randomDatabaseName :: IO Text
randomDatabaseName = do
    databaseId <- UUID.nextRandom
    pure ("test-" <> UUID.toText databaseId)

importSchema :: ByteString -> IO ()
importSchema databaseUrl = do
    -- We use the system psql to handle the initial Schema Import as it can handle
    -- complex Schema including variations in formatting, custom types, functions, and table definitions.
    let importSql file = Process.callCommand ("psql " <> (cs databaseUrl) <> " < " <> file)

    ihpSchemaSql <- findIHPSchemaSql
    importSql ihpSchemaSql
    importSql "Application/Schema.sql"