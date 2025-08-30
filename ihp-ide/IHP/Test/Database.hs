module IHP.Test.Database where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import IHP.IDE.CodeGen.MigrationGenerator (findIHPSchemaSql)


import           Data.ByteString.Builder                   (toLazyByteString)
import qualified Data.Vault.Lazy                           as Vault
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))
import           Network.Wai.Parse                         (Param (..))

import           IHP.ApplicationContext                    (ApplicationContext (..))
import qualified IHP.AutoRefresh.Types                     as AutoRefresh
import           IHP.Controller.RequestContext             (RequestBody (..), RequestContext (..))
import           IHP.ControllerSupport                     (InitControllerContext, Controller, runActionWithNewContext)
import           IHP.FrameworkConfig                       (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig                       as FrameworkConfig
import           IHP.ModelSupport                          (createModelContext, Id')
import           IHP.Prelude
import           IHP.Log.Types
import           IHP.Job.Types
import Test.Hspec
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import qualified IHP.LoginSupport.Helper.Controller as Session
import qualified Network.Wai.Session
import qualified Control.Exception as Exception
import qualified IHP.PGListener as PGListener
import IHP.Controller.Session (sessionVaultKey)
import qualified Network.Wai.Test as WaiTest

import qualified System.Process as Process
import IHP.Test.Mocking

data TestDatabase = TestDatabase
    { name :: Text
    , url :: ByteString
    }

-- | Given a Postgres Database URL it creates a new randomly named database on the database server. Returns a database url to the freshly created database
--
-- >>> createTestDatabase "postgresql:///app?host=/myapp/build/db"
-- TestDatabase { name = "test-7d3bd463-4cce-413f-a272-ac52e5d93739", url = "postgresql:///test-7d3bd463-4cce-413f-a272-ac52e5d93739?host=/myapp/build/db" }
--
createTestDatabase :: ByteString -> IO TestDatabase
createTestDatabase databaseUrl = do
--    currentDir <- Directory.getCurrentDirectory
    databaseId <- UUID.nextRandom
    let databaseName = "test-" <> UUID.toText databaseId

    withConnection databaseUrl \connection -> do
        PG.execute connection "CREATE DATABASE ?" [PG.Identifier databaseName]

    let newUrl :: ByteString = databaseUrl
            |> cs
            |> Text.replace "postgresql:///app" ("postgresql:///" <> databaseName)
            |> cs

    -- We use the system psql to handle the initial Schema Import as it can handle
    -- complex Schema including variations in formatting, custom types, functions, and table definitions.
    let importSql file = Process.callCommand ("psql " <> (cs newUrl) <> " < " <> file)

    ihpSchemaSql <- findIHPSchemaSql
    importSql ihpSchemaSql
    importSql "Application/Schema.sql"

    pure TestDatabase { name = databaseName, url = newUrl }

-- | Given the master connection url and the open test database, this will clean up the test database
--
-- >>> deleteDatabase "postgresql:///app?host=/myapp/build/db" TestDatabase { name = "test-7d3bd463-4cce-413f-a272-ac52e5d93739", url = "postgresql:///test-7d3bd463-4cce-413f-a272-ac52e5d93739?host=/myapp/build/db" }
--
-- The master connection url needs to be passed as we cannot drop the database we're currently connected to, and therefore
-- we cannot use the test database itself.
--
deleteDatabase :: ByteString -> TestDatabase -> IO ()
deleteDatabase masterDatabaseUrl testDatabase = do
    withConnection masterDatabaseUrl \connection -> do
        -- The WITH FORCE is required to force close open connections
        -- Otherwise the DROP DATABASE takes a few seconds to execute
        PG.execute connection "DROP DATABASE ? WITH (FORCE)" [PG.Identifier (testDatabase.name)]
    pure ()


withConnection databaseUrl = Exception.bracket (PG.connectPostgreSQL databaseUrl) PG.close

-- | Create contexts that can be used for mocking
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        let FrameworkConfig { dbPoolMaxConnections, dbPoolIdleTime, databaseUrl } = frameworkConfig

        logger <- newLogger def { level = Warn } -- don't log queries

        let initTestDatabase = createTestDatabase databaseUrl
        let cleanupTestDatabase testDatabase = deleteDatabase databaseUrl testDatabase
        let withTestDatabase = Exception.bracket initTestDatabase cleanupTestDatabase

        withTestDatabase \testDatabase -> do
            modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections (testDatabase.url) logger

            PGListener.withPGListener modelContext \pgListener -> do
                autoRefreshServer <- newIORef (AutoRefresh.newAutoRefreshServer pgListener)
                let sessionVault = Vault.insert sessionVaultKey mempty Vault.empty
                let applicationContext = ApplicationContext { modelContext = modelContext, autoRefreshServer, frameworkConfig, pgListener }

                let requestContext = RequestContext
                     { request = defaultRequest {vault = sessionVault}
                     , requestBody = FormBody [] []
                     , respond = const (pure ResponseReceived)
                     , frameworkConfig = frameworkConfig }

                (hspecAction MockContext { .. })
