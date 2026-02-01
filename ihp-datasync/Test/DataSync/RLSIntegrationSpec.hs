module Test.DataSync.RLSIntegrationSpec where

import Test.Hspec
import IHP.Prelude hiding (head)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import IHP.DataSync.RowLevelSecurity (rlsPolicyColumns)
import qualified Data.Set as Set
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)

-- | Get the master database URL from DATABASE_URL env var or use a sensible default
getMasterDatabaseUrl :: IO ByteString
getMasterDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    case envUrl of
        Just url -> pure (cs url)
        Nothing -> pure "postgresql:///postgres"

-- | Check if we can connect to Postgres. Returns True if a connection succeeds.
canConnectToPostgres :: IO Bool
canConnectToPostgres = do
    masterUrl <- getMasterDatabaseUrl
    result <- Exception.try (Exception.bracket (PG.connectPostgreSQL masterUrl) PG.close (\_ -> pure ()))
    case result of
        Left (_ :: Exception.SomeException) -> pure False
        Right _ -> pure True

-- | Create a temporary test database, run the action, then drop the database
withTestDatabase :: (ByteString -> IO a) -> IO a
withTestDatabase action = do
    masterUrl <- getMasterDatabaseUrl
    testDbName <- randomDatabaseName
    Exception.bracket (PG.connectPostgreSQL masterUrl) PG.close \masterConn -> do
        PG.execute_ masterConn (PG.Query ("CREATE DATABASE " <> cs testDbName))
        let testConnStr = "dbname=" <> cs testDbName
        Exception.finally
            (action testConnStr)
            (PG.execute_ masterConn (PG.Query ("DROP DATABASE " <> cs testDbName <> " WITH (FORCE)")))

-- | Generate a random database name for test isolation
randomDatabaseName :: IO Text
randomDatabaseName = do
    uuid <- UUID.nextRandom
    let name = "ihp_test_rls_" <> (uuid |> UUID.toText |> Text.replace "-" "_")
    pure name

-- | Create a hasql pool for the given connection string
withHasqlPool :: ByteString -> (Hasql.Pool.Pool -> IO a) -> IO a
withHasqlPool connStr action = do
    let poolConfig = Hasql.Pool.Config.settings
            [ Hasql.Pool.Config.size 2
            , Hasql.Pool.Config.staticConnectionSettings
                [HasqlSetting.connection (HasqlConnection.string (cs connStr))]
            ]
    pool <- Hasql.Pool.acquire poolConfig
    Exception.finally (action pool) (Hasql.Pool.release pool)

-- | Run a database test, skipping if Postgres is not available
withDB :: (ByteString -> IO ()) -> IO ()
withDB action = do
    available <- canConnectToPostgres
    if available
        then withTestDatabase action
        else pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"

tests :: Spec
tests = do
    describe "IHP.DataSync.RowLevelSecurity" do
        describe "rlsPolicyColumns" do
            it "returns columns referenced in a USING policy" do
                withDB \connStr -> do
                    Exception.bracket (PG.connectPostgreSQL connStr) PG.close \conn -> do
                        PG.execute_ conn "CREATE TABLE messages (id UUID PRIMARY KEY, user_id UUID NOT NULL, body TEXT)"
                        PG.execute_ conn "ALTER TABLE messages ENABLE ROW LEVEL SECURITY"
                        PG.execute_ conn "CREATE POLICY messages_policy ON messages USING (user_id = current_setting('rls.ihp_user_id')::uuid)"

                    withHasqlPool connStr \pool -> do
                        cols <- rlsPolicyColumns pool "messages"
                        cols `shouldSatisfy` Set.member "user_id"

            it "returns columns from WITH CHECK policy" do
                withDB \connStr -> do
                    Exception.bracket (PG.connectPostgreSQL connStr) PG.close \conn -> do
                        PG.execute_ conn "CREATE TABLE projects (id UUID PRIMARY KEY, owner_id UUID NOT NULL, name TEXT)"
                        PG.execute_ conn "ALTER TABLE projects ENABLE ROW LEVEL SECURITY"
                        PG.execute_ conn "CREATE POLICY projects_insert ON projects FOR INSERT WITH CHECK (owner_id = current_setting('rls.ihp_user_id')::uuid)"

                    withHasqlPool connStr \pool -> do
                        cols <- rlsPolicyColumns pool "projects"
                        cols `shouldSatisfy` Set.member "owner_id"

            it "returns empty set for a table without RLS policies" do
                withDB \connStr -> do
                    Exception.bracket (PG.connectPostgreSQL connStr) PG.close \conn -> do
                        PG.execute_ conn "CREATE TABLE no_rls_table (id UUID PRIMARY KEY, body TEXT)"

                    withHasqlPool connStr \pool -> do
                        cols <- rlsPolicyColumns pool "no_rls_table"
                        cols `shouldBe` Set.empty

            it "returns columns from multiple policies" do
                withDB \connStr -> do
                    Exception.bracket (PG.connectPostgreSQL connStr) PG.close \conn -> do
                        PG.execute_ conn "CREATE TABLE docs (id UUID PRIMARY KEY, user_id UUID NOT NULL, org_id UUID NOT NULL, body TEXT)"
                        PG.execute_ conn "ALTER TABLE docs ENABLE ROW LEVEL SECURITY"
                        PG.execute_ conn "CREATE POLICY docs_user ON docs USING (user_id = current_setting('rls.ihp_user_id')::uuid)"
                        PG.execute_ conn "CREATE POLICY docs_org ON docs USING (org_id = current_setting('rls.ihp_org_id')::uuid)"

                    withHasqlPool connStr \pool -> do
                        cols <- rlsPolicyColumns pool "docs"
                        cols `shouldSatisfy` Set.member "user_id"
                        cols `shouldSatisfy` Set.member "org_id"
