module Test.DataSync.RLSIntegrationSpec where

import Test.Hspec
import IHP.Prelude hiding (head)
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import IHP.DataSync.RowLevelSecurity (rlsPolicyColumns)
import IHP.DataSync.Hasql (runSession)
import qualified Data.Set as Set
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)

-- | Get the master database URL from DATABASE_URL env var or use a sensible default
getMasterDatabaseUrl :: IO Text
getMasterDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    case envUrl of
        Just url -> pure (cs url)
        Nothing -> pure "postgresql:///postgres"

-- | Create a hasql pool for the given connection string
makePool :: Text -> IO Hasql.Pool.Pool
makePool connStr = Hasql.Pool.acquire $ Hasql.Pool.Config.settings
    [ Hasql.Pool.Config.size 2
    , Hasql.Pool.Config.staticConnectionSettings
        (HasqlSettings.connectionString connStr)
    ]

-- | Run a raw SQL statement on a pool (for test setup)
execSQL :: Hasql.Pool.Pool -> ByteString -> IO ()
execSQL pool sql = runSession pool (Session.script (cs sql))

-- | Check if we can connect to Postgres. Returns True if a connection succeeds.
canConnectToPostgres :: IO Bool
canConnectToPostgres = do
    masterUrl <- getMasterDatabaseUrl
    result <- Exception.try $ Exception.bracket (makePool masterUrl) Hasql.Pool.release
        (\pool -> execSQL pool "SELECT 1")
    case result of
        Left (_ :: Exception.SomeException) -> pure False
        Right _ -> pure True

-- | Create a temporary test database, run the action, then drop the database
withTestDatabase :: (Text -> IO a) -> IO a
withTestDatabase action = do
    masterUrl <- getMasterDatabaseUrl
    testDbName <- randomDatabaseName
    Exception.bracket (makePool masterUrl) Hasql.Pool.release \masterPool -> do
        execSQL masterPool (cs ("CREATE DATABASE " <> testDbName))
        let testConnStr = "dbname=" <> testDbName
        Exception.finally
            (action testConnStr)
            (execSQL masterPool (cs ("DROP DATABASE " <> testDbName <> " WITH (FORCE)")))

-- | Generate a random database name for test isolation
randomDatabaseName :: IO Text
randomDatabaseName = do
    uuid <- UUID.nextRandom
    let name = "ihp_test_rls_" <> (uuid |> UUID.toText |> Text.replace "-" "_")
    pure name

-- | Create a hasql pool for the given connection string, run the action, then release
withHasqlPool :: Text -> (Hasql.Pool.Pool -> IO a) -> IO a
withHasqlPool connStr action =
    Exception.bracket (makePool connStr) Hasql.Pool.release action

-- | Run a database test, skipping if Postgres is not available
withDB :: (Text -> IO ()) -> IO ()
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
                    withHasqlPool connStr \pool -> do
                        execSQL pool "CREATE TABLE messages (id UUID PRIMARY KEY, user_id UUID NOT NULL, body TEXT)"
                        execSQL pool "ALTER TABLE messages ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY messages_policy ON messages USING (user_id = current_setting('rls.ihp_user_id')::uuid)"

                        cols <- rlsPolicyColumns pool "messages"
                        cols `shouldSatisfy` Set.member "user_id"

            it "returns columns from WITH CHECK policy" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        execSQL pool "CREATE TABLE projects (id UUID PRIMARY KEY, owner_id UUID NOT NULL, name TEXT)"
                        execSQL pool "ALTER TABLE projects ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY projects_insert ON projects FOR INSERT WITH CHECK (owner_id = current_setting('rls.ihp_user_id')::uuid)"

                        cols <- rlsPolicyColumns pool "projects"
                        cols `shouldSatisfy` Set.member "owner_id"

            it "returns empty set for a table without RLS policies" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        execSQL pool "CREATE TABLE no_rls_table (id UUID PRIMARY KEY, body TEXT)"

                        cols <- rlsPolicyColumns pool "no_rls_table"
                        cols `shouldBe` Set.empty

            it "returns columns from multiple policies" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        execSQL pool "CREATE TABLE docs (id UUID PRIMARY KEY, user_id UUID NOT NULL, org_id UUID NOT NULL, body TEXT)"
                        execSQL pool "ALTER TABLE docs ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY docs_user ON docs USING (user_id = current_setting('rls.ihp_user_id')::uuid)"
                        execSQL pool "CREATE POLICY docs_org ON docs USING (org_id = current_setting('rls.ihp_org_id')::uuid)"

                        cols <- rlsPolicyColumns pool "docs"
                        cols `shouldSatisfy` Set.member "user_id"
                        cols `shouldSatisfy` Set.member "org_id"
