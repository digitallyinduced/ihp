module Test.DataSync.ChangeNotifications where

import Test.Hspec
import IHP.Prelude
import Data.Aeson
import IHP.DataSync.ChangeNotifications (Change(..), makeCachedInstallTableChangeTriggers)
import IHP.DataSync.ControllerImpl (changesToValue)
import IHP.DataSync.DynamicQueryCompiler (Renamer(..))
import IHP.DataSync.DynamicQuery (ConditionExpression(..), ConditionOperator(..), FunctionCall(..), conditionColumns)
import IHP.DataSync.RowLevelSecurity (TableWithRLS(..))
import qualified Data.Set as Set
import qualified Prelude
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Concurrent.Async (mapConcurrently_, async, wait)
import qualified Hasql.Connection as Connection
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text

tests = do
    describe "IHP.DataSync.ChangeNotifications" do
        describe "FromJSON Change" do
            it "parses a regular Change with 'new' key" do
                let json = "{\"col\":\"body\",\"new\":\"Hello\"}"
                let expected = Change { col = "body", new = String "Hello" }
                eitherDecode json `shouldBe` Right expected

            it "parses an AppendChange with 'append' key" do
                let json = "{\"col\":\"body\",\"append\":\" World\"}"
                let expected = AppendChange { col = "body", append = " World" }
                eitherDecode json `shouldBe` Right expected

        describe "ToJSON Change" do
            it "serializes a regular Change" do
                let change = Change { col = "body", new = String "Hello" }
                let result = toJSON change
                result `shouldBe` object ["col" .= ("body" :: Text), "new" .= ("Hello" :: Text)]

            it "serializes an AppendChange" do
                let change = AppendChange { col = "body", append = " World" }
                let result = toJSON change
                result `shouldBe` object ["col" .= ("body" :: Text), "append" .= (" World" :: Text)]

        describe "ToJSON/FromJSON round-trip" do
            it "round-trips a regular Change" do
                let change = Change { col = "title", new = String "New Title" }
                (eitherDecode (encode change)) `shouldBe` Right change

            it "round-trips an AppendChange" do
                let change = AppendChange { col = "body", append = " appended text" }
                (eitherDecode (encode change)) `shouldBe` Right change

        describe "changesToValue" do
            let identityRenamer = Renamer { fieldToColumn = Prelude.id, columnToField = Prelude.id }

            it "splits mixed changes into changeSet and appendSet" do
                let changes =
                        [ Change { col = "title", new = String "New Title" }
                        , AppendChange { col = "body", append = " more text" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Just (object ["title" .= ("New Title" :: Text)])
                appendSet `shouldBe` Just (object ["body" .= (" more text" :: Text)])

            it "returns empty appendSet when all changes are regular" do
                let changes =
                        [ Change { col = "title", new = String "New Title" }
                        , Change { col = "body", new = String "Full body" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Just (object ["title" .= ("New Title" :: Text), "body" .= ("Full body" :: Text)])
                appendSet `shouldBe` Nothing

            it "returns empty changeSet when all changes are appends" do
                let changes =
                        [ AppendChange { col = "body", append = " suffix" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Nothing
                appendSet `shouldBe` Just (object ["body" .= (" suffix" :: Text)])

            it "applies renamer to column names" do
                let renamer = Renamer { fieldToColumn = Prelude.id, columnToField = \col -> case col of
                        "user_name" -> "userName"
                        other -> other
                    }
                let changes =
                        [ Change { col = "user_name", new = String "Alice" }
                        , AppendChange { col = "user_name", append = " Smith" }
                        ]
                let (changeSet, appendSet) = changesToValue renamer changes
                changeSet `shouldBe` Just (object ["userName" .= ("Alice" :: Text)])
                appendSet `shouldBe` Just (object ["userName" .= (" Smith" :: Text)])

        describe "conditionColumns" do
            it "returns a single column for a simple WHERE" do
                let condition = InfixOperatorExpression
                        { left = ColumnExpression "conversationId"
                        , op = OpEqual
                        , right = LiteralExpression (String "00000000-0000-0000-0000-000000000000")
                        }
                conditionColumns condition `shouldBe` Set.fromList ["conversationId"]

            it "returns multiple columns for a compound WHERE with AND" do
                let condition = InfixOperatorExpression
                        { left = InfixOperatorExpression
                            { left = ColumnExpression "conversationId"
                            , op = OpEqual
                            , right = LiteralExpression (String "00000000-0000-0000-0000-000000000000")
                            }
                        , op = OpAnd
                        , right = InfixOperatorExpression
                            { left = ColumnExpression "status"
                            , op = OpEqual
                            , right = LiteralExpression (String "active")
                            }
                        }
                conditionColumns condition `shouldBe` Set.fromList ["conversationId", "status"]

            it "extracts columns from nested AND/OR" do
                let condition = InfixOperatorExpression
                        { left = InfixOperatorExpression
                            { left = ColumnExpression "a"
                            , op = OpEqual
                            , right = LiteralExpression (Number 1)
                            }
                        , op = OpOr
                        , right = InfixOperatorExpression
                            { left = ColumnExpression "b"
                            , op = OpAnd
                            , right = ColumnExpression "c"
                            }
                        }
                conditionColumns condition `shouldBe` Set.fromList ["a", "b", "c"]

            it "returns empty set for CallExpression" do
                let condition = CallExpression (ToTSQuery "hello")
                conditionColumns condition `shouldBe` Set.empty

            it "returns empty set for ListExpression" do
                let condition = ListExpression [Number 1, Number 2]
                conditionColumns condition `shouldBe` Set.empty

            it "returns empty set for LiteralExpression" do
                let condition = LiteralExpression (String "hello")
                conditionColumns condition `shouldBe` Set.empty

        describe "concurrent trigger installation" do
            it "does not exhaust the connection pool under concurrent trigger installation" do
                withDB \connStr -> do
                    -- Small pool (4 connections) to make pool exhaustion visible
                    withHasqlPool connStr \pool -> do
                        execSQL pool "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

                        -- Use a unique table name to avoid stale global MVar state
                        -- across repeated test runs in the same ghci session
                        tableUuid <- UUID.nextRandom
                        let tableName = "test_concurrent_" <> Text.replace "-" "_" (UUID.toText tableUuid)
                        execSQL pool (cs ("CREATE TABLE " <> tableName <> " (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), body TEXT)"))

                        let table = TableWithRLS tableName

                        -- Hold AccessExclusiveLock from a raw connection (not from the pool).
                        -- This simulates slow DDL: trigger installation requires
                        -- AccessExclusiveLock which blocks while we hold it,
                        -- tying up pool connections on master.
                        Right lockConn <- Connection.acquire (HasqlSettings.connectionString (cs connStr))
                        _ <- Connection.use lockConn $
                            Session.script (cs ("BEGIN; LOCK TABLE " <> tableName <> " IN ACCESS EXCLUSIVE MODE"))

                        -- Create 10 independent cached installers, simulating 10
                        -- concurrent WebSocket connections.
                        -- On master: each gets its own IORef cache, so all 10 call
                        --   installTableChangeTriggers independently → 4 grab pool
                        --   connections and block on DDL → pool exhausted.
                        -- On fix: all share the process-global MVar, so only 1 grabs
                        --   a pool connection for DDL, the other 9 wait on the MVar
                        --   in Haskell without consuming pool connections.
                        installers <- replicateM 10 (makeCachedInstallTableChangeTriggers pool)

                        -- Launch concurrent trigger installations in background
                        installsAsync <- async $
                            mapConcurrently_ (\install -> install table) installers

                        -- Give installs time to start and block on the held lock
                        threadDelay 1_000_000

                        -- Canary query: if the pool is exhausted (all connections
                        -- stuck in DDL), this will time out.
                        canaryResult <- race
                            (threadDelay 5_000_000)  -- 5s timeout
                            (runSession pool (Session.script "SELECT 1"))

                        -- Release lock so blocked DDL threads can complete.
                        -- Using a raw connection + ROLLBACK avoids cancel (which
                        -- blocks on FFI calls to libpq).
                        _ <- Connection.use lockConn $
                            Session.script "ROLLBACK"
                        Connection.release lockConn

                        -- Wait for installers to finish (now unblocked)
                        _ <- Exception.try @Exception.SomeException (wait installsAsync)

                        case canaryResult of
                            Left () -> expectationFailure
                                "Pool exhausted — canary query timed out because concurrent trigger installation consumed all connections"
                            Right _ -> pure ()  -- Pool had available connections

-- DB helpers (same pattern as DataSyncIntegrationSpec.hs)

getMasterDatabaseUrl :: IO Text
getMasterDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    case envUrl of
        Just url -> pure (cs url)
        Nothing -> pure "postgresql:///postgres"

makePool :: Text -> IO Hasql.Pool.Pool
makePool connStr = Hasql.Pool.acquire $ Hasql.Pool.Config.settings
    [ Hasql.Pool.Config.size 4
    , Hasql.Pool.Config.staticConnectionSettings
        (HasqlSettings.connectionString connStr)
    ]

execSQL :: Hasql.Pool.Pool -> ByteString -> IO ()
execSQL pool sql = runSession pool (Session.script (cs sql))

canConnectToPostgres :: IO Bool
canConnectToPostgres = do
    masterUrl <- getMasterDatabaseUrl
    result <- Exception.try $ Exception.bracket (makePool masterUrl) Hasql.Pool.release
        (\pool -> execSQL pool "SELECT 1")
    case result of
        Left (_ :: Exception.SomeException) -> pure False
        Right _ -> pure True

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

randomDatabaseName :: IO Text
randomDatabaseName = do
    uuid <- UUID.nextRandom
    let name = "ihp_test_cn_" <> (uuid |> UUID.toText |> Text.replace "-" "_")
    pure name

withHasqlPool :: Text -> (Hasql.Pool.Pool -> IO a) -> IO a
withHasqlPool connStr action =
    Exception.bracket (makePool connStr) Hasql.Pool.release action

withDB :: (Text -> IO ()) -> IO ()
withDB action = do
    available <- canConnectToPostgres
    if available
        then withTestDatabase action
        else pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
