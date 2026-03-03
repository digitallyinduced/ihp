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
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (replicateM)
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
                    -- Tiny pool (2 connections) + short acquisition timeout to
                    -- make pool exhaustion visible quickly.
                    Exception.bracket (makePoolWithTimeout 2 3 connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

                        -- Use a unique table name to avoid stale global MVar state
                        -- across repeated test runs in the same ghci session
                        tableUuid <- UUID.nextRandom
                        let tableName = "test_concurrent_" <> Text.replace "-" "_" (UUID.toText tableUuid)
                        execSQL pool (cs ("CREATE TABLE " <> tableName <> " (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), body TEXT)"))

                        let table = TableWithRLS tableName

                        -- Create many independent cached installers, simulating
                        -- concurrent WebSocket connections.
                        -- On master: each gets its own IORef cache, so all of them
                        --   call installTableChangeTriggers independently. The DDL
                        --   requires AccessExclusiveLock, so they queue up behind
                        --   each other at the DB level, each holding a pool
                        --   connection while waiting → pool exhausted for seconds.
                        -- On fix: all share the process-global MVar, so only 1 grabs
                        --   a pool connection for DDL, the rest wait on the MVar
                        --   in Haskell without consuming pool connections.
                        installers <- replicateM 20000 (makeCachedInstallTableChangeTriggers pool)

                        -- Barrier: ensure all 20000 threads are created and waiting
                        -- before they rush the pool simultaneously.
                        barrier <- newEmptyMVar

                        result <- Exception.try $ do
                            installsAsync <- async $
                                mapConcurrently_ (\install -> readMVar barrier >> install table) installers

                            -- Give threads time to start and block on barrier
                            threadDelay 500_000

                            -- Release all installers at once
                            putMVar barrier ()

                            wait installsAsync

                        case result of
                            Left (_ :: Hasql.Pool.UsageError) ->
                                expectationFailure
                                    "Pool exhausted — concurrent trigger installation caused AcquisitionTimeoutUsageError"
                            Right () -> pure ()

-- DB helpers (same pattern as DataSyncIntegrationSpec.hs)

getMasterDatabaseUrl :: IO Text
getMasterDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    case envUrl of
        Just url -> pure (cs url)
        Nothing -> pure "postgresql:///postgres"

makePool :: Text -> IO Hasql.Pool.Pool
makePool = makePoolN 4

makePoolN :: Int -> Text -> IO Hasql.Pool.Pool
makePoolN poolSize connStr = Hasql.Pool.acquire $ Hasql.Pool.Config.settings
    [ Hasql.Pool.Config.size poolSize
    , Hasql.Pool.Config.staticConnectionSettings
        (HasqlSettings.connectionString connStr)
    ]

makePoolWithTimeout :: Int -> Int -> Text -> IO Hasql.Pool.Pool
makePoolWithTimeout poolSize timeoutSeconds connStr = Hasql.Pool.acquire $ Hasql.Pool.Config.settings
    [ Hasql.Pool.Config.size poolSize
    , Hasql.Pool.Config.acquisitionTimeout (fromIntegral timeoutSeconds)
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
