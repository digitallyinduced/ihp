module DataSync.ChangeNotifications where

import Test.Hspec
import IHP.Prelude
import Data.Aeson
import IHP.DataSync.ChangeNotifications (Change(..), InvalidationPlan(..), makeCachedInstallTableChangeTriggers, makeCachedInstallGlobalInvalidationTriggers, makeInstallInvalidationPlan, resolveInvalidationPlan, installTableChangeTriggers, installGlobalInvalidationTriggers)
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
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
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

        describe "global invalidation trigger" do
            it "installs and fires on a relation without an id column" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        tableUuid <- UUID.nextRandom
                        let tableName = "test_idless_" <> Text.replace "-" "_" (UUID.toText tableUuid)
                        execSQL pool (cs ("CREATE TABLE " <> tableName <> " (user_id UUID NOT NULL, resource_id UUID NOT NULL)"))

                        installGlobalInvalidationTriggers pool
                        triggerCount <- queryGlobalInvalidationTriggerCount pool tableName
                        triggerCount `shouldBe` 1

                        -- A row-level trigger would fail here by referencing NEW.id.
                        -- The global trigger is statement-level and payload-free.
                        userId <- UUID.nextRandom
                        resourceId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO " <> tableName <> " (user_id, resource_id) VALUES ('" <> UUID.toText userId <> "', '" <> UUID.toText resourceId <> "')"))

            it "reconciles new relations outside the search path" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        install <- makeCachedInstallGlobalInvalidationTriggers pool
                        install

                        execSQL pool "CREATE SCHEMA auth"
                        execSQL pool "CREATE TABLE auth.memberships (user_id UUID NOT NULL, resource_id UUID NOT NULL)"

                        -- The fingerprint must invalidate the prior successful
                        -- reconciliation even on a long-lived controller/pool.
                        install
                        queryGlobalInvalidationTriggerCount pool "auth.memberships"
                            `shouldReturn` 1

            it "serializes first installs across controllers without exhausting the pool" do
                withDB \connStr -> do
                    Exception.bracket (makePoolWithTimeout 2 1 connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE TABLE global_install_lock_test (value TEXT NOT NULL)"
                        lockPool <- makePoolN 1 connStr
                        execSQL lockPool "BEGIN; INSERT INTO global_install_lock_test (value) VALUES ('lock holder')"

                        installers <- replicateM 100 (makeCachedInstallGlobalInvalidationTriggers pool)
                        installsAsync <- async
                            (Exception.try (mapConcurrently_ id installers) :: IO (Either Exception.SomeException ()))

                        -- Keep CREATE TRIGGER blocked beyond the pool acquisition
                        -- timeout. Only the elected Haskell-side installer may own
                        -- a pool connection while all other callers wait.
                        threadDelay 1_200_000
                        Hasql.Pool.release lockPool

                        result <- wait installsAsync
                        case result of
                            Left exception -> expectationFailure
                                ("Concurrent global trigger installation failed: " <> displayException exception)
                            Right () -> pure ()

            it "rejects a same-name trigger wired to the wrong function" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE TABLE conflicting_global_trigger (value TEXT NOT NULL)"
                        execSQL pool "CREATE FUNCTION conflicting_global_trigger_fn() RETURNS TRIGGER AS $$ BEGIN RETURN NULL; END $$ LANGUAGE plpgsql"
                        execSQL pool "CREATE TRIGGER ihp_datasync_invalidate AFTER INSERT ON conflicting_global_trigger FOR EACH STATEMENT EXECUTE PROCEDURE conflicting_global_trigger_fn()"

                        result <- Exception.try (installGlobalInvalidationTriggers pool)
                            :: IO (Either Exception.SomeException ())
                        case result of
                            Left exception -> displayException exception
                                `shouldContain` "Incompatible pre-existing trigger"
                            Right () -> expectationFailure "Accepted incompatible same-name invalidation trigger"

            it "rejects a replica-only invalidation trigger" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE TABLE replica_only_trigger (id UUID NOT NULL)"
                        execSQL pool "ALTER TABLE replica_only_trigger ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY replica_only_trigger_policy ON replica_only_trigger USING (true)"

                        installPlan <- makeInstallInvalidationPlan pool
                        initialPlan <- resolveInvalidationPlan pool (TableWithRLS "replica_only_trigger")
                        installPlan initialPlan
                        execSQL pool "ALTER TABLE replica_only_trigger ENABLE REPLICA TRIGGER ihp_datasync_invalidate"

                        replicaOnlyPlan <- resolveInvalidationPlan pool (TableWithRLS "replica_only_trigger")
                        replicaOnlyPlan.missingRelationOids `shouldSatisfy` (not . null)
                        result <- Exception.try (installPlan replicaOnlyPlan)
                            :: IO (Either Exception.SomeException ())
                        case result of
                            Left exception -> displayException exception
                                `shouldContain` "Incompatible pre-existing trigger"
                            Right () -> expectationFailure "Accepted a trigger that does not fire for normal writes"

            it "rejects relations excluded by the trigger installer namespace rules" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        result <- Exception.try
                            (resolveInvalidationPlan pool (TableWithRLS "pg_catalog.pg_authid"))
                            :: IO (Either Exception.SomeException InvalidationPlan)
                        case result of
                            Left exception -> displayException exception
                                `shouldContain` "cannot install a safe invalidation trigger"
                            Right _ -> expectationFailure "Accepted a pg_catalog relation that the installer skips"

            it "recreates a dropped function and skips an already complete exact plan" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE TABLE exact_reconcile_test (id UUID NOT NULL)"
                        execSQL pool "ALTER TABLE exact_reconcile_test ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY exact_reconcile_policy ON exact_reconcile_test USING (true)"

                        installPlan <- makeInstallInvalidationPlan pool
                        firstPlan <- resolveInvalidationPlan pool (TableWithRLS "exact_reconcile_test")
                        installPlan firstPlan

                        execSQL pool "DROP FUNCTION public.ihp_datasync_notify_invalidation() CASCADE"
                        missingPlan <- resolveInvalidationPlan pool (TableWithRLS "exact_reconcile_test")
                        missingPlan.missingRelationOids `shouldSatisfy` (not . null)
                        installPlan missingPlan

                        completePlan <- resolveInvalidationPlan pool (TableWithRLS "exact_reconcile_test")
                        completePlan.missingRelationOids `shouldBe` []
                        installPlan completePlan
                        queryGlobalInvalidationTriggerCount pool "exact_reconcile_test"
                            `shouldReturn` 1

        -- https://github.com/digitallyinduced/ihp/issues/2467
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

        describe "trigger installation with locked table" do
            it "succeeds on second call even when a writer holds RowExclusiveLock" do
                withDB \connStr -> do
                    Exception.bracket (makePool connStr) Hasql.Pool.release \pool -> do
                        execSQL pool "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

                        tableUuid <- UUID.nextRandom
                        let tableName = "test_lock_" <> Text.replace "-" "_" (UUID.toText tableUuid)
                        execSQL pool (cs ("CREATE TABLE " <> tableName <> " (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), body TEXT)"))

                        let table = TableWithRLS tableName

                        -- First call: install triggers (table is not locked)
                        installTableChangeTriggers pool table

                        -- Verify triggers were created
                        triggerCount <- queryTriggerCount pool tableName
                        triggerCount `shouldBe` 3

                        -- Simulate a long-running writer holding RowExclusiveLock.
                        -- RowExclusiveLock conflicts with ShareRowExclusiveLock
                        -- (which CREATE TRIGGER takes), but NOT with the
                        -- IF NOT EXISTS check (AccessShareLock on pg_trigger).
                        -- An open SELECT only takes AccessShareLock which does
                        -- NOT conflict with CREATE TRIGGER, so we must use a
                        -- writer (INSERT) to properly test this.
                        lockPool <- makePoolN 1 connStr
                        execSQL lockPool (cs ("BEGIN; INSERT INTO " <> tableName <> " (body) VALUES ('lock holder')"))

                        -- Second call: triggers already exist, so this should
                        -- skip CREATE TRIGGER and succeed despite the writer lock.
                        -- With the old code (DROP TRIGGER + CREATE TRIGGER always),
                        -- this would block on the table lock.
                        result <- Exception.try $ do
                            execSQL pool "SET statement_timeout = '2s'"
                            installTableChangeTriggers pool table
                            execSQL pool "SET statement_timeout = '0'"

                        Hasql.Pool.release lockPool

                        case result of
                            Left (e :: Exception.SomeException) ->
                                expectationFailure
                                    (cs ("Second trigger install blocked or failed with locked table: " <> show e))
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

queryTriggerCount :: Hasql.Pool.Pool -> Text -> IO Int
queryTriggerCount pool tableName = do
    let session = Session.statement (cs tableName) $ Statement.preparable
            "SELECT count(*)::int FROM pg_trigger WHERE tgrelid = $1::regclass AND tgname LIKE 'did_%'"
            (Encoders.param (Encoders.nonNullable Encoders.text))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int4))))
    runSession pool session

queryGlobalInvalidationTriggerCount :: Hasql.Pool.Pool -> Text -> IO Int
queryGlobalInvalidationTriggerCount pool tableName = do
    let session = Session.statement (cs tableName) $ Statement.preparable
            "SELECT count(*)::int FROM pg_trigger WHERE tgrelid = $1::regclass AND tgname = 'ihp_datasync_invalidate'"
            (Encoders.param (Encoders.nonNullable Encoders.text))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int4))))
    runSession pool session

withDB :: (Text -> IO ()) -> IO ()
withDB action = do
    available <- canConnectToPostgres
    if available
        then withTestDatabase action
        else pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
