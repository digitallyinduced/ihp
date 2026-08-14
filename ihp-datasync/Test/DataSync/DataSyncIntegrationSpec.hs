{-# LANGUAGE UndecidableInstances #-}
module DataSync.DataSyncIntegrationSpec where

import Test.Hspec
import IHP.Prelude
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)
import IHP.DataSync.ControllerImpl (runDataSyncController, decodeDataSyncMessageEnvelope)
import IHP.DataSync.Types
import IHP.DataSync.DynamicQuery (Field(..))
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)
import IHP.DataSync.RowLevelSecurity (TableWithRLS(..), makeCachedEnsureRLSEnabled)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.RequestVault (pgListenerVaultKey, frameworkConfigVaultKey)
import IHP.LoginSupport.Types (HasNewSessionUrl(..), CurrentUserRecord, currentUserVaultKey)
import qualified IHP.ModelSupport as ModelSupport
import IHP.ModelSupport (noopLogger)
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
import qualified IHP.PGListener as PGListener
import IHP.FrameworkConfig (buildFrameworkConfig)
import IHP.FrameworkConfig.Types

import qualified Data.Vault.Lazy as Vault
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import Network.Wai (defaultRequest, vault)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

-- | Define CurrentUserRecord for this test module
data TestUser = TestUser { id :: Id' "test_users" }
    deriving (Show, Typeable)

type instance CurrentUserRecord = TestUser
type instance GetTableName TestUser = "test_users"
type instance PrimaryKey "test_users" = UUID

instance HasNewSessionUrl TestUser where
    newSessionUrl _ = "/"

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
    [ Hasql.Pool.Config.size 4
    , Hasql.Pool.Config.staticConnectionSettings
        (HasqlSettings.connectionString connStr)
    ]

-- | Run a raw SQL statement on a pool (for test setup)
execSQL :: Hasql.Pool.Pool -> ByteString -> IO ()
execSQL pool sql = runSession pool (Session.script (cs sql))

-- | Check if we can connect to Postgres
canConnectToPostgres :: IO Bool
canConnectToPostgres = do
    masterUrl <- getMasterDatabaseUrl
    result <- Exception.try $ Exception.bracket (makePool masterUrl) Hasql.Pool.release
        (\pool -> execSQL pool "SELECT 1")
    case result of
        Left (_ :: Exception.SomeException) -> pure False
        Right _ -> pure True

-- | Create a temporary test database, run the action, then drop it
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
    let name = "ihp_test_datasync_" <> (uuid |> UUID.toText |> Text.replace "-" "_")
    pure name

-- | Create a hasql pool, run the action, then release
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

-- | Set up the test database schema
setupTestSchema :: Hasql.Pool.Pool -> IO ()
setupTestSchema pool = do
    execSQL pool "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""
    execSQL pool "CREATE TABLE test_users (id UUID PRIMARY KEY DEFAULT gen_random_uuid())"
    execSQL pool "CREATE TABLE messages (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), user_id UUID NOT NULL, body TEXT NOT NULL)"
    execSQL pool "CREATE TABLE idless_resources (owner_id UUID NOT NULL, body TEXT NOT NULL)"
    execSQL pool "CREATE TABLE unrelated_events (body TEXT NOT NULL)"
    -- Deliberately has no id column. Writes here can change message visibility
    -- through the EXISTS policy and must use the global statement trigger.
    execSQL pool "CREATE TABLE message_memberships (user_id UUID NOT NULL, message_id UUID NOT NULL REFERENCES messages(id) ON DELETE CASCADE, PRIMARY KEY (user_id, message_id))"
    execSQL pool "CREATE OR REPLACE FUNCTION public.ihp_user_id() RETURNS UUID AS $$ SELECT NULLIF(current_setting('rls.ihp_user_id'), '')::uuid $$ LANGUAGE SQL STABLE"
    execSQL pool "ALTER TABLE messages ENABLE ROW LEVEL SECURITY"
    execSQL pool "CREATE POLICY messages_policy ON messages USING (user_id = public.ihp_user_id())"
    execSQL pool "CREATE POLICY messages_membership_policy ON messages USING (EXISTS (SELECT 1 FROM message_memberships WHERE message_memberships.message_id = messages.id AND message_memberships.user_id = public.ihp_user_id()))"
    execSQL pool "ALTER TABLE idless_resources ENABLE ROW LEVEL SECURITY"
    execSQL pool "CREATE POLICY idless_resources_policy ON idless_resources USING (owner_id = public.ihp_user_id())"
    -- Create the authenticated role and grant permissions
    execSQL pool "DO $$ BEGIN CREATE ROLE ihp_authenticated NOLOGIN; EXCEPTION WHEN duplicate_object THEN null; END $$"
    execSQL pool "GRANT USAGE ON SCHEMA public TO ihp_authenticated"
    execSQL pool "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO ihp_authenticated"

-- | Insert test data and return the user ID and message ID
insertTestData :: Hasql.Pool.Pool -> IO (UUID, UUID)
insertTestData pool = do
    userId <- UUID.nextRandom
    messageId <- UUID.nextRandom
    execSQL pool (cs ("INSERT INTO test_users (id) VALUES ('" <> UUID.toText userId <> "')"))
    execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText messageId <> "', '" <> UUID.toText userId <> "', 'Hello')"))
    pure (userId, messageId)

-- | Run the DataSync controller with TQueue-based I/O, yielding send/receive handles to the test.
withDataSyncController
    :: Text -- ^ database connection string
    -> UUID -- ^ test user ID
    -> ((ByteString -> IO (), IO DataSyncResponse, Async ()) -> IO a)
    -> IO a
withDataSyncController connStr testUserId action = do
    withHasqlPool connStr \hasqlPool -> do
        -- If connStr already has dbname= prefix, use it directly; otherwise format it
        let actualConnStr = if "dbname=" `Text.isPrefixOf` connStr
                then cs connStr
                else cs ("dbname=" <> connStr)
        let logger = noopLogger
        ModelSupport.withModelContext actualConnStr logger \modelContext -> do
            PGListener.withPGListener actualConnStr logger \pgListener -> do
                frameworkConfig <- buildFrameworkConfig logger (pure ())
                let frameworkConfig' = frameworkConfig { databaseUrl = actualConnStr }

                let testUser = Just (TestUser { id = Id testUserId }) :: Maybe TestUser
                let v = Vault.empty
                        |> Vault.insert pgListenerVaultKey pgListener
                        |> Vault.insert frameworkConfigVaultKey frameworkConfig'
                        |> Vault.insert currentUserVaultKey testUser
                let request = defaultRequest { vault = v }

                -- Set up the request and current user
                let ?request = request
                let ?context = ?request

                -- Create the DataSync state IORef
                stateRef <- newIORef DataSyncController
                let ?state = stateRef

                -- Create TQueues for communication
                inQueue <- newTQueueIO :: IO (TQueue ByteString)
                outQueue <- newTQueueIO :: IO (TQueue DataSyncResponse)

                let receiveData = atomically $ readTQueue inQueue
                let sendJSON response = atomically $ writeTQueue outQueue response

                -- Build the helper functions
                ensureRLSEnabled <- makeCachedEnsureRLSEnabled hasqlPool
                let installTableChangeTriggers = ChangeNotifications.installTableChangeTriggers hasqlPool

                -- Start the controller in an async thread
                let ?modelContext = modelContext
                controllerAsync <- async $
                    runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers receiveData sendJSON (\_ _ -> pure ()) (\_ -> camelCaseRenamer)

                -- Run the test action, then clean up
                Exception.finally
                    (action (\msg -> atomically $ writeTQueue inQueue msg, readResponseWithTimeout outQueue, controllerAsync))
                    (cancel controllerAsync)

-- | Read the next DataSyncResponse with a timeout
readResponseWithTimeout :: TQueue DataSyncResponse -> IO DataSyncResponse
readResponseWithTimeout outQueue = do
    result <- race (threadDelay 5_000_000) (atomically $ readTQueue outQueue)
    case result of
        Left () -> error "readResponse: timed out waiting for DataSync response"
        Right response -> pure response

-- | Encode a DataSyncQuery message as JSON
encodeDataSyncQuery :: Text -> Int -> Maybe UUID -> ByteString
encodeDataSyncQuery table requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("DataSyncQuery" :: Text)
    , "query" .= object
        [ "table" .= table
        , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
        , "whereCondition" .= Null
        , "orderByClause" .= ([] :: [Value])
        , "distinctOnColumn" .= Null
        , "limit" .= Null
        , "offset" .= Null
        ]
    , "requestId" .= requestId
    , "transactionId" .= transactionId
    ]

-- | Encode a CreateRecordMessage as JSON
encodeCreateRecord :: Text -> [(Text, Value)] -> Int -> Maybe UUID -> ByteString
encodeCreateRecord table fields requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("CreateRecordMessage" :: Text)
    , "table" .= table
    , "record" .= object (map (\(k, v) -> (cs k) .= v) fields)
    , "requestId" .= requestId
    , "transactionId" .= transactionId
    ]

-- | Encode an UpdateRecordMessage as JSON
encodeUpdateRecord :: Text -> UUID -> [(Text, Value)] -> Int -> Maybe UUID -> ByteString
encodeUpdateRecord table recordId patch requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("UpdateRecordMessage" :: Text)
    , "table" .= table
    , "id" .= recordId
    , "patch" .= object (map (\(k, v) -> (cs k) .= v) patch)
    , "requestId" .= requestId
    , "transactionId" .= transactionId
    ]

-- | Encode a DeleteRecordMessage as JSON
encodeDeleteRecord :: Text -> UUID -> Int -> Maybe UUID -> ByteString
encodeDeleteRecord table recordId requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("DeleteRecordMessage" :: Text)
    , "table" .= table
    , "id" .= recordId
    , "requestId" .= requestId
    , "transactionId" .= transactionId
    ]

-- | Encode a CreateDataSubscription as JSON
encodeCreateDataSubscription :: Text -> Int -> ByteString
encodeCreateDataSubscription table requestId = encodeCreateDataSubscriptionQueryWithProtocol Nothing requestId $ object
    [ "table" .= table
    , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
    , "whereCondition" .= Null
    , "orderByClause" .= ([] :: [Value])
    , "distinctOnColumn" .= Null
    , "limit" .= Null
    , "offset" .= Null
    ]

encodeCreateDataSubscriptionV2 :: Text -> Int -> ByteString
encodeCreateDataSubscriptionV2 table requestId = encodeCreateDataSubscriptionQueryWithProtocol (Just 1) requestId $ object
    [ "table" .= table
    , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
    , "whereCondition" .= Null
    , "orderByClause" .= ([] :: [Value])
    , "distinctOnColumn" .= Null
    , "limit" .= Null
    , "offset" .= Null
    ]

encodeCreateDataSubscriptionQuery :: Int -> Value -> ByteString
encodeCreateDataSubscriptionQuery = encodeCreateDataSubscriptionQueryWithProtocol Nothing

encodeCreateDataSubscriptionQueryV2 :: Int -> Value -> ByteString
encodeCreateDataSubscriptionQueryV2 = encodeCreateDataSubscriptionQueryWithProtocol (Just 1)

encodeCreateDataSubscriptionQueryWithProtocol :: Maybe Int -> Int -> Value -> ByteString
encodeCreateDataSubscriptionQueryWithProtocol protocolVersion requestId query = cs $ Aeson.encode $ object $
    [ "tag" .= ("CreateDataSubscription" :: Text)
    , "query" .= query
    , "requestId" .= requestId
    ] <> maybe [] (\version -> ["protocolVersion" .= version]) protocolVersion

encodeCreateCountSubscription :: Text -> Int -> ByteString
encodeCreateCountSubscription table requestId = cs $ Aeson.encode $ object
    [ "tag" .= ("CreateCountSubscription" :: Text)
    , "query" .= object
        [ "table" .= table
        , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
        , "whereCondition" .= Null
        , "orderByClause" .= ([] :: [Value])
        , "distinctOnColumn" .= Null
        , "limit" .= Null
        , "offset" .= Null
        ]
    , "requestId" .= requestId
    ]

-- | Encode a DeleteDataSubscription as JSON
encodeDeleteDataSubscription :: UUID -> Int -> ByteString
encodeDeleteDataSubscription subscriptionId requestId = cs $ Aeson.encode $ object
    [ "tag" .= ("DeleteDataSubscription" :: Text)
    , "subscriptionId" .= subscriptionId
    , "requestId" .= requestId
    ]

-- | Encode a StartTransaction as JSON
encodeStartTransaction :: Int -> ByteString
encodeStartTransaction requestId = cs $ Aeson.encode $ object
    [ "tag" .= ("StartTransaction" :: Text)
    , "requestId" .= requestId
    ]

-- | Encode a CommitTransaction as JSON
encodeCommitTransaction :: Int -> UUID -> ByteString
encodeCommitTransaction requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("CommitTransaction" :: Text)
    , "requestId" .= requestId
    , "id" .= transactionId
    ]

-- | Encode a RollbackTransaction as JSON
encodeRollbackTransaction :: Int -> UUID -> ByteString
encodeRollbackTransaction requestId transactionId = cs $ Aeson.encode $ object
    [ "tag" .= ("RollbackTransaction" :: Text)
    , "requestId" .= requestId
    , "id" .= transactionId
    ]

-- | Find a field value by name in a list of Fields
findField :: Text -> [Field] -> Maybe Value
findField name fields = case filter (\f -> f.fieldName == name) fields of
    (f:_) -> Just f.fieldValue
    [] -> Nothing

eventSubscriptionId :: DataSyncResponse -> Maybe UUID
eventSubscriptionId DidDelete { subscriptionId } = Just subscriptionId
eventSubscriptionId DidInsert { subscriptionId } = Just subscriptionId
eventSubscriptionId DidUpdate { subscriptionId } = Just subscriptionId
eventSubscriptionId _ = Nothing

tests :: Spec
tests = do
    describe "DataSync protocol envelope" do
        it "keeps the legacy Haskell constructor while detecting snapshot capability" do
            case decodeDataSyncMessageEnvelope (encodeCreateDataSubscription "messages" 1) of
                Right (Nothing, CreateDataSubscription { requestId }) -> requestId `shouldBe` 1
                _ -> expectationFailure "Expected legacy CreateDataSubscription envelope"

            case decodeDataSyncMessageEnvelope (encodeCreateDataSubscriptionV2 "messages" 2) of
                Right (Just protocolVersion, CreateDataSubscription { requestId }) -> do
                    protocolVersion `shouldBe` 1
                    requestId `shouldBe` 2
                _ -> expectationFailure "Expected snapshot-capable CreateDataSubscription envelope"

        it "preserves the legacy create response and uses an explicit V2 response" do
            subscriptionId <- UUID.nextRandom
            case Aeson.toJSON DidCreateDataSubscription
                    { requestId = 1
                    , subscriptionId
                    , result = []
                    } of
                Object fields -> AesonKeyMap.lookup "revision" fields `shouldBe` Nothing
                _ -> expectationFailure "Expected object response"

            case Aeson.toJSON DidCreateDataSubscriptionV2
                    { requestId = 2
                    , subscriptionId
                    , revision = 0
                    , result = []
                    } of
                Object fields -> AesonKeyMap.lookup "revision" fields `shouldBe` Just (Number 0)
                _ -> expectationFailure "Expected object response"

    describe "IHP.DataSync Integration" do
        describe "DataSyncQuery" do
            it "returns rows from a table with RLS" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeDataSyncQuery "messages" 1 Nothing)
                            response <- recv
                            case response of
                                DataSyncResult { result, requestId } -> do
                                    requestId `shouldBe` 1
                                    length result `shouldBe` 1
                                    case result of
                                        (row:_) -> do
                                            findField "id" row `shouldBe` Just (String (UUID.toText messageId))
                                            findField "body" row `shouldBe` Just (String "Hello")
                                        [] -> expectationFailure "Expected at least one row"
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DataSyncResult"

        describe "CreateRecordMessage" do
            it "creates a record and returns it" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateRecord "messages"
                                [ ("userId", String (UUID.toText userId))
                                , ("body", String "New message")
                                ] 2 Nothing)
                            response <- recv
                            case response of
                                DidCreateRecord { record, requestId } -> do
                                    requestId `shouldBe` 2
                                    findField "body" record `shouldBe` Just (String "New message")
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DidCreateRecord"

        describe "UpdateRecordMessage" do
            it "updates a record and returns the updated version" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeUpdateRecord "messages" messageId
                                [ ("body", String "Updated message") ] 3 Nothing)
                            response <- recv
                            case response of
                                DidUpdateRecord { record, requestId } -> do
                                    requestId `shouldBe` 3
                                    findField "body" record `shouldBe` Just (String "Updated message")
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DidUpdateRecord"

        describe "DeleteRecordMessage" do
            it "deletes a record" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeDeleteRecord "messages" messageId 4 Nothing)
                            response <- recv
                            case response of
                                DidDeleteRecord { requestId } ->
                                    requestId `shouldBe` 4
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DidDeleteRecord"

                            -- Verify the record is gone by querying
                            send (encodeDataSyncQuery "messages" 5 Nothing)
                            response2 <- recv
                            case response2 of
                                DataSyncResult { result } ->
                                    length result `shouldBe` 0
                                _ -> expectationFailure "Expected DataSyncResult"

        describe "RLS enforcement" do
            it "only returns records visible to the current user" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        -- Insert a message belonging to a different user
                        otherUserId <- UUID.nextRandom
                        otherMsgId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO test_users (id) VALUES ('" <> UUID.toText otherUserId <> "')"))
                        execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText otherMsgId <> "', '" <> UUID.toText otherUserId <> "', 'Other user message')"))

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeDataSyncQuery "messages" 6 Nothing)
                            response <- recv
                            case response of
                                DataSyncResult { result } ->
                                    -- Should only see the message belonging to our test user
                                    length result `shouldBe` 1
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DataSyncResult"

            it "rejects queries on tables without RLS" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        -- test_users does NOT have RLS enabled
                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeDataSyncQuery "test_users" 7 Nothing)
                            response <- recv
                            case response of
                                DataSyncError {} -> pure ()
                                _ -> expectationFailure "Expected DataSyncError for table without RLS"

        describe "Subscriptions" do
            it "creates and deletes a data subscription" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscription "messages" 8)
                            response <- recv
                            case response of
                                DidCreateDataSubscription { requestId, subscriptionId, result } -> do
                                    requestId `shouldBe` 8
                                    length result `shouldBe` 1

                                    -- Now delete the subscription
                                    send (encodeDeleteDataSubscription subscriptionId 9)
                                    response2 <- recv
                                    case response2 of
                                        DidDeleteDataSubscription { requestId, subscriptionId = deletedId } -> do
                                            requestId `shouldBe` 9
                                            deletedId `shouldBe` subscriptionId
                                        _ -> expectationFailure "Expected DidDeleteDataSubscription"

                                    -- The delete acknowledgement is a lifecycle barrier:
                                    -- no callback may send after it.
                                    insertedId <- UUID.nextRandom
                                    execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText insertedId <> "', '" <> UUID.toText userId <> "', 'After delete')"))
                                    afterDelete <- race (threadDelay 300_000) recv
                                    case afterDelete of
                                        Left () -> pure ()
                                        Right _ -> expectationFailure "Expected no subscription message after delete acknowledgement"
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DidCreateDataSubscription"

            it "keeps legacy clients on RLS-derived delta messages" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            -- No protocolVersion: the old response constructor and old
                            -- delta tags remain wire-compatible.
                            send (encodeCreateDataSubscription "messages" 90)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscription { subscriptionId, result } -> do
                                    length result `shouldBe` 1
                                    pure subscriptionId
                                _ -> expectationFailure "Expected legacy DidCreateDataSubscription" >> error "unreachable"

                            execSQL pool (cs ("UPDATE messages SET body = 'Legacy safe' WHERE id = '" <> UUID.toText messageId <> "'"))

                            cleared <- recv
                            case cleared of
                                DidDelete { subscriptionId = deletedSubscriptionId, id } -> do
                                    deletedSubscriptionId `shouldBe` subscriptionId
                                    id `shouldBe` messageId
                                _ -> expectationFailure "Expected RLS-derived legacy delete"

                            reinserted <- recv
                            case reinserted of
                                DidInsert { subscriptionId = insertedSubscriptionId, record } -> do
                                    insertedSubscriptionId `shouldBe` subscriptionId
                                    findField "id" record `shouldBe` Just (String (UUID.toText messageId))
                                    findField "body" record `shouldBe` Just (String "Legacy safe")
                                _ -> expectationFailure "Expected RLS-derived legacy insert"

            it "invalidates V2 snapshots when an id-less RLS membership table grants and revokes access" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, ownMessageId) <- insertTestData pool
                        otherUserId <- UUID.nextRandom
                        sharedMessageId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText sharedMessageId <> "', '" <> UUID.toText otherUserId <> "', 'Shared')"))

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "messages" 93)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscriptionV2 { subscriptionId, result } -> do
                                    length result `shouldBe` 1
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateDataSubscriptionV2" >> error "unreachable"

                            execSQL pool (cs ("INSERT INTO message_memberships (user_id, message_id) VALUES ('" <> UUID.toText userId <> "', '" <> UUID.toText sharedMessageId <> "')"))
                            granted <- recv
                            case granted of
                                DidReplaceDataSubscription { subscriptionId = replacedId, revision, result } -> do
                                    replacedId `shouldBe` subscriptionId
                                    revision `shouldBe` 1
                                    Set.fromList (mapMaybe (findField "id") result) `shouldBe`
                                        Set.fromList [String (UUID.toText ownMessageId), String (UUID.toText sharedMessageId)]
                                _ -> expectationFailure "Expected replacement after membership grant"

                            execSQL pool (cs ("DELETE FROM message_memberships WHERE user_id = '" <> UUID.toText userId <> "' AND message_id = '" <> UUID.toText sharedMessageId <> "'"))
                            revoked <- recv
                            case revoked of
                                DidReplaceDataSubscription { revision, result } -> do
                                    revision `shouldBe` 2
                                    length result `shouldBe` 1
                                    map (findField "id") result `shouldNotContain` [Just (String (UUID.toText sharedMessageId))]
                                _ -> expectationFailure "Expected replacement after membership revoke"

            it "subscribes only to direct RLS relation dependencies" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        otherUserId <- UUID.nextRandom
                        sharedMessageId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText sharedMessageId <> "', '" <> UUID.toText otherUserId <> "', 'Scoped shared')"))

                        channels <- ChangeNotifications.invalidationChannelsForTable pool (TableWithRLS "messages")
                        Set.size channels `shouldBe` 2 -- messages + message_memberships
                        Set.member ChangeNotifications.globalInvalidationChannel channels `shouldBe` False

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "messages" 102)
                            _ <- recv

                            -- unrelated_events has its own relation channel, so
                            -- this write cannot wake/requery the messages worker.
                            execSQL pool "INSERT INTO unrelated_events (body) VALUES ('unrelated')"
                            unrelatedResponse <- race (threadDelay 300_000) recv
                            case unrelatedResponse of
                                Left () -> pure ()
                                Right _ -> expectationFailure "Unrelated relation woke messages subscription"

                            execSQL pool (cs ("INSERT INTO message_memberships (user_id, message_id) VALUES ('" <> UUID.toText userId <> "', '" <> UUID.toText sharedMessageId <> "')"))
                            membershipResponse <- recv
                            case membershipResponse of
                                DidReplaceDataSubscription { result } ->
                                    mapMaybe (findField "id") result `shouldContain`
                                        [String (UUID.toText sharedMessageId)]
                                _ -> expectationFailure "Membership dependency did not refresh subscription"

            it "uses global fallback for an opaque replacement of ihp_user_id" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        execSQL pool "CREATE OR REPLACE FUNCTION public.ihp_user_id() RETURNS UUID AS $$ SELECT CASE WHEN EXISTS (SELECT 1 FROM unrelated_events) THEN NULLIF(current_setting('rls.ihp_user_id'), '')::uuid ELSE NULL::uuid END $$ LANGUAGE SQL STABLE"

                        channels <- ChangeNotifications.invalidationChannelsForTable pool (TableWithRLS "messages")
                        Set.member ChangeNotifications.globalInvalidationChannel channels `shouldBe` True

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "messages" 103)
                            created <- recv
                            case created of
                                DidCreateDataSubscriptionV2 { result } -> length result `shouldBe` 0
                                _ -> expectationFailure "Expected opaque-policy subscription"

                            execSQL pool "INSERT INTO unrelated_events (body) VALUES ('unlock visibility')"
                            refreshed <- recv
                            case refreshed of
                                DidReplaceDataSubscription { result } -> length result `shouldBe` 1
                                _ -> expectationFailure "Opaque policy global fallback did not refresh"

            it "invalidates a parent subscription after direct partition-child DML" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        execSQL pool "CREATE TABLE partitioned_messages (id UUID NOT NULL, user_id UUID NOT NULL, body TEXT NOT NULL, bucket INT NOT NULL) PARTITION BY LIST (bucket)"
                        execSQL pool "CREATE TABLE partitioned_messages_one PARTITION OF partitioned_messages FOR VALUES IN (1)"
                        execSQL pool "ALTER TABLE partitioned_messages ENABLE ROW LEVEL SECURITY"
                        execSQL pool "CREATE POLICY partitioned_messages_policy ON partitioned_messages USING (user_id = public.ihp_user_id())"
                        execSQL pool "GRANT ALL PRIVILEGES ON partitioned_messages, partitioned_messages_one TO ihp_authenticated"

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "partitioned_messages" 104)
                            created <- recv
                            case created of
                                DidCreateDataSubscriptionV2 { result } -> length result `shouldBe` 0
                                _ -> expectationFailure "Expected partition-parent subscription"

                            insertedId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO partitioned_messages_one (id, user_id, body, bucket) VALUES ('" <> UUID.toText insertedId <> "', '" <> UUID.toText userId <> "', 'Child write', 1)"))
                            refreshed <- recv
                            case refreshed of
                                DidReplaceDataSubscription { result } ->
                                    map (findField "id") result `shouldBe` [Just (String (UUID.toText insertedId))]
                                _ -> expectationFailure "Direct partition-child write did not invalidate parent"

            it "invalidates count subscriptions through an id-less RLS membership table" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        otherUserId <- UUID.nextRandom
                        sharedMessageId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText sharedMessageId <> "', '" <> UUID.toText otherUserId <> "', 'Shared count')"))

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateCountSubscription "messages" 94)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateCountSubscription { subscriptionId, count } -> do
                                    count `shouldBe` 1
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateCountSubscription" >> error "unreachable"

                            execSQL pool (cs ("INSERT INTO message_memberships (user_id, message_id) VALUES ('" <> UUID.toText userId <> "', '" <> UUID.toText sharedMessageId <> "')"))
                            granted <- recv
                            case granted of
                                DidChangeCount { subscriptionId = changedSubscriptionId, count } -> do
                                    changedSubscriptionId `shouldBe` subscriptionId
                                    count `shouldBe` 2
                                _ -> expectationFailure "Expected count after membership grant"

                            execSQL pool (cs ("DELETE FROM message_memberships WHERE user_id = '" <> UUID.toText userId <> "' AND message_id = '" <> UUID.toText sharedMessageId <> "'"))
                            revoked <- recv
                            case revoked of
                                DidChangeCount { subscriptionId = changedSubscriptionId, count } -> do
                                    changedSubscriptionId `shouldBe` subscriptionId
                                    count `shouldBe` 1
                                _ -> expectationFailure "Expected count after membership revoke"

            it "keeps legacy RLS membership invalidations snapshot-safe" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, ownMessageId) <- insertTestData pool
                        otherUserId <- UUID.nextRandom
                        sharedMessageId <- UUID.nextRandom
                        execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText sharedMessageId <> "', '" <> UUID.toText otherUserId <> "', 'Legacy shared')"))

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscription "messages" 95)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscription { subscriptionId } -> pure subscriptionId
                                _ -> expectationFailure "Expected legacy subscription" >> error "unreachable"

                            execSQL pool (cs ("INSERT INTO message_memberships (user_id, message_id) VALUES ('" <> UUID.toText userId <> "', '" <> UUID.toText sharedMessageId <> "')"))
                            grantEvents <- sequence [recv, recv, recv, recv]
                            let grantedDeletes = Set.fromList [id | DidDelete { id } <- grantEvents]
                            let grantedInserts = Set.fromList [value | DidInsert { record } <- grantEvents, Just value <- [findField "id" record]]
                            grantedDeletes `shouldBe` Set.fromList [ownMessageId, sharedMessageId]
                            grantedInserts `shouldBe` Set.fromList [String (UUID.toText ownMessageId), String (UUID.toText sharedMessageId)]
                            all (\event -> eventSubscriptionId event == Just subscriptionId) grantEvents `shouldBe` True

                            execSQL pool (cs ("DELETE FROM message_memberships WHERE user_id = '" <> UUID.toText userId <> "' AND message_id = '" <> UUID.toText sharedMessageId <> "'"))
                            revokeEvents <- sequence [recv, recv, recv]
                            let revokedDeletes = Set.fromList [id | DidDelete { id } <- revokeEvents]
                            let revokedInserts = [record | DidInsert { record } <- revokeEvents]
                            revokedDeletes `shouldBe` Set.fromList [ownMessageId, sharedMessageId]
                            map (findField "id") revokedInserts `shouldBe` [Just (String (UUID.toText ownMessageId))]

            it "rejects legacy projections without id while V2 projections remain authoritative" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool
                        let projectedQuery = object
                                [ "table" .= ("messages" :: Text)
                                , "selectedColumns" .= object
                                    [ "tag" .= ("SelectSpecific" :: Text)
                                    , "contents" .= ["body" :: Text]
                                    ]
                                , "whereCondition" .= Null
                                , "orderByClause" .= ([] :: [Value])
                                , "distinctOnColumn" .= Null
                                , "limit" .= Null
                                , "offset" .= Null
                                ]

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionQuery 96 projectedQuery)
                            legacyResponse <- recv
                            case legacyResponse of
                                DataSyncError { errorMessage } ->
                                    errorMessage `shouldSatisfy` Text.isInfixOf "require the id column"
                                _ -> expectationFailure "Expected explicit legacy projection error"

                            send (encodeCreateDataSubscriptionQueryV2 97 projectedQuery)
                            created <- recv
                            case created of
                                DidCreateDataSubscriptionV2 { result } -> do
                                    map (findField "body") result `shouldBe` [Just (String "Hello")]
                                    map (findField "id") result `shouldBe` [Nothing]
                                _ -> expectationFailure "Expected V2 projected subscription"

                            execSQL pool (cs ("UPDATE messages SET body = 'Projected update' WHERE id = '" <> UUID.toText messageId <> "'"))
                            replaced <- recv
                            case replaced of
                                DidReplaceDataSubscription { revision, result } -> do
                                    revision `shouldBe` 1
                                    map (findField "body") result `shouldBe` [Just (String "Projected update")]
                                    map (findField "id") result `shouldBe` [Nothing]
                                _ -> expectationFailure "Expected projected replacement"

            it "supports V2 and count subscriptions on id-less tables without installing legacy row triggers" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool
                        execSQL pool (cs ("INSERT INTO idless_resources (owner_id, body) VALUES ('" <> UUID.toText userId <> "', 'First')"))

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscription "idless_resources" 98)
                            legacyResponse <- recv
                            case legacyResponse of
                                DataSyncError { errorMessage } ->
                                    errorMessage `shouldSatisfy` Text.isInfixOf "require the id column"
                                _ -> expectationFailure "Expected legacy id-less table rejection"

                            send (encodeCreateDataSubscriptionV2 "idless_resources" 99)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscriptionV2 { subscriptionId, result } -> do
                                    map (findField "body") result `shouldBe` [Just (String "First")]
                                    pure subscriptionId
                                _ -> expectationFailure "Expected id-less V2 subscription" >> error "unreachable"

                            -- This UPDATE would fail inside notify_did_change_* if
                            -- the id-dependent legacy trigger had been installed.
                            execSQL pool "UPDATE idless_resources SET body = 'Updated'"
                            replaced <- recv
                            case replaced of
                                DidReplaceDataSubscription { result } ->
                                    map (findField "body") result `shouldBe` [Just (String "Updated")]
                                _ -> expectationFailure "Expected id-less V2 replacement"

                            send (encodeDeleteDataSubscription subscriptionId 100)
                            _ <- recv
                            pure ()

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateCountSubscription "idless_resources" 101)
                            created <- recv
                            case created of
                                DidCreateCountSubscription { count } -> count `shouldBe` 1
                                _ -> expectationFailure "Expected id-less count subscription"

                            execSQL pool (cs ("INSERT INTO idless_resources (owner_id, body) VALUES ('" <> UUID.toText userId <> "', 'Second')"))
                            changed <- recv
                            case changed of
                                DidChangeCount { count } -> count `shouldBe` 2
                                _ -> expectationFailure "Expected id-less count refresh"

            it "replaces the complete RLS-filtered snapshot with monotonic revisions" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "messages" 10)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscriptionV2 { subscriptionId, revision, result } -> do
                                    revision `shouldBe` 0
                                    length result `shouldBe` 1
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateDataSubscriptionV2" >> error "unreachable"

                            insertedId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText insertedId <> "', '" <> UUID.toText userId <> "', 'Second')"))

                            replacement <- recv
                            case replacement of
                                DidReplaceDataSubscription { subscriptionId = replacedId, revision, result } -> do
                                    replacedId `shouldBe` subscriptionId
                                    revision `shouldBe` 1
                                    length result `shouldBe` 2
                                _ -> expectationFailure "Expected DidReplaceDataSubscription"

                            execSQL pool (cs ("UPDATE messages SET body = 'Updated second' WHERE id = '" <> UUID.toText insertedId <> "'"))

                            replacement2 <- recv
                            case replacement2 of
                                DidReplaceDataSubscription { revision, result } -> do
                                    revision `shouldBe` 2
                                    let updatedRows = filter (\row -> findField "id" row == Just (String (UUID.toText insertedId))) result
                                    map (findField "body") updatedRows `shouldBe` [Just (String "Updated second")]
                                _ -> expectationFailure "Expected second DidReplaceDataSubscription"

            it "does not emit a replacement for an RLS-hidden change" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionV2 "messages" 11)
                            created <- recv
                            case created of
                                DidCreateDataSubscriptionV2 { revision, result } -> do
                                    revision `shouldBe` 0
                                    length result `shouldBe` 1
                                _ -> expectationFailure "Expected DidCreateDataSubscriptionV2"

                            otherUserId <- UUID.nextRandom
                            hiddenMessageId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText hiddenMessageId <> "', '" <> UUID.toText otherUserId <> "', 'Hidden')"))

                            possibleResponse <- race (threadDelay 500_000) recv
                            case possibleResponse of
                                Left () -> pure ()
                                Right _ -> expectationFailure "Expected no response for an RLS-hidden insert"

                            -- A subsequent visible change must still be revision 1. This
                            -- both proves the listener remains live and that the hidden
                            -- notification did not consume a revision.
                            execSQL pool (cs ("UPDATE messages SET body = 'Visible update' WHERE id = '" <> UUID.toText messageId <> "'"))
                            visibleReplacement <- recv
                            case visibleReplacement of
                                DidReplaceDataSubscription { revision, result } -> do
                                    revision `shouldBe` 1
                                    map (findField "body") result `shouldBe` [Just (String "Visible update")]
                                _ -> expectationFailure "Expected DidReplaceDataSubscription for the visible update"

            it "replaces a filtered snapshot when an unwatched row enters the query" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        let filteredQuery = object
                                [ "table" .= ("messages" :: Text)
                                , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
                                , "whereCondition" .= object
                                    [ "tag" .= ("InfixOperatorExpression" :: Text)
                                    , "left" .= object
                                        [ "tag" .= ("ColumnExpression" :: Text)
                                        , "field" .= ("body" :: Text)
                                        ]
                                    , "op" .= ("OpEqual" :: Text)
                                    , "right" .= object
                                        [ "tag" .= ("LiteralExpression" :: Text)
                                        , "value" .= ("Matches" :: Text)
                                        ]
                                    ]
                                , "orderByClause" .= ([] :: [Value])
                                , "distinctOnColumn" .= Null
                                , "limit" .= Null
                                , "offset" .= Null
                                ]

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionQueryV2 12 filteredQuery)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscriptionV2 { subscriptionId, revision, result } -> do
                                    revision `shouldBe` 0
                                    length result `shouldBe` 0
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateDataSubscriptionV2" >> error "unreachable"

                            -- DidCreate is sent only after LISTEN is active, so this immediate
                            -- update also exercises the former setup race.
                            execSQL pool (cs ("UPDATE messages SET body = 'Matches' WHERE id = '" <> UUID.toText messageId <> "'"))
                            entered <- recv
                            case entered of
                                DidReplaceDataSubscription { subscriptionId = replacedId, revision, result } -> do
                                    replacedId `shouldBe` subscriptionId
                                    revision `shouldBe` 1
                                    map (findField "id") result `shouldBe` [Just (String (UUID.toText messageId))]
                                _ -> expectationFailure "Expected row-entered replacement"

                            execSQL pool (cs ("UPDATE messages SET body = 'No longer' WHERE id = '" <> UUID.toText messageId <> "'"))
                            left <- recv
                            case left of
                                DidReplaceDataSubscription { revision, result } -> do
                                    revision `shouldBe` 2
                                    length result `shouldBe` 0
                                _ -> expectationFailure "Expected row-left replacement"

            it "replaces a limited ordered window when a row is displaced" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, messageId) <- insertTestData pool

                        let limitedQuery = object
                                [ "table" .= ("messages" :: Text)
                                , "selectedColumns" .= object ["tag" .= ("SelectAll" :: Text)]
                                , "whereCondition" .= Null
                                , "orderByClause" .=
                                    [ object
                                        [ "orderByColumn" .= ("body" :: Text)
                                        , "orderByDirection" .= ("Asc" :: Text)
                                        ]
                                    ]
                                , "distinctOnColumn" .= Null
                                , "limit" .= (1 :: Int)
                                , "offset" .= Null
                                ]

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateDataSubscriptionQueryV2 13 limitedQuery)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateDataSubscriptionV2 { subscriptionId, revision, result } -> do
                                    revision `shouldBe` 0
                                    map (findField "id") result `shouldBe` [Just (String (UUID.toText messageId))]
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateDataSubscriptionV2" >> error "unreachable"

                            insertedId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText insertedId <> "', '" <> UUID.toText userId <> "', 'A first')"))
                            displaced <- recv
                            case displaced of
                                DidReplaceDataSubscription { subscriptionId = replacedId, revision, result } -> do
                                    replacedId `shouldBe` subscriptionId
                                    revision `shouldBe` 1
                                    map (findField "id") result `shouldBe` [Just (String (UUID.toText insertedId))]
                                _ -> expectationFailure "Expected limited-window replacement"

            it "authoritatively refreshes and deletes a count subscription" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeCreateCountSubscription "messages" 91)
                            created <- recv
                            subscriptionId <- case created of
                                DidCreateCountSubscription { subscriptionId, count } -> do
                                    count `shouldBe` 1
                                    pure subscriptionId
                                _ -> expectationFailure "Expected DidCreateCountSubscription" >> error "unreachable"

                            insertedId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText insertedId <> "', '" <> UUID.toText userId <> "', 'Counted')"))
                            changed <- recv
                            case changed of
                                DidChangeCount { subscriptionId = changedSubscriptionId, count } -> do
                                    changedSubscriptionId `shouldBe` subscriptionId
                                    count `shouldBe` 2
                                _ -> expectationFailure "Expected DidChangeCount"

                            send (encodeDeleteDataSubscription subscriptionId 92)
                            deleted <- recv
                            case deleted of
                                DidDeleteDataSubscription { subscriptionId = deletedSubscriptionId } ->
                                    deletedSubscriptionId `shouldBe` subscriptionId
                                _ -> expectationFailure "Expected DidDeleteDataSubscription"

                            anotherId <- UUID.nextRandom
                            execSQL pool (cs ("INSERT INTO messages (id, user_id, body) VALUES ('" <> UUID.toText anotherId <> "', '" <> UUID.toText userId <> "', 'After count delete')"))
                            afterDelete <- race (threadDelay 300_000) recv
                            case afterDelete of
                                Left () -> pure ()
                                Right _ -> expectationFailure "Expected no count message after delete acknowledgement"

        describe "Transactions" do
            it "starts, uses, and commits a transaction" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeStartTransaction 10)
                            response <- recv
                            case response of
                                DidStartTransaction { requestId, transactionId } -> do
                                    requestId `shouldBe` 10

                                    -- Create a record within the transaction
                                    send (encodeCreateRecord "messages"
                                        [ ("userId", String (UUID.toText userId))
                                        , ("body", String "Transactional message")
                                        ] 11 (Just transactionId))
                                    response2 <- recv
                                    case response2 of
                                        DidCreateRecord {} -> pure ()
                                        DataSyncError { errorMessage } ->
                                            expectationFailure (cs $ "Create in txn failed: " <> errorMessage)
                                        _ -> expectationFailure "Expected DidCreateRecord"

                                    -- Commit the transaction
                                    send (encodeCommitTransaction 12 transactionId)
                                    response3 <- recv
                                    case response3 of
                                        DidCommitTransaction { requestId } ->
                                            requestId `shouldBe` 12
                                        _ -> expectationFailure "Expected DidCommitTransaction"

                                    -- Verify the record persists after commit
                                    send (encodeDataSyncQuery "messages" 13 Nothing)
                                    response4 <- recv
                                    case response4 of
                                        DataSyncResult { result } ->
                                            -- Original + transactional message
                                            length result `shouldBe` 2
                                        _ -> expectationFailure "Expected DataSyncResult"
                                _ -> expectationFailure "Expected DidStartTransaction"

            it "rolls back a transaction" do
                withDB \connStr -> do
                    withHasqlPool connStr \pool -> do
                        setupTestSchema pool
                        (userId, _) <- insertTestData pool

                        withDataSyncController connStr userId \(send, recv, _) -> do
                            send (encodeStartTransaction 20)
                            response <- recv
                            case response of
                                DidStartTransaction { transactionId } -> do
                                    -- Create a record in the transaction
                                    send (encodeCreateRecord "messages"
                                        [ ("userId", String (UUID.toText userId))
                                        , ("body", String "Will be rolled back")
                                        ] 21 (Just transactionId))
                                    _ <- recv  -- DidCreateRecord

                                    -- Rollback
                                    send (encodeRollbackTransaction 22 transactionId)
                                    response3 <- recv
                                    case response3 of
                                        DidRollbackTransaction { requestId } ->
                                            requestId `shouldBe` 22
                                        _ -> expectationFailure "Expected DidRollbackTransaction"

                                    -- Verify the record was NOT persisted
                                    send (encodeDataSyncQuery "messages" 23 Nothing)
                                    response4 <- recv
                                    case response4 of
                                        DataSyncResult { result } ->
                                            length result `shouldBe` 1 -- only the original
                                        _ -> expectationFailure "Expected DataSyncResult"
                                _ -> expectationFailure "Expected DidStartTransaction"
