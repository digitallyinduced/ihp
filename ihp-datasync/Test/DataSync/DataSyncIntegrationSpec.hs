{-# LANGUAGE UndecidableInstances, DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Test.DataSync.DataSyncIntegrationSpec where

import Test.Hspec
import IHP.Prelude
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)
import IHP.DataSync.ControllerImpl (runDataSyncController)
import IHP.DataSync.Types
import IHP.DataSync.DynamicQuery (Field(..))
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)
import IHP.DataSync.RowLevelSecurity (makeCachedEnsureRLSEnabled)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.RequestVault (pgListenerVaultKey, frameworkConfigVaultKey)
import IHP.Controller.Context (newControllerContext, putContext, freeze)
import IHP.LoginSupport.Types (HasNewSessionUrl(..), CurrentUserRecord)
import qualified IHP.ModelSupport as ModelSupport
import IHP.ModelSupport.Types (PrimaryKey)
import qualified IHP.PGListener as PGListener
import IHP.FrameworkConfig (buildFrameworkConfig)
import IHP.FrameworkConfig.Types

import qualified Data.Vault.Lazy as Vault
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import System.Environment (lookupEnv)
import Network.Wai (defaultRequest, vault)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import qualified IHP.Log as Log

-- | Define CurrentUserRecord for this test module
newtype TestUserId = TestUserId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "test_users" = TestUserId
instance IdNewtype TestUserId UUID where
    toId = TestUserId
    fromId (TestUserId x) = x

data TestUser = TestUser { id :: TestUserId }
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
    execSQL pool "ALTER TABLE messages ENABLE ROW LEVEL SECURITY"
    execSQL pool "CREATE POLICY messages_policy ON messages USING (user_id = current_setting('rls.ihp_user_id')::uuid)"
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
        logger <- Log.newLogger def { Log.level = Log.Error }
        ModelSupport.withModelContext actualConnStr logger \modelContext -> do
            PGListener.withPGListener actualConnStr logger \pgListener -> do
                frameworkConfig <- buildFrameworkConfig (pure ())
                let frameworkConfig' = frameworkConfig { databaseUrl = actualConnStr }

                let v = Vault.empty
                        |> Vault.insert pgListenerVaultKey pgListener
                        |> Vault.insert frameworkConfigVaultKey frameworkConfig'
                let request = defaultRequest { vault = v }

                -- Set up ControllerContext with the request and current user
                let ?request = request
                context <- newControllerContext
                let ?context = context

                -- Put the current user into context so currentUserOrNothing can find it
                putContext (Just (TestUser { id = Id testUserId }) :: Maybe TestUser)

                -- Freeze the context so it can be accessed from pure code
                frozenContext <- freeze ?context
                let ?context = frozenContext

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
encodeCreateDataSubscription table requestId = cs $ Aeson.encode $ object
    [ "tag" .= ("CreateDataSubscription" :: Text)
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

tests :: Spec
tests = do
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
                                DataSyncError { errorMessage } ->
                                    expectationFailure (cs $ "Unexpected error: " <> errorMessage)
                                _ -> expectationFailure "Expected DidCreateDataSubscription"

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
