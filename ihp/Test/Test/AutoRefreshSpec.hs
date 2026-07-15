{-|
Module: Test.AutoRefreshSpec
Tests that AutoRefresh preserves query parameters when re-rendering
with a bare WebSocket request (no query params).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.AutoRefreshSpec where
import Test.Hspec
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai
import Network.HTTP.Types
import IHP.AutoRefresh
    ( addQueryBuilderReadDependency
    , addWholeTableReadDependency
    , commitAutoRefreshReadDependencies
    , globalAutoRefreshServerVar
    , sessionResponseHasChanged
    , updateSession
    , withAutoRefreshReadTracker
    )
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import IHP.QueryBuilder.Types (QueryBuilderRead (..), QueryBuilderReadKind (..), QueryBuilderPrimaryKey (..), SQLQuery (..))
import IHP.Hasql.FromRow (FromRowHasql (..), HasqlDecodeColumn (..))
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as HasqlSession
import System.Log.FastLogger (FastLogger)
import System.Environment (lookupEnv)
import IHP.Server (initMiddlewareStack)
import Network.Wai.Test (runSession, request, SResponse(..), simpleBody)
import IHP.Test.Mocking
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai

data AutoRefreshCompositeItem = AutoRefreshCompositeItem
    { tenantId :: UUID
    , itemId :: Int
    , name :: Text
    }
    deriving (Eq, Show)

type instance GetTableName AutoRefreshCompositeItem = "auto_refresh_composite_items"
type instance GetModelByTableName "auto_refresh_composite_items" = AutoRefreshCompositeItem
type instance PrimaryKey "auto_refresh_composite_items" = (UUID, Int)

instance Table AutoRefreshCompositeItem where
    columnNames = ["tenant_id", "item_id", "name"]
    primaryKeyColumnNames = ["tenant_id", "item_id"]

instance FromRowHasql AutoRefreshCompositeItem where
    hasqlRowDecoder = AutoRefreshCompositeItem
        <$> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = ShowItemAction
    | ShowItemHtmlAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action ShowItemAction = autoRefresh do
        let marketId = param @Text "marketId"
        renderPlain (cs marketId)
    action ShowItemHtmlAction = autoRefresh do
        let marketId = param @Text "marketId"
        let meta = autoRefreshMeta
        respondHtml [hsx|<html><head>{meta}</head><body>{marketId}</body></html>|]

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]

instance InitControllerContext WebApplication where
  initContext = pure ()

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config :: ConfigBuilder
config = do
    option Development
    option (AppPort 8000)

-- | Helper that calls a controller action with query parameters (GET-style)
-- and passes a PGListener to the middleware stack so autoRefresh can work.
callActionWithQueryParams
    :: forall application controller
     . ( Controller controller
       , ContextParameters application
       , Typeable application
       , Typeable controller
       )
    => PGListener.PGListener
    -> controller
    -> [(ByteString, ByteString)]
    -> IO SResponse
callActionWithQueryParams pgListener controller queryParams = do
    let MockContext { frameworkConfig, modelContext } = ?mocking

    let baseRequest = ?request
            { Wai.queryString = map (\(k,v) -> (k, Just v)) queryParams
            , Wai.rawQueryString = renderSimpleQuery True queryParams
            }

    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller

    middlewareStack <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
    runSession (request baseRequest) (middlewareStack controllerApp)

testLogger :: FastLogger
testLogger = noopLogger

withAutoRefreshDB :: (ModelContext -> IO ()) -> IO ()
withAutoRefreshDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    modelContext <- createModelContext databaseUrl noopLogger
    let pool = modelContext.hasqlPool
    let setup = runHasqlScript pool
            "DROP TABLE IF EXISTS auto_refresh_composite_items;\
            \CREATE TABLE auto_refresh_composite_items (\
            \tenant_id UUID NOT NULL, item_id INT NOT NULL, name TEXT NOT NULL,\
            \PRIMARY KEY (tenant_id, item_id));\
            \INSERT INTO auto_refresh_composite_items (tenant_id, item_id, name) VALUES\
            \('a0000000-0000-0000-0000-000000000001', 7, 'tracked');"
    let teardown = do
            _ <- HasqlPool.use pool (HasqlSession.script "DROP TABLE IF EXISTS auto_refresh_composite_items;")
            releaseModelContext modelContext
    result <- Exception.try (setup >> action modelContext `Exception.finally` teardown)
    case result of
        Right () -> pure ()
        Left (HasqlError (HasqlPool.ConnectionUsageError _)) ->
            pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
        Left exception -> Exception.throwIO exception

runHasqlScript :: HasqlPool.Pool -> Text -> IO ()
runHasqlScript pool sql = do
    result <- HasqlPool.use pool (HasqlSession.script sql)
    case result of
        Left error -> Exception.throwIO (HasqlError error)
        Right () -> pure ()

getOnlyAutoRefreshSession :: IORef AutoRefreshServer -> IO AutoRefreshSession
getOnlyAutoRefreshSession serverRef = do
    server <- readIORef serverRef
    case server.sessions of
        session:_ -> pure session
        [] -> error "Expected AutoRefresh session"

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "AutoRefresh" do
        describe "read dependency tracking" do
            let sqlQuery = SQLQuery
                    { selectFrom = "tasks"
                    , distinctClause = False
                    , distinctOnClause = Nothing
                    , whereCondition = Nothing
                    , orderByClause = []
                    , limitClause = Nothing
                    , offsetClause = Nothing
                    , columns = ["id"]
                    , columnsSql = "tasks.id"
                    }
            let queryRead = QueryBuilderRead
                    { trackedSqlQuery = sqlQuery
                    , queryBuilderReadKind = QueryBuilderRows
                    , queryBuilderPrimaryKeyColumns = ["id"]
                    , queryBuilderResultPrimaryKeys = Just [QueryBuilderPrimaryKey [Aeson.String "task-1"]]
                    }

            it "retains structured QueryBuilder reads" $ \_ -> do
                let dependencies = addQueryBuilderReadDependency "tasks" queryRead Map.empty
                case Map.lookup "tasks" dependencies of
                    Just (AutoRefreshQueryBuilderReads [trackedRead]) -> do
                        trackedRead.trackedSqlQuery.selectFrom `shouldBe` "tasks"
                        trackedRead.queryBuilderReadKind `shouldBe` QueryBuilderRows
                        trackedRead.queryBuilderPrimaryKeyColumns `shouldBe` ["id"]
                        trackedRead.queryBuilderResultPrimaryKeys
                            `shouldBe` Just [QueryBuilderPrimaryKey [Aeson.String "task-1"]]
                    _ -> expectationFailure "Expected a structured QueryBuilder dependency"

            it "keeps whole-table reads absorbing regardless of read order" $ \_ -> do
                let structuredThenWhole =
                        addWholeTableReadDependency "tasks" (addQueryBuilderReadDependency "tasks" queryRead Map.empty)
                let wholeThenStructured =
                        addQueryBuilderReadDependency "tasks" queryRead (addWholeTableReadDependency "tasks" Map.empty)
                forM_ [structuredThenWhole, wholeThenStructured] \dependencies ->
                    Map.lookup "tasks" dependencies
                        `shouldSatisfy` \case
                            Just AutoRefreshWholeTable -> True
                            _ -> False

            it "keeps the existing withTableReadTracker API and manual reads conservative" $ withContext do
                withTableReadTracker do
                    trackTableRead "users"
                    readIORef ?touchedTables `shouldReturn` Set.singleton "users"

                withAutoRefreshReadTracker do
                    trackTableRead "users"
                    readIORef ?touchedTables `shouldReturn` Set.singleton "users"
                    dependencies <- readIORef ?autoRefreshReadDependencies
                    Map.lookup "users" dependencies `shouldSatisfy` \case
                        Just AutoRefreshWholeTable -> True
                        _ -> False

            it "falls back to constant-size tracking after too many queries" $ \_ -> do
                let dependencies = foldl' (\current _ -> addQueryBuilderReadDependency "tasks" queryRead current) Map.empty [1..65 :: Int]
                Map.lookup "tasks" dependencies `shouldSatisfy` \case
                    Just AutoRefreshWholeTable -> True
                    _ -> False

            it "captures canonical composite keys through a real fetch" $ \_ ->
                withAutoRefreshDB \modelContext -> do
                    let ?modelContext = modelContext
                    withAutoRefreshReadTracker do
                        items <- query @AutoRefreshCompositeItem |> fetch
                        items `shouldBe` [AutoRefreshCompositeItem
                            { tenantId = "a0000000-0000-0000-0000-000000000001"
                            , itemId = 7
                            , name = "tracked"
                            }]

                        dependencies <- readIORef ?autoRefreshReadDependencies
                        case Map.lookup "auto_refresh_composite_items" dependencies of
                            Just (AutoRefreshQueryBuilderReads [trackedRead]) -> do
                                trackedRead.queryBuilderReadKind `shouldBe` QueryBuilderRows
                                trackedRead.queryBuilderPrimaryKeyColumns `shouldBe` ["tenant_id", "item_id"]
                                trackedRead.queryBuilderResultPrimaryKeys `shouldBe` Just
                                    [ QueryBuilderPrimaryKey
                                        [ Aeson.String "a0000000-0000-0000-0000-000000000001"
                                        , Aeson.Number 7
                                        ]
                                    ]
                            _ -> expectationFailure "Expected one tracked composite-key fetch"

        describe "autoRefreshMeta" do
            it "renders the ihp-auto-refresh-id meta tag on the initial response" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    response <- callActionWithQueryParams pgListener ShowItemHtmlAction [("marketId", "abc-123")]
                    let bodyBs = LBS.toStrict (simpleBody response)
                    BS.isInfixOf "ihp-auto-refresh-id" bodyBs `shouldBe` True

                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

        describe "renderView" do
            it "should preserve query parameters when re-rendering with a websocket request" $ withContext do
                -- Clean up any leftover global state from previous tests
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    -- 1. Call the action with query params — this triggers autoRefresh
                    --    which stores a session with renderView
                    response <- callActionWithQueryParams pgListener ShowItemAction [("marketId", "abc-123")]
                    cs (simpleBody response) `shouldBe` ("abc-123" :: Text)

                    -- 2. Extract the stored renderView from the AutoRefreshSession
                    maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                    serverRef <- case maybeServerRef of
                        Just ref -> pure ref
                        Nothing -> error "AutoRefreshServer was not created"

                    server <- readIORef serverRef
                    session <- case server.sessions of
                        (s:_) -> pure s
                        [] -> error "No AutoRefresh sessions found"

                    -- 3. Call renderView with a bare request (simulating WebSocket re-render)
                    --    The WebSocket request has NO query params — this is the bug scenario
                    reResponse <- runSession (request defaultRequest) session.renderView
                    -- If query params are NOT preserved, this would throw ParamNotFoundException
                    cs (simpleBody reResponse) `shouldBe` ("abc-123" :: Text)

                    -- Cleanup
                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

        describe "graceful degradation without PGListener" do
            it "should run the action without crashing when PGListener is not available" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                response <- callActionWithParams ShowItemAction [("marketId", "degraded-ok")]
                body <- responseBody response
                cs body `shouldBe` ("degraded-ok" :: Text)

                -- Verify autoRefresh skipped subscription machinery entirely
                maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                case maybeServerRef of
                    Nothing -> pure ()
                    Just _ -> expectationFailure "Expected globalAutoRefreshServerVar to be Nothing"

        describe "session state tracking" do
            it "should compare re-rendered html against the latest session response" $ withContext do
                event <- MVar.newEmptyMVar
                now <- getCurrentTime
                let session =
                        AutoRefreshSession
                            { id = UUID.nil
                            , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                            , event
                            , tables = mempty
                            , lastResponse = "resolved"
                            , lastPing = now
                            }
                serverRef <-
                    newIORef
                        AutoRefreshServer
                            { subscriptions = []
                            , sessions = [session]
                            , subscribedTables = mempty
                            , pgListener = error "pgListener unused in session state test"
                            }

                updateSession serverRef UUID.nil (\currentSession -> currentSession { lastResponse = "unresolved" })

                sessionResponseHasChanged serverRef UUID.nil "resolved" `shouldReturn` True
                sessionResponseHasChanged serverRef UUID.nil "unresolved" `shouldReturn` False

            it "publishes rerender dependencies only after registration succeeds" $ withContext do
                event <- MVar.newEmptyMVar
                now <- getCurrentTime
                let oldDependencies = Map.singleton "old_table" AutoRefreshWholeTable
                let newDependencies = Map.singleton "new_table" AutoRefreshWholeTable
                let session = TrackedAutoRefreshSession
                        { id = UUID.nil
                        , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                        , event
                        , tables = Map.keysSet oldDependencies
                        , readDependencies = oldDependencies
                        , lastResponse = "old"
                        , lastPing = now
                        }
                serverRef <- newIORef AutoRefreshServer
                    { subscriptions = []
                    , sessions = [session]
                    , subscribedTables = mempty
                    , pgListener = error "pgListener unused in dependency commit test"
                    }

                failed <- Exception.try @SomeException $
                    commitAutoRefreshReadDependencies
                        (Exception.throwIO (userError "subscription failed"))
                        serverRef
                        UUID.nil
                        newDependencies
                failed `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False
                getAutoRefreshReadDependencies <$> getOnlyAutoRefreshSession serverRef
                    `shouldReturn` oldDependencies

                commitAutoRefreshReadDependencies (pure ()) serverRef UUID.nil newDependencies
                getAutoRefreshReadDependencies <$> getOnlyAutoRefreshSession serverRef
                    `shouldReturn` newDependencies
