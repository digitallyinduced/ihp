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
    ( AutoRefreshChangeBatch (..)
    , addAutoRefreshRowChange
    , addQueryBuilderReadDependency
    , addWholeTableReadDependency
    , autoRefreshBatchWindowMicroseconds
    , commitAutoRefreshReadDependencies
    , emptyAutoRefreshBatchWorkerState
    , emptyAutoRefreshChangeBatch
    , enqueueAutoRefreshRowChange
    , globalAutoRefreshServerVar
    , queryBuilderMatcherCacheKey
    , runAutoRefreshBatchWorker
    , sessionResponseHasChanged
    , shouldRefreshForChange
    , shouldRefreshForChangeBatch
    , updateSession
    , withAutoRefreshReadTracker
    )
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import IHP.QueryBuilder.Types (QueryBuilderRead (..), QueryBuilderReadKind (..), QueryBuilderPrimaryKey (..), SQLQuery (..))
import IHP.Hasql.FromRow (FromRowHasql (..), HasqlDecodeColumn (..))
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Async as Async
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

data AutoRefreshItem = AutoRefreshItem
    { autoRefreshItemId :: UUID
    , views :: Int
    }
    deriving (Eq, Show)

type instance GetTableName AutoRefreshItem = "auto_refresh_items"
type instance GetModelByTableName "auto_refresh_items" = AutoRefreshItem
type instance PrimaryKey "auto_refresh_items" = UUID

instance Table AutoRefreshItem where
    columnNames = ["id", "views"]
    primaryKeyColumnNames = ["id"]

instance FromRowHasql AutoRefreshItem where
    hasqlRowDecoder = AutoRefreshItem
        <$> hasqlColumnDecoder
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
            "DROP TABLE IF EXISTS auto_refresh_items;\
            \DROP TABLE IF EXISTS auto_refresh_composite_items;\
            \CREATE TABLE auto_refresh_composite_items (\
            \tenant_id UUID NOT NULL, item_id INT NOT NULL, name TEXT NOT NULL,\
            \PRIMARY KEY (tenant_id, item_id));\
            \INSERT INTO auto_refresh_composite_items (tenant_id, item_id, name) VALUES\
            \('a0000000-0000-0000-0000-000000000001', 7, 'tracked');\
            \CREATE TABLE auto_refresh_items (id UUID PRIMARY KEY, views INT NOT NULL);\
            \INSERT INTO auto_refresh_items (id, views) VALUES\
            \('b0000000-0000-0000-0000-000000000001', 5),\
            \('b0000000-0000-0000-0000-000000000002', 8);"
    let teardown = do
            _ <- HasqlPool.use pool (HasqlSession.script "DROP TABLE IF EXISTS auto_refresh_items; DROP TABLE IF EXISTS auto_refresh_composite_items;")
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

            it "shares only dependencies whose parameters can be compared exactly" $ \_ -> do
                let changedIds = Set.singleton "task-2"
                queryBuilderMatcherCacheKey changedIds queryRead `shouldSatisfy` isJust

                let parameterizedRead = queryRead
                        { trackedSqlQuery = buildQuery
                            (query @AutoRefreshItem
                                |> filterWhereGreaterThan (#views, 6 :: Int))
                        }
                queryBuilderMatcherCacheKey changedIds parameterizedRead `shouldBe` Nothing

            it "falls back to whole-table tracking for OFFSET queries" $ \_ -> do
                let offsetRead = queryRead
                        { trackedSqlQuery = sqlQuery { offsetClause = Just 1 }
                        }
                let dependencies = addQueryBuilderReadDependency "tasks" offsetRead Map.empty
                Map.lookup "tasks" dependencies `shouldBe` Just AutoRefreshWholeTable

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

            it "matches changed rows against the retained parameterized QueryBuilder query" $ \_ ->
                withAutoRefreshDB \modelContext -> do
                    let ?modelContext = modelContext
                    dependency <- withAutoRefreshReadTracker do
                        items <- query @AutoRefreshItem
                            |> filterWhereGreaterThan (#views, 6 :: Int)
                            |> fetch
                        items `shouldBe`
                            [ AutoRefreshItem
                                { autoRefreshItemId = "b0000000-0000-0000-0000-000000000002"
                                , views = 8
                                }
                            ]

                        dependencies <- readIORef ?autoRefreshReadDependencies
                        case Map.lookup "auto_refresh_items" dependencies of
                            Just dependency@(AutoRefreshQueryBuilderReads [_]) -> pure dependency
                            _ -> expectationFailure "Expected one tracked QueryBuilder fetch" >> pure AutoRefreshWholeTable

                    shouldRefreshForChange modelContext dependency AutoRefreshRowChange
                        { operation = "insert"
                        , rowId = Just "b0000000-0000-0000-0000-000000000002"
                        }
                        `shouldReturn` True
                    shouldRefreshForChange modelContext dependency AutoRefreshRowChange
                        { operation = "insert"
                        , rowId = Just "b0000000-0000-0000-0000-000000000001"
                        }
                        `shouldReturn` False

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

        describe "parameterized query filtering" do
            it "uses the captured matcher for inserts" $ withContext do
                let boundOwnerId = "owner-1" :: Text
                let read = TrackedQueryRead
                        { trackedRowIds = Just mempty
                        , trackedQueryMatchesIds = Just (\changedIds -> pure (boundOwnerId == "owner-1" && "matching-row" `Set.member` changedIds))
                        }
                let dependency = AutoRefreshTrackedQueryReads [read]

                shouldRefreshForChange ?modelContext dependency AutoRefreshRowChange { operation = "insert", rowId = Just "matching-row" }
                    `shouldReturn` True
                shouldRefreshForChange ?modelContext dependency AutoRefreshRowChange { operation = "insert", rowId = Just "other-row" }
                    `shouldReturn` False

            it "uses known IDs for deletes and updates" $ withContext do
                let read = TrackedQueryRead
                        { trackedRowIds = Just (Set.fromList ["visible-row"])
                        , trackedQueryMatchesIds = Just (\changedIds -> pure ("newly-visible-row" `Set.member` changedIds))
                        }
                let dependency = AutoRefreshTrackedQueryReads [read]

                shouldRefreshForChange ?modelContext dependency AutoRefreshRowChange { operation = "delete", rowId = Just "other-row" }
                    `shouldReturn` False
                shouldRefreshForChange ?modelContext dependency AutoRefreshRowChange { operation = "update", rowId = Just "visible-row" }
                    `shouldReturn` True
                shouldRefreshForChange ?modelContext dependency AutoRefreshRowChange { operation = "update", rowId = Just "newly-visible-row" }
                    `shouldReturn` True

            it "falls back to refreshing when tracking is incomplete or the matcher fails" $ withContext do
                let untrackedRead = TrackedQueryRead
                        { trackedRowIds = Nothing
                        , trackedQueryMatchesIds = Nothing
                        }
                let failingRead = TrackedQueryRead
                        { trackedRowIds = Just mempty
                        , trackedQueryMatchesIds = Just (\_ -> throwIO (userError "matcher failed"))
                        }

                shouldRefreshForChange ?modelContext (AutoRefreshTrackedQueryReads [untrackedRead]) AutoRefreshRowChange { operation = "update", rowId = Just "row" }
                    `shouldReturn` True
                shouldRefreshForChange ?modelContext (AutoRefreshTrackedQueryReads [failingRead]) AutoRefreshRowChange { operation = "insert", rowId = Just "row" }
                    `shouldReturn` True

        describe "notification batching" do
            it "has no intentional batching delay by default" $ \mockContext -> do
                mockContext.frameworkConfig.autoRefreshBatchWindow `shouldBe` 0
                autoRefreshBatchWindowMicroseconds mockContext.frameworkConfig.autoRefreshBatchWindow
                    `shouldBe` 0

            it "accepts an explicit batching window" $ \_ -> do
                frameworkConfig <- buildFrameworkConfig testLogger do
                    option Development
                    option (AutoRefreshBatchWindow 100)
                frameworkConfig.autoRefreshBatchWindow `shouldBe` 100
                autoRefreshBatchWindowMicroseconds frameworkConfig.autoRefreshBatchWindow
                    `shouldBe` 100000

            it "coalesces a burst before running relevance checks" $ \_ -> do
                workerState <- MVar.newMVar emptyAutoRefreshBatchWorkerState
                processedBatches <- newIORef []
                let changes =
                        [ AutoRefreshRowChange { operation = "insert", rowId = Just ("row-" <> tshow number) }
                        | number <- [1..10 :: Int]
                        ]

                starts <- forM changes (enqueueAutoRefreshRowChange workerState)
                starts `shouldBe` (True : replicate 9 False)

                worker <- Async.async $ runAutoRefreshBatchWorker 0 workerState \batch ->
                    atomicModifyIORef' processedBatches (\batches -> (batch:batches, ()))
                Async.wait worker

                batches <- readIORef processedBatches
                case batches of
                    [batch] -> do
                        batch.insertedRowIds `shouldBe` Set.fromList ["row-" <> tshow number | number <- [1..10 :: Int]]
                        batch.updatedRowIds `shouldBe` Set.empty
                        batch.deletedRowIds `shouldBe` Set.empty
                    _ -> expectationFailure ("Expected one batch, got " <> cs (tshow (length batches)))

            it "calls a retained Typed SQL matcher once for all changed IDs" $ withContext do
                receivedBatches <- newIORef []
                let read = TrackedQueryRead
                        { trackedRowIds = Just Set.empty
                        , trackedQueryMatchesIds = Just \changedIds -> do
                            atomicModifyIORef' receivedBatches (\batches -> (changedIds:batches, ()))
                            pure ("matching-row" `Set.member` changedIds)
                        }
                let batch = foldl'
                        (flip addAutoRefreshRowChange)
                        emptyAutoRefreshChangeBatch
                        [ AutoRefreshRowChange { operation = "insert", rowId = Just "other-row" }
                        , AutoRefreshRowChange { operation = "insert", rowId = Just "matching-row" }
                        ]

                shouldRefreshForChangeBatch ?modelContext (AutoRefreshTrackedQueryReads [read]) batch
                    `shouldReturn` True
                readIORef receivedBatches
                    `shouldReturn` [Set.fromList ["other-row", "matching-row"]]

            it "keeps a trailing batch when a change arrives during processing" $ \_ -> do
                workerState <- MVar.newMVar emptyAutoRefreshBatchWorkerState
                firstBatchStarted <- MVar.newEmptyMVar
                releaseFirstBatch <- MVar.newEmptyMVar
                processedBatches <- newIORef []
                let firstChange = AutoRefreshRowChange { operation = "insert", rowId = Just "first-row" }
                let trailingChange = AutoRefreshRowChange { operation = "insert", rowId = Just "trailing-row" }
                let processBatch batch = do
                        isFirst <- atomicModifyIORef' processedBatches \batches ->
                            (batch:batches, null batches)
                        when isFirst do
                            MVar.putMVar firstBatchStarted ()
                            MVar.takeMVar releaseFirstBatch

                enqueueAutoRefreshRowChange workerState firstChange `shouldReturn` True
                worker <- Async.async (runAutoRefreshBatchWorker 0 workerState processBatch)
                MVar.takeMVar firstBatchStarted

                enqueueAutoRefreshRowChange workerState trailingChange `shouldReturn` False
                MVar.putMVar releaseFirstBatch ()
                Async.wait worker

                batches <- reverse <$> readIORef processedBatches
                map (.insertedRowIds) batches
                    `shouldBe` [Set.singleton "first-row", Set.singleton "trailing-row"]
