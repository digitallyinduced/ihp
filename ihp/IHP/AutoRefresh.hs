{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport hiding (request)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Dynamic (fromDynamic)
import IHP.ModelSupport
import IHP.QueryBuilder.Types (Condition (..), ConditionValue (..), QueryBuilderRead (..), QueryBuilderReadKind (..), QueryBuilderPrimaryKey (..), SQLQuery (..))
import IHP.QueryBuilder.HasqlCompiler (buildQueryMatchesRowsStatement)
import qualified Data.Aeson as Aeson
import qualified IHP.ModelSupport.TableReadTracker as TableReadTracker
import qualified Control.Exception as Exception
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (void)
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.WebSocket
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Session as HasqlSession
import System.Log.FastLogger (toLogStr)
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import IHP.RequestVault (pgListenerVaultKey)
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Environment (Environment(..))

{-# NOINLINE globalAutoRefreshServerVar #-}
globalAutoRefreshServerVar :: MVar.MVar (Maybe (IORef AutoRefreshServer))
globalAutoRefreshServerVar = unsafePerformIO (MVar.newMVar Nothing)

{-# NOINLINE notificationTriggerRegistrationLock #-}
notificationTriggerRegistrationLock :: MVar.MVar ()
notificationTriggerRegistrationLock = unsafePerformIO (MVar.newMVar ())

getOrCreateAutoRefreshServer :: (?request :: Request) => IO (IORef AutoRefreshServer)
getOrCreateAutoRefreshServer =
    MVar.modifyMVar globalAutoRefreshServerVar $ \case
        Just server -> pure (Just server, server)
        Nothing -> do
            let pgListener = case Vault.lookup pgListenerVaultKey ?request.vault of
                    Just pl -> pl
                    Nothing -> error "getOrCreateAutoRefreshServer: PGListener not found in request vault"
            server <- newIORef (newAutoRefreshServer pgListener)
            pure (Just server, server)

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?respond :: Respond
    ) => ((?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => IO ResponseReceived) -> IO ResponseReceived
autoRefresh runAction = do
    -- When PGListener is not available, degrade gracefully to a
    -- plain action without auto-refresh.
    case Vault.lookup pgListenerVaultKey ?request.vault of
        Nothing -> runAction
        Just _ -> do
            let autoRefreshState = Vault.lookup autoRefreshStateVaultKey ?request.vault
            autoRefreshServer <- getOrCreateAutoRefreshServer

            case autoRefreshState of
                Just (AutoRefreshEnabled {}) -> do
                    -- When this function calls the 'action ?theAction' in the other case
                    -- we will evaluate this branch
                    runAction
                _ -> do
                    availableSessions <- getAvailableSessions autoRefreshServer

                    id <- UUID.nextRandom

                    -- Update the vault with AutoRefreshEnabled so that autoRefreshMeta can read it
                    let newRequest = ?request { vault = Vault.insert autoRefreshStateVaultKey (AutoRefreshEnabled id) ?request.vault }
                    let ?request = newRequest

                    -- Capture the current request and context for re-rendering. The
                    -- request vault carries all per-request state (current user, flash
                    -- messages, framework config, ...) so passing the closure-captured
                    -- values back into the renderView callback is enough.
                    let originalRequest = ?request
                    let renderView = \waiRequest waiRespond -> do
                            withAutoRefreshReadTracker do
                                result <- earlyReturnMiddleware (\_ respond -> do
                                    let ?request = originalRequest
                                    let ?context = ?request
                                    let ?respond = respond
                                    action ?theAction
                                    ) waiRequest waiRespond
                                readDependencies <- readIORef ?autoRefreshReadDependencies
                                commitAutoRefreshReadDependencies
                                    (registerNewNotificationTriggers ?touchedTables autoRefreshServer)
                                    autoRefreshServer
                                    id
                                    readDependencies
                                pure result

                    -- We save the allowed session ids to the session cookie to only grant a client access
                    -- to sessions it initially opened itself
                    --
                    -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
                    setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

                    withAutoRefreshReadTracker do
                        (result, capturedResponse) <- captureResponseBody ?respond \respond -> do
                            let ?respond = respond
                            runAction

                        -- After the action completes, set up the auto refresh session
                        readDependencies <- readIORef ?autoRefreshReadDependencies
                        let tables = Map.keysSet readDependencies
                        lastPing <- getCurrentTime
                        case capturedResponse of
                            Just lastResponse -> do
                                event <- MVar.newEmptyMVar
                                let session = TrackedAutoRefreshSession { id, renderView, event, tables, readDependencies, lastResponse, lastPing }
                                registerNotificationTrigger ?touchedTables autoRefreshServer
                                atomicModifyIORef' autoRefreshServer (\s -> (s { sessions = session:s.sessions }, ()))
                                _ <- async (gcSessions autoRefreshServer)
                                pure ()
                            Nothing -> pure () -- Response wasn't a builder type, can't do auto refresh

                        pure result

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        let ?context = ?request
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        autoRefreshServer <- getOrCreateAutoRefreshServer
        availableSessions <- getAvailableSessions autoRefreshServer

        when (sessionId `elem` availableSessions) do
            session <- getSessionById autoRefreshServer sessionId
            let renderView = session.renderView
            let event = session.event

            let handleOtherException :: SomeException -> IO ()
                handleOtherException ex = ?context.frameworkConfig.logger (toLogStr ("AutoRefresh: Failed to re-render view: " <> tshow ex))

            async $ forever do
                MVar.takeMVar event
                let currentRequest = ?request
                (do
                    (_, capturedResponse) <- captureResponseBody (\_ -> pure (error "AutoRefresh: ResponseReceived placeholder")) \respond ->
                        renderView currentRequest respond
                    case capturedResponse of
                        Just html -> do
                            responseChanged <- sessionResponseHasChanged autoRefreshServer sessionId html
                            when responseChanged do
                                sendTextData html
                                updateSession autoRefreshServer sessionId (\session -> session { lastResponse = html })
                        Nothing -> pure ()
                    ) `catch` handleOtherException
                pure ()

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        autoRefreshServer <- getOrCreateAutoRefreshServer
        updateSession autoRefreshServer sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                autoRefreshServer <- getOrCreateAutoRefreshServer
                atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = filter (\session -> session.id /= sessionId) server.sessions }, ()))
            AwaitingSessionID -> pure ()


-- | Runs an action while capturing the response body.
-- Returns the action's result and the captured body (if it was a ResponseBuilder).
-- Only captures ResponseBuilder responses (used by HSX/Blaze rendering).
captureResponseBody :: Respond -> (Respond -> IO a) -> IO (a, Maybe LByteString)
captureResponseBody originalRespond action = do
    bodyRef <- newIORef Nothing
    let capturingRespond response = do
            case response of
                Wai.ResponseBuilder _status _headers builder -> do
                    let body = ByteString.toLazyByteString builder
                    evaluatedBody <- Exception.evaluate body
                    writeIORef bodyRef (Just evaluatedBody)
                _ -> pure ()
            originalRespond response
    result <- action capturingRespond
    captured <- readIORef bodyRef
    pure (result, captured)

-- | Maximum structured reads retained for a table in one rendered response.
-- Above this limit AutoRefresh falls back to a constant-size whole-table
-- dependency instead of retaining an unbounded number of encoder closures.
maxAutoRefreshQueryBuilderReadsPerTable :: Int
maxAutoRefreshQueryBuilderReadsPerTable = 64

-- | Maximum returned keys retained for one row query.
maxAutoRefreshPrimaryKeysPerRead :: Int
maxAutoRefreshPrimaryKeysPerRead = 1024

-- | Convert the public millisecond configuration to the unit expected by
-- 'threadDelay'. Zero deliberately stays zero: it means that AutoRefresh adds
-- no batching delay to its real-time notification path.
autoRefreshBatchWindowMicroseconds :: Int -> Int
autoRefreshBatchWindowMicroseconds milliseconds = milliseconds * 1000

-- | Bound the amount of row identity state retained during a burst. Overflow
-- degrades to one conservative whole-table change instead of growing memory.
maxAutoRefreshChangedRowsPerBatch :: Int
maxAutoRefreshChangedRowsPerBatch = 4096

data AutoRefreshChangeBatch = AutoRefreshChangeBatch
    { insertedRowIds :: !(Set Text)
    , updatedRowIds :: !(Set Text)
    , deletedRowIds :: !(Set Text)
    , hasUnknownChange :: !Bool
    } deriving (Eq, Show)

data AutoRefreshBatchWorkerState = AutoRefreshBatchWorkerState
    { workerScheduled :: !Bool
    , pendingChanges :: !AutoRefreshChangeBatch
    }

emptyAutoRefreshChangeBatch :: AutoRefreshChangeBatch
emptyAutoRefreshChangeBatch = AutoRefreshChangeBatch
    { insertedRowIds = Set.empty
    , updatedRowIds = Set.empty
    , deletedRowIds = Set.empty
    , hasUnknownChange = False
    }

emptyAutoRefreshBatchWorkerState :: AutoRefreshBatchWorkerState
emptyAutoRefreshBatchWorkerState = AutoRefreshBatchWorkerState
    { workerScheduled = False
    , pendingChanges = emptyAutoRefreshChangeBatch
    }

autoRefreshChangeBatchIsEmpty :: AutoRefreshChangeBatch -> Bool
autoRefreshChangeBatchIsEmpty batch =
    not batch.hasUnknownChange
    && Set.null batch.insertedRowIds
    && Set.null batch.updatedRowIds
    && Set.null batch.deletedRowIds

addAutoRefreshRowChange :: AutoRefreshRowChange -> AutoRefreshChangeBatch -> AutoRefreshChangeBatch
addAutoRefreshRowChange _ batch | batch.hasUnknownChange = batch
addAutoRefreshRowChange AutoRefreshRowChange { operation, rowId = Just changedId } batch =
    boundBatch case operation of
        "insert" -> batch { insertedRowIds = Set.insert changedId batch.insertedRowIds }
        "update" -> batch { updatedRowIds = Set.insert changedId batch.updatedRowIds }
        "delete" -> batch { deletedRowIds = Set.insert changedId batch.deletedRowIds }
        _ -> conservativeBatch
  where
    boundBatch candidate
        | Set.size (candidate.insertedRowIds <> candidate.updatedRowIds <> candidate.deletedRowIds) > maxAutoRefreshChangedRowsPerBatch = conservativeBatch
        | otherwise = candidate
addAutoRefreshRowChange AutoRefreshRowChange { rowId = Nothing } _ = conservativeBatch

conservativeBatch :: AutoRefreshChangeBatch
conservativeBatch = emptyAutoRefreshChangeBatch { hasUnknownChange = True }

-- | Track reads for AutoRefresh without changing the public ModelContext or
-- 'withTableReadTracker' APIs. Dependencies are aggregated and bounded while
-- the action runs, rather than collecting an unbounded intermediate list.
withAutoRefreshReadTracker :: (?modelContext :: ModelContext)
    => (( ?modelContext :: ModelContext
        , ?touchedTables :: IORef (Set Text)
        , ?autoRefreshReadDependencies :: IORef (Map.Map Text AutoRefreshReadDependency)
        ) => IO a)
    -> IO a
withAutoRefreshReadTracker trackedSection = do
    touchedTablesVar <- newIORef Set.empty
    readDependenciesVar <- newIORef Map.empty
    let trackTableReadCallback tableName =
            atomicModifyIORef' touchedTablesVar (\tables -> (Set.insert tableName tables, ()))
    let collectRead trackedRead =
            atomicModifyIORef' readDependenciesVar (\dependencies -> (addTrackedRead trackedRead dependencies, ()))
    let oldModelContext = ?modelContext
    let ?modelContext = oldModelContext { trackTableReadCallback = Just trackTableReadCallback }
    let ?touchedTables = touchedTablesVar
    let ?autoRefreshReadDependencies = readDependenciesVar
    TableReadTracker.withTrackedTableReadCallback trackTableReadCallback collectRead trackedSection

addTrackedRead :: TableReadTracker.TrackedTableRead -> Map.Map Text AutoRefreshReadDependency -> Map.Map Text AutoRefreshReadDependency
addTrackedRead trackedRead dependencies = case trackedRead of
    TableReadTracker.TrackedWholeTableRead tableName ->
        addWholeTableReadDependency tableName dependencies
    TableReadTracker.TrackedStructuredTableRead tableName dynamicRead ->
        case fromDynamic @QueryBuilderRead dynamicRead of
            Just queryBuilderRead -> addQueryBuilderReadDependency tableName queryBuilderRead dependencies
            Nothing -> case fromDynamic @TrackedQueryRead dynamicRead of
                Just queryRead -> addTrackedQueryReadDependency tableName queryRead dependencies
                Nothing -> addWholeTableReadDependency tableName dependencies

-- | A manual or unknown read is absorbing for the table.
addWholeTableReadDependency :: Text -> Map.Map Text AutoRefreshReadDependency -> Map.Map Text AutoRefreshReadDependency
addWholeTableReadDependency tableName =
    Map.insert tableName AutoRefreshWholeTable

-- | Add a bounded structured read. Row reads without canonical returned keys,
-- oversized result sets, and tables exceeding the read limit safely degrade to
-- a whole-table dependency and release all retained queries for that table.
addQueryBuilderReadDependency :: Text -> QueryBuilderRead -> Map.Map Text AutoRefreshReadDependency -> Map.Map Text AutoRefreshReadDependency
addQueryBuilderReadDependency tableName queryBuilderRead =
    Map.alter addRead tableName
    where
        addRead _ | cannotRetainRead = Just AutoRefreshWholeTable
        addRead Nothing = Just (AutoRefreshQueryBuilderReads [queryBuilderRead])
        addRead (Just AutoRefreshWholeTable) = Just AutoRefreshWholeTable
        addRead (Just (AutoRefreshTrackedQueryReads _)) = Just AutoRefreshWholeTable
        addRead (Just (AutoRefreshQueryBuilderReads queryReads))
            | length queryReads >= maxAutoRefreshQueryBuilderReadsPerTable = Just AutoRefreshWholeTable
            | otherwise = Just (AutoRefreshQueryBuilderReads (queryBuilderRead:queryReads))

        cannotRetainRead = case queryBuilderRead.queryBuilderReadKind of
            QueryBuilderRows ->
                isJust queryBuilderRead.trackedSqlQuery.offsetClause
                || null queryBuilderRead.queryBuilderPrimaryKeyColumns
                || case queryBuilderRead.queryBuilderResultPrimaryKeys of
                    Nothing -> True
                    Just primaryKeys -> length primaryKeys > maxAutoRefreshPrimaryKeysPerRead
            QueryBuilderCount -> False
            QueryBuilderExists -> False

-- | Add a bounded executable read, currently produced by Typed SQL. Mixing
-- different structured producers for one table falls back to the conservative
-- whole-table dependency until they share a single relevance representation.
addTrackedQueryReadDependency :: Text -> TrackedQueryRead -> Map.Map Text AutoRefreshReadDependency -> Map.Map Text AutoRefreshReadDependency
addTrackedQueryReadDependency tableName queryRead =
    Map.alter addRead tableName
    where
        addRead _ | cannotRetainRead = Just AutoRefreshWholeTable
        addRead Nothing = Just (AutoRefreshTrackedQueryReads [queryRead])
        addRead (Just AutoRefreshWholeTable) = Just AutoRefreshWholeTable
        addRead (Just (AutoRefreshQueryBuilderReads _)) = Just AutoRefreshWholeTable
        addRead (Just (AutoRefreshTrackedQueryReads queryReads))
            | length queryReads >= maxAutoRefreshQueryBuilderReadsPerTable = Just AutoRefreshWholeTable
            | otherwise = Just (AutoRefreshTrackedQueryReads (queryRead:queryReads))

        cannotRetainRead =
            isNothing queryRead.trackedQueryMatchesIds
            || case queryRead.trackedRowIds of
                Nothing -> True
                Just rowIds -> Set.size rowIds > maxAutoRefreshPrimaryKeysPerRead

-- | Register every new table first and only then publish the new dependency
-- snapshot. If registration throws, the session continues to describe the HTML
-- that is still displayed by the client.
commitAutoRefreshReadDependencies
    :: IO ()
    -> IORef AutoRefreshServer
    -> UUID
    -> Map.Map Text AutoRefreshReadDependency
    -> IO ()
commitAutoRefreshReadDependencies registerNewTables autoRefreshServer sessionId readDependencies = do
    registerNewTables
    updateSession autoRefreshServer sessionId (setAutoRefreshReadDependencies readDependencies)

-- | Register tables touched by a rerender. Production registration skips
-- already subscribed tables; development registration deliberately reinstalls
-- their idempotent trigger SQL because @make db@ may have recreated the DB.
registerNewNotificationTriggers :: (?modelContext :: ModelContext, ?request :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNewNotificationTriggers = registerNotificationTrigger

registerNotificationTrigger :: (?modelContext :: ModelContext, ?request :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer =
    MVar.withMVar notificationTriggerRegistrationLock \_ -> do
        touchedTables <- Set.toList <$> readIORef touchedTablesVar
        subscribedTables <- (.subscribedTables) <$> readIORef autoRefreshServer

        let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)

        -- In development, always re-run trigger SQL for all touched tables because
        -- `make db` drops and recreates the database, destroying triggers that were
        -- previously installed. The trigger SQL is idempotent so re-running is safe.
        -- In production, only install triggers for newly seen tables.
        let isDevelopment = ?request.frameworkConfig.environment == Development

        pgListener <- (.pgListener) <$> readIORef autoRefreshServer
        let matchingModelContext = ?modelContext
                { transactionRunner = Nothing
                , trackTableReadCallback = Nothing
                , rowLevelSecurity = Nothing
                }
        let batchWindowMicroseconds =
                autoRefreshBatchWindowMicroseconds ?request.frameworkConfig.autoRefreshBatchWindow
        forM_ subscriptionRequired \table -> do
            -- We need to add the trigger from the main IHP database role other we will get this error:
            -- ERROR:  permission denied for schema public
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

            batchWorkerState <- MVar.newMVar emptyAutoRefreshBatchWorkerState
            let processBatch batch =
                    processAutoRefreshChangeBatch matchingModelContext autoRefreshServer table batch
                        `Exception.catch` \(exception :: SomeException) ->
                            matchingModelContext.logger (toLogStr ("AutoRefresh: Failed to process notification batch: " <> tshow exception))
            subscription <- pgListener |> PGListener.subscribeJSON (channelName table) \change -> do
                    startWorker <- enqueueAutoRefreshRowChange batchWorkerState change
                    when startWorker do
                        void (async (runAutoRefreshBatchWorker batchWindowMicroseconds batchWorkerState processBatch))
                    pure ()

            atomicModifyIORef' autoRefreshServer \server ->
                ( server
                    { subscriptions = server.subscriptions <> [subscription]
                    , subscribedTables = Set.insert table server.subscribedTables
                    }
                , ()
                )

        -- Re-run trigger SQL for already-subscribed tables in dev mode
        when isDevelopment do
            let alreadySubscribed = touchedTables |> filter (\table -> subscribedTables |> Set.member table)
            forM_ alreadySubscribed \table -> do
                withRowLevelSecurityDisabled do
                    let pool = ?modelContext.hasqlPool
                    runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

enqueueAutoRefreshRowChange :: MVar.MVar AutoRefreshBatchWorkerState -> AutoRefreshRowChange -> IO Bool
enqueueAutoRefreshRowChange batchWorkerState change =
    MVar.modifyMVar batchWorkerState \state -> do
        let startWorker = not state.workerScheduled
        let nextState = state
                { workerScheduled = True
                , pendingChanges = addAutoRefreshRowChange change state.pendingChanges
                }
        pure (nextState, startWorker)

runAutoRefreshBatchWorker
    :: Int
    -> MVar.MVar AutoRefreshBatchWorkerState
    -> (AutoRefreshChangeBatch -> IO ())
    -> IO ()
runAutoRefreshBatchWorker batchWindowMicroseconds batchWorkerState processBatch = do
    when (batchWindowMicroseconds > 0) (threadDelay batchWindowMicroseconds)
    batch <- MVar.modifyMVar batchWorkerState \state ->
        pure (state { pendingChanges = emptyAutoRefreshChangeBatch }, state.pendingChanges)
    unless (autoRefreshChangeBatchIsEmpty batch) (processBatch batch)
    continue <- MVar.modifyMVar batchWorkerState \state ->
        if autoRefreshChangeBatchIsEmpty state.pendingChanges
            then pure (state { workerScheduled = False }, False)
            else pure (state, True)
    when continue (runAutoRefreshBatchWorker batchWindowMicroseconds batchWorkerState processBatch)

processAutoRefreshChangeBatch
    :: ModelContext
    -> IORef AutoRefreshServer
    -> Text
    -> AutoRefreshChangeBatch
    -> IO ()
processAutoRefreshChangeBatch modelContext autoRefreshServer table batch = do
    matcherCache <- newIORef Map.empty
    sessions <- (.sessions) <$> readIORef autoRefreshServer
    forM_ sessions \session ->
        case Map.lookup table (getAutoRefreshReadDependencies session) of
            Nothing -> pure ()
            Just dependency -> do
                shouldRefresh <- shouldRefreshForChangeBatchWithCache modelContext matcherCache dependency batch
                when shouldRefresh (void (MVar.tryPutMVar session.event ()))

-- | Backwards-compatible single-notification entry point. The live listener
-- calls 'shouldRefreshForChangeBatchWithCache' after coalescing notifications.
shouldRefreshForChange :: ModelContext -> AutoRefreshReadDependency -> AutoRefreshRowChange -> IO Bool
shouldRefreshForChange modelContext dependency change = do
    matcherCache <- newIORef Map.empty
    shouldRefreshForChangeBatchWithCache
        modelContext
        matcherCache
        dependency
        (addAutoRefreshRowChange change emptyAutoRefreshChangeBatch)

shouldRefreshForChangeBatch :: ModelContext -> AutoRefreshReadDependency -> AutoRefreshChangeBatch -> IO Bool
shouldRefreshForChangeBatch modelContext dependency batch = do
    matcherCache <- newIORef Map.empty
    shouldRefreshForChangeBatchWithCache modelContext matcherCache dependency batch

shouldRefreshForChangeBatchWithCache
    :: ModelContext
    -> IORef (Map.Map Text Bool)
    -> AutoRefreshReadDependency
    -> AutoRefreshChangeBatch
    -> IO Bool
shouldRefreshForChangeBatchWithCache _ _ _ batch | autoRefreshChangeBatchIsEmpty batch = pure False
shouldRefreshForChangeBatchWithCache _ _ AutoRefreshWholeTable _ = pure True
shouldRefreshForChangeBatchWithCache _ _ _ AutoRefreshChangeBatch { hasUnknownChange = True } = pure True
shouldRefreshForChangeBatchWithCache modelContext matcherCache (AutoRefreshQueryBuilderReads reads) batch =
    if null reads
        then pure True
        else queryBuilderReadsAffectBatch modelContext matcherCache reads batch
shouldRefreshForChangeBatchWithCache _ _ (AutoRefreshTrackedQueryReads reads) batch =
    if null reads
        then pure True
        else trackedReadsAffectBatch reads batch

-- Check every read's already-visible IDs before running any retained matcher.
-- A page commonly performs several reads of the same table. If a later read
-- already contains an updated/deleted row, the refresh decision is known and
-- no earlier read should issue an avoidable SELECT EXISTS query.
queryBuilderReadsAffectBatch
    :: ModelContext
    -> IORef (Map.Map Text Bool)
    -> [QueryBuilderRead]
    -> AutoRefreshChangeBatch
    -> IO Bool
queryBuilderReadsAffectBatch modelContext matcherCache reads batch
    | any isNothing visibleRowIds = pure True
    | any directlyAffected (catMaybes visibleRowIds) = pure True
    | Set.null rowsToMatch = pure False
    | otherwise = anyM (queryBuilderQueryAffects modelContext matcherCache rowsToMatch) reads
  where
    visibleRowIds = map queryBuilderReadRowIds reads
    directlyAffectingIds = batch.updatedRowIds <> batch.deletedRowIds
    rowsToMatch = batch.insertedRowIds <> batch.updatedRowIds
    directlyAffected ids = not (Set.null (ids `Set.intersection` directlyAffectingIds))

-- Only the conventional scalar @id@ can be matched against the compact trigger
-- payload. Marc's retained custom/composite keys remain available in the
-- dependency snapshot, but currently take the conservative refresh path.
queryBuilderReadRowIds :: QueryBuilderRead -> Maybe (Set Text)
queryBuilderReadRowIds QueryBuilderRead
        { queryBuilderReadKind = QueryBuilderRows
        , queryBuilderPrimaryKeyColumns = ["id"]
        , queryBuilderResultPrimaryKeys = Just primaryKeys
        } = Set.fromList <$> mapM primaryKeyText primaryKeys
queryBuilderReadRowIds _ = Nothing

primaryKeyText :: QueryBuilderPrimaryKey -> Maybe Text
primaryKeyText (QueryBuilderPrimaryKey [value]) = case value of
    Aeson.String text -> Just text
    Aeson.Number _ -> Just (cs (Aeson.encode value))
    Aeson.Bool True -> Just "true"
    Aeson.Bool False -> Just "false"
    _ -> Nothing
primaryKeyText _ = Nothing

queryBuilderQueryAffects
    :: ModelContext
    -> IORef (Map.Map Text Bool)
    -> Set Text
    -> QueryBuilderRead
    -> IO Bool
queryBuilderQueryAffects modelContext matcherCache changedIds queryRead = do
    let runMatcher = do
            let statement = buildQueryMatchesRowsStatement queryRead.trackedSqlQuery
            let ?modelContext = modelContext
            result <- Exception.try @SomeException $
                sqlStatementHasql modelContext.hasqlPool (Set.toList changedIds) statement
            pure (either (const True) (\value -> value) result)
    case queryBuilderMatcherCacheKey changedIds queryRead of
        Nothing -> runMatcher
        Just cacheKey -> do
            cached <- Map.lookup cacheKey <$> readIORef matcherCache
            case cached of
                Just result -> pure result
                Nothing -> do
                    result <- runMatcher
                    atomicModifyIORef' matcherCache (\cache -> (Map.insert cacheKey result cache, ()))
                    pure result

-- Parameter encoders are intentionally opaque. We only share matcher results
-- when the retained query has no condition parameters, so equality is exact.
-- LIMIT values remain part of the shown SQLQuery and therefore of the key.
queryBuilderMatcherCacheKey :: Set Text -> QueryBuilderRead -> Maybe Text
queryBuilderMatcherCacheKey changedIds QueryBuilderRead { trackedSqlQuery }
    | maybe False conditionHasParameter trackedSqlQuery.whereCondition = Nothing
    | otherwise = Just
        ( tshow trackedSqlQuery
        <> "\NUL"
        <> Text.intercalate "\NUL" (Set.toAscList changedIds)
        )

conditionHasParameter :: Condition -> Bool
conditionHasParameter (ColumnCondition _ _ (Param _) _ _) = True
conditionHasParameter (ColumnCondition _ _ (Literal _) _ _) = False
conditionHasParameter (OrCondition left right) = conditionHasParameter left || conditionHasParameter right
conditionHasParameter (AndCondition left right) = conditionHasParameter left || conditionHasParameter right

trackedReadsAffectBatch :: [TrackedQueryRead] -> AutoRefreshChangeBatch -> IO Bool
trackedReadsAffectBatch reads batch
    | any visibleRowsAffected reads = pure True
    | Set.null rowsToMatch = pure False
    | otherwise = anyM (trackedQueryAffectsRead rowsToMatch) reads
  where
    directlyAffectingIds = batch.updatedRowIds <> batch.deletedRowIds
    rowsToMatch = batch.insertedRowIds <> batch.updatedRowIds
    visibleRowsAffected TrackedQueryRead { trackedRowIds = Nothing } = True
    visibleRowsAffected TrackedQueryRead { trackedRowIds = Just ids } =
        not (Set.null (ids `Set.intersection` directlyAffectingIds))

trackedQueryAffectsRead :: Set Text -> TrackedQueryRead -> IO Bool
trackedQueryAffectsRead _ TrackedQueryRead { trackedQueryMatchesIds = Nothing } = pure True
trackedQueryAffectsRead changedIds TrackedQueryRead { trackedQueryMatchesIds = Just matchesIds } = do
    result <- Exception.try @SomeException (matchesIds changedIds)
    pure (either (const True) (\value -> value) result)

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM predicate (value:values) = do
    matches <- predicate value
    if matches then pure True else anyM predicate values

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?request :: Request) => IORef AutoRefreshServer -> IO [UUID]
getAvailableSessions autoRefreshServer = do
    allSessions <- (.sessions) <$> readIORef autoRefreshServer
    text <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (.id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

-- | Returns a session for a given session id. Errors in case the session does not exist.
getSessionById :: IORef AutoRefreshServer -> UUID -> IO AutoRefreshSession
getSessionById autoRefreshServer sessionId = do
    autoRefreshServer <- readIORef autoRefreshServer
    autoRefreshServer.sessions
        |> find (\session -> session.id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

-- | Applies a update function to a session specified by its session id
updateSession :: IORef AutoRefreshServer -> UUID -> (AutoRefreshSession -> AutoRefreshSession) -> IO ()
updateSession server sessionId updateFunction = do
    let updateSession' session = if session.id == sessionId then updateFunction session else session
    atomicModifyIORef' server (\server -> (server { sessions = map updateSession' server.sessions }, ()))

-- | Returns 'True' when the rendered html differs from the session's latest
-- known response.
--
-- This must read the current session state instead of comparing against a
-- websocket-local snapshot, otherwise switching back to an earlier DOM state
-- can be incorrectly suppressed as "unchanged".
sessionResponseHasChanged :: IORef AutoRefreshServer -> UUID -> LByteString -> IO Bool
sessionResponseHasChanged autoRefreshServer sessionId html = do
    currentLastResponse <- (.lastResponse) <$> getSessionById autoRefreshServer sessionId
    pure (html /= currentLastResponse)

-- | Removes all expired sessions
--
-- This is useful to avoid dead sessions hanging around. This can happen when a websocket connection was never established
-- after the initial request. Then the onClose of the websocket app is never called and thus the session will not be
-- removed automatically.
gcSessions :: IORef AutoRefreshServer -> IO ()
gcSessions autoRefreshServer = do
    now <- getCurrentTime
    atomicModifyIORef' autoRefreshServer (\server -> (server { sessions = filter (not . isSessionExpired now) server.sessions }, ()))

-- | A session is expired if it was not pinged in the last 60 seconds
isSessionExpired :: UTCTime -> AutoRefreshSession -> Bool
isSessionExpired now session = (now `diffUTCTime` session.lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_" <> cs tableName

-- | Returns SQL that installs a compact row-level notification trigger.
--
-- Wrapped in a DO $$ block with EXCEPTION handler because concurrent requests
-- can race to CREATE OR REPLACE the same function, causing PostgreSQL to throw
-- 'tuple concurrently updated' (SQLSTATE XX000). This is safe to ignore: the
-- other connection's CREATE OR REPLACE will have succeeded.
notificationTriggerSQL :: Text -> Text
notificationTriggerSQL tableName =
        "DO $$\n"
        <> "BEGIN\n"
        <> "    CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "DECLARE\n"
            <> "    payload TEXT;\n"
            <> "BEGIN\n"
            <> "    IF (TG_OP = 'DELETE') THEN\n"
            <> "        payload := jsonb_build_object('op', lower(TG_OP), 'id', to_jsonb(OLD)->>'id')::text;\n"
            <> "        PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
            <> "        RETURN OLD;\n"
            <> "    ELSE\n"
            <> "        payload := jsonb_build_object('op', lower(TG_OP), 'id', to_jsonb(NEW)->>'id')::text;\n"
            <> "        PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
            <> "        RETURN NEW;\n"
            <> "    END IF;\n"
            <> "END;\n"
            <> "$BODY$ language plpgsql;\n"
        <> "    DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    DROP TRIGGER IF EXISTS " <> deleteTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null; -- 'tuple concurrently updated': another connection installed it first\n"
        <> "END; $$"
    where
        -- Reuse the existing names so installing the row-level variant also
        -- replaces the legacy statement-level triggers during an upgrade.
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
