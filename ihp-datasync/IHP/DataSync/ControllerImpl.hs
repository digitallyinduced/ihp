{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.ControllerImpl where

import IHP.ControllerPrelude hiding (OrderByClause, sqlQuery, sqlExec, sqlQueryScalar)
import qualified Control.Exception.Safe as Exception
import System.Log.FastLogger (toLogStr)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Scientific as Scientific

import Data.Aeson.TH
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Statement as Hasql
import qualified Hasql.Pool
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession, runSessionOnConnection, withDedicatedConnection)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (void)
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.TypedEncoder (makeCachedColumnTypeLookup, typedAesonValueToSnippet, lookupColumnType)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import qualified IHP.PGListener as PGListener
import qualified Data.Set as Set
import GHC.Conc (ThreadId, myThreadId, atomically)
import Control.Concurrent.QSemN
import Control.Concurrent.STM.TVar
import qualified Data.List as List

$(deriveFromJSON defaultOptions ''DataSyncMessage)
$(deriveToJSON defaultOptions { omitNothingFields = True } 'DataSyncResult)

-- | The transport envelope deliberately lives outside 'DataSyncMessage'. Older
-- Haskell callers can keep constructing @CreateDataSubscription query requestId@
-- while newer wire clients advertise snapshot support with an extra JSON field.
decodeDataSyncMessageEnvelope :: ByteString -> Either String (Maybe Int, DataSyncMessage)
decodeDataSyncMessageEnvelope input = do
    value <- Aeson.eitherDecodeStrict' input
    message <- case Aeson.fromJSON value of
        Aeson.Success decoded -> Right decoded
        Aeson.Error errorMessage -> Left errorMessage
    let protocolVersion = case value of
            Aeson.Object fields -> case Aeson.lookup "protocolVersion" fields of
                Just (Aeson.Number version) -> Scientific.toBoundedInteger version
                _ -> Nothing
            _ -> Nothing
    pure (protocolVersion, message)

type EnsureRLSEnabledFn = Text -> IO TableWithRLS
type InstallTableChangeTriggerFn = TableWithRLS -> IO ()
type SendJSONFn = DataSyncResponse -> IO ()
type HandleCustomMessageFn = (DataSyncResponse -> IO ()) -> DataSyncMessage -> IO ()

runDataSyncController ::
    ( HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?request :: Request
    , ?modelContext :: ModelContext
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> IO ByteString -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> IO ()
runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers receiveData sendJSON handleCustomMessage renamer = do
    let ?context = ?request
    setState DataSyncReady { subscriptions = HashMap.empty, transactions = HashMap.empty }

    columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
    handleMessage :: Maybe Int -> DataSyncMessage -> IO () <- buildMessageHandlerWithProtocolVersion hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup


    sem  <- newQSemN (maxSubscriptionsPerConnection * 2) -- needs to be larger than the subscriptions limit to trigger an error on overload. otherwise an overflow of connections might queue up silently

    -- Track Asyncs so we can cancel/wait on socket close
    childrenVar <- newTVarIO (HashMap.empty :: HashMap ThreadId (Async ()))

    let spawnWorker protocolVersion decodedMessage = Exception.mask \restoreParent -> do
            tidReady <- MVar.newEmptyMVar
            startGate <- MVar.newEmptyMVar
            a <- asyncWithUnmask \unmask -> do
                tid <- myThreadId
                MVar.putMVar tidReady tid
                -- The handler cannot finish and self-delete before the parent has
                -- inserted this Async into childrenVar.
                MVar.takeMVar startGate
                -- Take/release concurrency slot entirely inside the worker
                Exception.bracket_ (waitQSemN sem 1) (signalQSemN sem 1) do
                    Exception.finally
                        (unmask do
                            result <- Exception.try (handleMessage protocolVersion decodedMessage)
                            case result of
                                Left (e :: Exception.SomeException) -> do
                                    let requestId    = decodedMessage.requestId
                                    let errorMessage = cs (displayException e)
                                    ?modelContext.logger (toLogStr (tshow e))
                                    sendJSON DataSyncError { requestId, errorMessage }
                                Right _ -> pure ()
                        )
                        -- Self-deregister
                        (do
                            tid' <- myThreadId
                            atomically $ modifyTVar' childrenVar (HashMap.delete tid')
                        )
            let register = do
                    tid <- MVar.takeMVar tidReady
                    atomically $ modifyTVar' childrenVar (HashMap.insert tid a)
                    MVar.putMVar startGate ()
            register `Exception.onException` cancel a
            restoreParent (pure ())

    let loop = forever do
            msg <- decodeDataSyncMessageEnvelope <$> receiveData
            case msg of
                Right (protocolVersion, decoded) -> spawnWorker protocolVersion decoded
                Left err      -> sendJSON FailedToDecodeMessageError { errorMessage = cs err }

    -- On websocket close: cancel and drain all children
    loop `Exception.finally` do
        m <- readTVarIO childrenVar
        let handles = HashMap.elems m
        mapM_ cancel handles
        mapM_ (const (pure ())) =<< mapM waitCatch handles
{-# INLINE runDataSyncController #-}


buildMessageHandler ::
    ( HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?context :: Request
    , ?request :: Request
    , ?modelContext :: ModelContext
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    )
    => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (Text -> IO ColumnTypeInfo) -> IO (DataSyncMessage -> IO ())
buildMessageHandler hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup = do
    handleMessage <- buildMessageHandlerWithProtocolVersion hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup
    pure (handleMessage Nothing)

-- | Internal wire-aware variant. The optional protocol version is kept out of
-- the public 'DataSyncMessage' constructors for source compatibility.
buildMessageHandlerWithProtocolVersion ::
    ( HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?context :: Request
    , ?request :: Request
    , ?modelContext :: ModelContext
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    )
    => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (Text -> IO ColumnTypeInfo) -> IO (Maybe Int -> DataSyncMessage -> IO ())
buildMessageHandlerWithProtocolVersion hasqlPool ensureRLSEnabled _installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup = do
    getRLSColumns <- makeCachedRLSPolicyColumns hasqlPool
    subscriptionClosedSignals <- newIORef HashMap.empty
    installInvalidationPlan <- ChangeNotifications.makeInstallInvalidationPlan hasqlPool
    let ?subscriptionClosedSignals = subscriptionClosedSignals
    let ?installInvalidationPlan = installInvalidationPlan
    pure (handleMessage getRLSColumns)
    where
            pgListener = ?request.pgListener
            handleMessage ::
                ( ?subscriptionClosedSignals :: IORef (HashMap UUID (MVar.MVar ()))
                , ?installInvalidationPlan :: ChangeNotifications.InvalidationPlan -> IO ()
                ) => (Text -> IO (Set.Set Text)) -> Maybe Int -> DataSyncMessage -> IO ()
            handleMessage getRLSColumns _protocolVersion DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (query.table)

                columnTypes <- columnTypeLookup query.table
                let querySnippet = compileQueryTyped (renamer query.table) columnTypes query
                let stmt = compiledQueryStatement querySnippet

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DataSyncResult { result, requestId }

            handleMessage _getRLSColumns protocolVersion CreateDataSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled (query.table)
                columnTypes <- columnTypeLookup query.table
                let queryRenamer = renamer query.table
                let tableHasUuidId = HashMap.lookup "id" columnTypes.typeMap == Just "uuid"
                let supportsSnapshots = maybe False (>= 1) protocolVersion
                let selectsId = case query.selectedColumns of
                        SelectAll -> tableHasUuidId
                        SelectSpecific columns -> any ((== "id") . queryRenamer.fieldToColumn) columns
                when (not supportsSnapshots && (not tableHasUuidId || not selectsId)) do
                    Exception.throwIO (userError "Legacy DataSubscriptions require the id column to be selected with UUID type; use protocolVersion 1 for projections without id")

                subscriptionId <- UUID.nextRandom

                close <- MVar.newEmptyMVar
                closed <- MVar.newEmptyMVar

                let querySnippet = compileQueryTyped queryRenamer columnTypes query
                let stmt = compiledQueryStatement querySnippet

                invalidationPlan <- ChangeNotifications.resolveInvalidationPlan hasqlPool tableNameRLS
                ?installInvalidationPlan invalidationPlan
                let subscriptionChannels = invalidationPlan.channels

                snapshotStateRef <- newIORef (Nothing :: Maybe (Int, [[Field]]))
                initialReady <- MVar.newEmptyMVar
                refreshSignal <- MVar.newEmptyMVar

                -- Legacy deltas are derived exclusively from two complete RLS-filtered
                -- snapshots. Notification payloads can therefore never reveal a value
                -- that is no longer visible to the current user. Re-deleting both the old
                -- and new id sets also makes a retry safe after a partial socket write.
                let sendLegacyReplacement previousResult nextResult = do
                        let idsToClear = Set.fromList (recordIds previousResult <> recordIds nextResult)
                        forM_ idsToClear \id ->
                            sendJSON DidDelete { subscriptionId, id }
                        forM_ nextResult \record ->
                            sendJSON DidInsert { subscriptionId, record }

                let refreshSnapshot = do
                        Just (revision, previousResult) <- readIORef snapshotStateRef
                        nextResult :: [[Field]] <- sqlQueryWithRLS hasqlPool stmt
                        let resultChanged = Aeson.toJSON nextResult /= Aeson.toJSON previousResult
                        when resultChanged do
                            let nextRevision = revision + 1
                            if supportsSnapshots
                                then sendJSON DidReplaceDataSubscription
                                    { subscriptionId
                                    , revision = nextRevision
                                    , result = nextResult
                                    }
                                else sendLegacyReplacement previousResult nextResult
                            -- Advance only after every wire message was sent. A transient
                            -- send failure retries from the last fully delivered snapshot.
                            writeIORef snapshotStateRef (Just (nextRevision, nextResult))

                let retryRefresh delay = do
                        result <- Exception.tryAny refreshSnapshot
                        case result of
                            Right () -> pure ()
                            Left exception -> do
                                ?modelContext.logger (toLogStr ("DataSync subscription refresh failed: " <> displayException exception))
                                Concurrent.threadDelay delay
                                retryRefresh (min 5_000_000 (delay * 2))

                let refreshWorker = forever do
                        MVar.takeMVar refreshSignal
                        MVar.readMVar initialReady
                        retryRefresh 50_000

                let signalRefresh = void (MVar.tryPutMVar refreshSignal ())
                let subscribe = subscribeToInvalidationChannels pgListener subscriptionChannels signalRefresh
                let unsubscribe = unsubscribeAll pgListener

                let reserveSubscription = do
                        atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                            (HashMap.insert subscriptionId closed signals, ())
                        reserved <- atomicModifyIORef' ?state \state ->
                            if HashMap.size state.subscriptions >= maxSubscriptionsPerConnection
                                then (state, False)
                                else (state |> modify #subscriptions (HashMap.insert subscriptionId close), True)
                        unless reserved do
                            atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                                (HashMap.delete subscriptionId signals, ())
                            Exception.throwIO (userError ("You've reached the subscriptions limit of " <> cs (tshow maxSubscriptionsPerConnection) <> " subscriptions"))

                let releaseSubscription = do
                        wasStillRegistered <- atomicModifyIORef' ?state \state ->
                            let wasRegistered = HashMap.member subscriptionId state.subscriptions
                            in (state |> modify #subscriptions (HashMap.delete subscriptionId), wasRegistered)
                        void (MVar.tryPutMVar closed ())
                        -- If DeleteDataSubscription removed the state entry, it owns
                        -- this completion MVar until it has observed cleanup.
                        when wasStillRegistered do
                            atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                                (HashMap.delete subscriptionId signals, ())

                Exception.bracket_
                    reserveSubscription
                    releaseSubscription
                    (Exception.bracket subscribe unsubscribe \_channelSubscriptions ->
                        Exception.bracket (async refreshWorker) cancel \_refreshWorker ->
                            Exception.finally
                                (do
                                    isListening <- PGListener.waitUntilListeningTo 10_000_000 subscriptionChannels pgListener
                                    unless isListening do
                                        Exception.throwIO (userError "Timed out waiting for PostgreSQL LISTEN while creating DataSubscription")

                                    result :: [[Field]] <- sqlQueryWithRLS hasqlPool stmt
                                    if supportsSnapshots
                                        then sendJSON DidCreateDataSubscriptionV2
                                            { subscriptionId
                                            , requestId
                                            , revision = 0
                                            , result
                                            }
                                        else sendJSON DidCreateDataSubscription
                                            { subscriptionId
                                            , requestId
                                            , result
                                            }
                                    writeIORef snapshotStateRef (Just (0, result))
                                    MVar.putMVar initialReady ()
                                    MVar.takeMVar close
                                )
                                (void (MVar.tryPutMVar initialReady ()))
                    )

            handleMessage _getRLSColumns _protocolVersion CreateCountSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled query.table

                subscriptionId <- UUID.nextRandom

                close <- MVar.newEmptyMVar
                closed <- MVar.newEmptyMVar

                columnTypes <- columnTypeLookup query.table
                let querySnippet = compileQueryTyped (renamer query.table) columnTypes query

                let countSnippet = Snippet.sql "SELECT COUNT(*) FROM (" <> querySnippet <> Snippet.sql ") AS _inner"
                let countDecoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))
                let countStmt = Snippet.toPreparableStatement countSnippet countDecoder

                invalidationPlan <- ChangeNotifications.resolveInvalidationPlan hasqlPool tableNameRLS
                ?installInvalidationPlan invalidationPlan
                let subscriptionChannels = invalidationPlan.channels

                countRef <- newIORef (Nothing :: Maybe Int)
                initialReady <- MVar.newEmptyMVar
                refreshSignal <- MVar.newEmptyMVar

                let refreshCount = do
                        Just lastCount <- readIORef countRef
                        newCount :: Int <- sqlQueryScalarWithRLS hasqlPool countStmt
                        when (newCount /= lastCount) do
                            sendJSON DidChangeCount { subscriptionId, count = newCount }
                            writeIORef countRef (Just newCount)

                let retryRefresh delay = do
                        result <- Exception.tryAny refreshCount
                        case result of
                            Right () -> pure ()
                            Left exception -> do
                                ?modelContext.logger (toLogStr ("DataSync count refresh failed: " <> displayException exception))
                                Concurrent.threadDelay delay
                                retryRefresh (min 5_000_000 (delay * 2))

                let refreshWorker = forever do
                        MVar.takeMVar refreshSignal
                        MVar.readMVar initialReady
                        retryRefresh 50_000

                let signalRefresh = void (MVar.tryPutMVar refreshSignal ())
                let subscribe = subscribeToInvalidationChannels pgListener subscriptionChannels signalRefresh
                let unsubscribe = unsubscribeAll pgListener

                let reserveSubscription = do
                        atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                            (HashMap.insert subscriptionId closed signals, ())
                        reserved <- atomicModifyIORef' ?state \state ->
                            if HashMap.size state.subscriptions >= maxSubscriptionsPerConnection
                                then (state, False)
                                else (state |> modify #subscriptions (HashMap.insert subscriptionId close), True)
                        unless reserved do
                            atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                                (HashMap.delete subscriptionId signals, ())
                            Exception.throwIO (userError ("You've reached the subscriptions limit of " <> cs (tshow maxSubscriptionsPerConnection) <> " subscriptions"))

                let releaseSubscription = do
                        wasStillRegistered <- atomicModifyIORef' ?state \state ->
                            let wasRegistered = HashMap.member subscriptionId state.subscriptions
                            in (state |> modify #subscriptions (HashMap.delete subscriptionId), wasRegistered)
                        void (MVar.tryPutMVar closed ())
                        when wasStillRegistered do
                            atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                                (HashMap.delete subscriptionId signals, ())

                Exception.bracket_
                    reserveSubscription
                    releaseSubscription
                    (Exception.bracket subscribe unsubscribe \_channelSubscriptions ->
                        Exception.bracket (async refreshWorker) cancel \_refreshWorker ->
                            Exception.finally
                                (do
                                    isListening <- PGListener.waitUntilListeningTo 10_000_000 subscriptionChannels pgListener
                                    unless isListening do
                                        Exception.throwIO (userError "Timed out waiting for PostgreSQL LISTEN while creating CountSubscription")

                                    count :: Int <- sqlQueryScalarWithRLS hasqlPool countStmt
                                    sendJSON DidCreateCountSubscription { subscriptionId, requestId, count }
                                    writeIORef countRef (Just count)
                                    MVar.putMVar initialReady ()
                                    MVar.takeMVar close
                                )
                                (void (MVar.tryPutMVar initialReady ()))
                    )

            handleMessage _getRLSColumns _protocolVersion DeleteDataSubscription { requestId, subscriptionId } = do
                closeSignal <- atomicModifyIORef' ?state \state ->
                    let signal = HashMap.lookup subscriptionId state.subscriptions
                    in (state |> modify #subscriptions (HashMap.delete subscriptionId), signal)
                case closeSignal of
                    Just close -> do
                        closedSignals <- readIORef ?subscriptionClosedSignals
                        closed <- case HashMap.lookup subscriptionId closedSignals of
                            Just signal -> pure signal
                            Nothing -> Exception.throwIO (userError "DataSubscription lifecycle completion signal is missing")

                        void (MVar.tryPutMVar close ())
                        MVar.readMVar closed
                        atomicModifyIORef' ?subscriptionClosedSignals \signals ->
                            (HashMap.delete subscriptionId signals, ())

                        sendJSON DidDeleteDataSubscription { subscriptionId, requestId }
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "Failed to delete DataSubscription, could not find DataSubscription with id " <> tshow subscriptionId }

            handleMessage getRLSColumns _protocolVersion CreateRecordMessage { table, record, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let pairsList = record
                        |> HashMap.toList
                        |> map (\(fieldName, val) ->
                            let col = (renamer table).fieldToColumn fieldName
                            in (col, lookupColumnType columnTypes col, val)
                        )

                let columns = map (\(c,_,_) -> c) pairsList
                let valueSnippets = map (\(_, colType, val) -> typedAesonValueToSnippet colType val) pairsList
                let insertResult = compileInsert table columns valueSnippets (renamer table) columnTypes
                let stmt = compiledQueryStatement insertResult

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                case result of
                    [record] ->
                        sendJSON DidCreateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Unexpected result in CreateRecordMessage handler" }

                pure ()

            handleMessage getRLSColumns _protocolVersion CreateRecordsMessage { table, records, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                case head records of
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "At least one record is required" }
                    Just firstRecord -> do
                        let fieldNames = HashMap.keys firstRecord
                        let columns = map (renamer table).fieldToColumn fieldNames

                        let encodeRow object = map
                                (\(fieldName, col) -> typedAesonValueToSnippet (lookupColumnType columnTypes col) (fromMaybe Aeson.Null (HashMap.lookup fieldName object)))
                                (zip fieldNames columns)
                        let valueRows = map encodeRow records

                        let insertResult = compileInsertMany table columns valueRows (renamer table) columnTypes
                        let stmt = compiledQueryStatement insertResult

                        records :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                        sendJSON DidCreateRecords { requestId, records }

                        pure ()

            handleMessage getRLSColumns _protocolVersion UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let setSql = encodePatchToSetSql (renamer table) columnTypes patch
                let updateResult = compileUpdate table setSql (Snippet.sql "id = " <> uuidParam id) (renamer table) columnTypes
                let stmt = compiledQueryStatement updateResult

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                case result of
                    [record] ->
                        sendJSON DidUpdateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Could not apply the update to the given record. Are you sure the record ID you passed is correct? If the record ID is correct, likely the row level security policy is not making the record visible to the UPDATE operation." }

                pure ()

            handleMessage getRLSColumns _protocolVersion UpdateRecordsMessage { table, ids, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let setSql = encodePatchToSetSql (renamer table) columnTypes patch
                let inList = mconcat $ List.intersperse (Snippet.sql ", ") (map uuidParam ids)
                let updateResult = compileUpdate table setSql (Snippet.sql "id IN (" <> inList <> Snippet.sql ")") (renamer table) columnTypes
                let stmt = compiledQueryStatement updateResult

                records <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidUpdateRecords { requestId, records }

                pure ()

            handleMessage getRLSColumns _protocolVersion DeleteRecordMessage { table, id, requestId, transactionId } = do
                ensureRLSEnabled table

                let deleteSnippet = Snippet.sql ("DELETE FROM " <> quoteIdentifier table <> " WHERE id = ") <> uuidParam id
                let stmt = Snippet.toPreparableStatement deleteSnippet Decoders.noResult
                sqlExecWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidDeleteRecord { requestId }

            handleMessage getRLSColumns _protocolVersion DeleteRecordsMessage { table, ids, requestId, transactionId } = do
                ensureRLSEnabled table

                let inList = mconcat $ List.intersperse (Snippet.sql ", ") (map uuidParam ids)
                let stmt = Snippet.toPreparableStatement (Snippet.sql ("DELETE FROM " <> quoteIdentifier table <> " WHERE id IN (") <> inList <> Snippet.sql ")") Decoders.noResult
                sqlExecWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidDeleteRecords { requestId }

            handleMessage getRLSColumns _protocolVersion StartTransaction { requestId } = do
                ensureBelowTransactionLimit

                transactionId <- UUID.nextRandom

                withDedicatedConnection ?context.frameworkConfig.databaseUrl \connection -> do
                    transactionSignal <- MVar.newEmptyMVar

                    runSessionOnConnection connection $ do
                        Session.script "BEGIN"
                        setRLSConfigSession

                    let transaction = DataSyncTransaction
                            { id = transactionId
                            , connection
                            , close = transactionSignal
                            }

                    atomicModifyIORef'' ?state (\state -> state |> modify #transactions (HashMap.insert transactionId transaction))

                    sendJSON DidStartTransaction { requestId, transactionId }

                    MVar.takeMVar transactionSignal

                    atomicModifyIORef'' ?state (\state -> state |> modify #transactions (HashMap.delete transactionId))

            handleMessage getRLSColumns _protocolVersion RollbackTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.script "ROLLBACK")
                MVar.putMVar close ()

                sendJSON DidRollbackTransaction { requestId, transactionId = id }

            handleMessage getRLSColumns _protocolVersion CommitTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.script "COMMIT")
                MVar.putMVar close ()

                sendJSON DidCommitTransaction { requestId, transactionId = id }

            handleMessage _getRLSColumns _protocolVersion otherwise = handleCustomMessage sendJSON otherwise

-- | Register all relation invalidation channels as one exception-safe resource.
-- Async exceptions are restored while each PGListener subscription is created,
-- then masked again before its handle is added to the cleanup list.
subscribeToInvalidationChannels :: PGListener.PGListener -> Set.Set ByteString -> IO () -> IO [PGListener.Subscription]
subscribeToInvalidationChannels pgListener channels signalRefresh = Exception.mask \restore -> do
    acquiredRef <- newIORef []
    let cleanup = readIORef acquiredRef >>= unsubscribeAll pgListener
    let acquire (isFirst, channel) = do
            let reconnectCallback = if isFirst then signalRefresh else pure ()
            subscription <- restore
                (PGListener.subscribeWithReconnect channel (const signalRefresh) reconnectCallback pgListener)
            modifyIORef' acquiredRef (subscription :)
            pure subscription
    let channelList = Set.toList channels
    let markedChannels = zip (True : repeat False) channelList
    mapM acquire markedChannels `Exception.onException` cleanup

unsubscribeAll :: PGListener.PGListener -> [PGListener.Subscription] -> IO ()
unsubscribeAll _pgListener [] = pure ()
unsubscribeAll pgListener (subscription : remaining) =
    PGListener.unsubscribe subscription pgListener
        `Exception.finally` unsubscribeAll pgListener remaining

changesToValue :: Renamer -> [ChangeNotifications.Change] -> (Maybe Value, Maybe Value)
changesToValue renamer changes = (maybeObject replacePairs, maybeObject appendPairs)
    where
        maybeObject [] = Nothing
        maybeObject pairs = Just (object pairs)
        replacePairs = mapMaybe toReplacePair changes
        appendPairs  = mapMaybe toAppendPair changes
        toReplacePair ChangeNotifications.Change { col, new } =
            Just $ (Aeson.fromText $ renamer.columnToField col) .= new
        toReplacePair _ = Nothing
        toAppendPair ChangeNotifications.AppendChange { col, append } =
            Just $ (Aeson.fromText $ renamer.columnToField col) .= append
        toAppendPair _ = Nothing

findTransactionById :: (?state :: IORef DataSyncController) => UUID -> IO DataSyncTransaction
findTransactionById transactionId = do
    transactions <- (.transactions) <$> readIORef ?state
    case HashMap.lookup transactionId transactions of
        Just transaction -> pure transaction
        Nothing -> Exception.throwIO (userError ("No transaction with id " <> cs (tshow transactionId)))

-- | Allow max 10 concurrent transactions per connection to avoid running out of database connections
--
-- Each transaction removes a database connection from the connection pool. If we don't limit the transactions,
-- a single user could take down the application by starting more than the pool size (HASQL_POOL_SIZE)
-- concurrent transactions. Then all database connections are removed from the connection pool and further database
-- queries for other users will fail.
--
ensureBelowTransactionLimit :: (?state :: IORef DataSyncController, ?request :: Request) => IO ()
ensureBelowTransactionLimit = do
    transactions <- (.transactions) <$> readIORef ?state
    let transactionCount = HashMap.size transactions
    when (transactionCount >= maxTransactionsPerConnection) do
        Exception.throwIO (userError ("You've reached the transaction limit of " <> cs (tshow maxTransactionsPerConnection) <> " transactions"))

ensureBelowSubscriptionsLimit :: (?state :: IORef DataSyncController, ?request :: Request) => IO ()
ensureBelowSubscriptionsLimit = do
    subscriptions <- (.subscriptions) <$> readIORef ?state
    let subscriptionsCount = HashMap.size subscriptions
    when (subscriptionsCount >= maxSubscriptionsPerConnection) do
        Exception.throwIO (userError ("You've reached the subscriptions limit of " <> cs (tshow maxSubscriptionsPerConnection) <> " subscriptions"))

maxTransactionsPerConnection :: (?request :: Request) => Int
maxTransactionsPerConnection =
    let ?context = ?request
    in case getAppConfig @DataSyncMaxTransactionsPerConnection of
        DataSyncMaxTransactionsPerConnection value -> value

maxSubscriptionsPerConnection :: (?request :: Request) => Int
maxSubscriptionsPerConnection =
    let ?context = ?request
    in case getAppConfig @DataSyncMaxSubscriptionsPerConnection of
        DataSyncMaxSubscriptionsPerConnection value -> value

-- | Encode a JSON patch (field name -> value) into a SQL SET clause 'Snippet' like @"col1" = $1, "col2" = $2@.
encodePatchToSetSql :: Renamer -> ColumnTypeInfo -> HashMap Text Value -> Snippet
encodePatchToSetSql ren columnTypes patch =
    let pairsList = patch
            |> HashMap.toList
            |> map (\(fieldName, val) ->
                let col = ren.fieldToColumn fieldName
                in (col, lookupColumnType columnTypes col, val)
            )
        encodeSetClause (col, colType, val) =
            Snippet.sql (quoteIdentifier col <> " = ") <> typedAesonValueToSnippet colType val
        setSnippets = map encodeSetClause pairsList
    in mconcat $ List.intersperse (Snippet.sql ", ") setSnippets

sqlQueryWithRLSAndTransactionId ::
    ( ?context :: Request
    , ?request :: Request
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Hasql.Statement () [result] -> IO [result]
sqlQueryWithRLSAndTransactionId _pool (Just transactionId) statement = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (Session.statement () statement)
sqlQueryWithRLSAndTransactionId pool Nothing statement = runSession pool (sqlQueryWithRLSSession statement)

-- | Like 'sqlQueryWithRLSAndTransactionId', but uses a write transaction when no transaction ID is provided.
--
-- Use this for INSERT, UPDATE, or DELETE statements with RETURNING that need
-- to return results (e.g. wrapped with 'wrapDynamicQuery').
sqlQueryWriteWithRLSAndTransactionId ::
    ( ?context :: Request
    , ?request :: Request
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Hasql.Statement () [result] -> IO [result]
sqlQueryWriteWithRLSAndTransactionId _pool (Just transactionId) statement = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (Session.statement () statement)
sqlQueryWriteWithRLSAndTransactionId pool Nothing statement = runSession pool (sqlQueryWriteWithRLSSession statement)

sqlExecWithRLSAndTransactionId ::
    ( ?context :: Request
    , ?request :: Request
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Hasql.Statement () () -> IO ()
sqlExecWithRLSAndTransactionId _pool (Just transactionId) statement = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (Session.statement () statement)
sqlExecWithRLSAndTransactionId pool Nothing statement = runSession pool (sqlExecWithRLSSession statement)


instance SetField "subscriptions" DataSyncController (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "transactions" DataSyncController (HashMap UUID DataSyncTransaction) where
    setField transactions record = record { transactions }

atomicModifyIORef'' ref updateFn = atomicModifyIORef' ref (\value -> (updateFn value, ()))
