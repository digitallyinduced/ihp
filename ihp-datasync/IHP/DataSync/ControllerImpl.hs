{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.ControllerImpl where

import IHP.ControllerPrelude hiding (OrderByClause, sqlQuery, sqlExec, sqlQueryScalar)
import qualified Control.Exception.Safe as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson

import Data.Aeson.TH
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Statement as Hasql
import qualified Hasql.Pool
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession, runSessionOnConnection, withDedicatedConnection)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.TypedEncoder (makeCachedColumnTypeLookup, typedAesonValueToSnippet, lookupColumnType)
import IHP.QueryBuilder.HasqlCompiler (CompilerState(..), emptyCompilerState, nextParam)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import qualified IHP.PGListener as PGListener
import qualified Data.Set as Set
import GHC.Conc (ThreadId, myThreadId, atomically)
import Control.Concurrent.QSemN
import Control.Concurrent.STM.TVar
import qualified Data.List as List

$(deriveFromJSON defaultOptions ''DataSyncMessage)
$(deriveToJSON defaultOptions { omitNothingFields = True } 'DataSyncResult)

type EnsureRLSEnabledFn = Text -> IO TableWithRLS
type InstallTableChangeTriggerFn = TableWithRLS -> IO ()
type SendJSONFn = DataSyncResponse -> IO ()
type HandleCustomMessageFn = (DataSyncResponse -> IO ()) -> DataSyncMessage -> IO ()

runDataSyncController ::
    ( HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> IO ByteString -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> IO ()
runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers receiveData sendJSON handleCustomMessage renamer = do
    setState DataSyncReady { subscriptions = HashMap.empty, transactions = HashMap.empty }

    columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
    handleMessage :: DataSyncMessage -> IO () <- buildMessageHandler hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup


    sem  <- newQSemN (maxSubscriptionsPerConnection * 2) -- needs to be larger than the subscriptions limit to trigger an error on overload. otherwise an overflow of connections might queue up silently

    -- Track Asyncs so we can cancel/wait on socket close
    childrenVar <- newTVarIO (HashMap.empty :: HashMap ThreadId (Async ()))

    let spawnWorker decodedMessage = do
            tidReady <- MVar.newEmptyMVar
            a <- asyncWithUnmask \unmask -> do
                -- Register myself
                tid <- myThreadId
                MVar.putMVar tidReady tid
                -- Take/release concurrency slot entirely inside the worker
                Exception.bracket_ (waitQSemN sem 1) (signalQSemN sem 1) do
                    Exception.finally
                        (unmask do
                            result <- Exception.try (handleMessage decodedMessage)
                            case result of
                                Left (e :: Exception.SomeException) -> do
                                    let requestId    = decodedMessage.requestId
                                    let errorMessage = cs (displayException e)
                                    Log.error (tshow e)
                                    sendJSON DataSyncError { requestId, errorMessage }
                                Right _ -> pure ()
                        )
                        -- Self-deregister
                        (do
                            tid' <- myThreadId
                            atomically $ modifyTVar' childrenVar (HashMap.delete tid')
                        )
            -- Parent stores the Async by ThreadId (no race thanks to tidReady)
            tid <- MVar.takeMVar tidReady
            atomically $ modifyTVar' childrenVar (HashMap.insert tid a)

    let loop = forever do
            msg <- Aeson.eitherDecodeStrict' <$> receiveData
            case msg of
                Right decoded -> spawnWorker decoded
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
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , ?request :: Request
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (Id' (GetTableName CurrentUserRecord))
    )
    => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (Text -> IO ColumnTypeInfo) -> IO (DataSyncMessage -> IO ())
buildMessageHandler hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup = do
    getRLSColumns <- makeCachedRLSPolicyColumns hasqlPool
    pure (handleMessage getRLSColumns)
    where
            pgListener = ?request.pgListener
            handleMessage :: (Text -> IO (Set.Set Text)) -> DataSyncMessage -> IO ()
            handleMessage getRLSColumns DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (query.table)

                columnTypes <- columnTypeLookup query.table
                let queryResult = compileQueryTyped (renamer query.table) columnTypes query
                let stmt = compiledQueryStatement queryResult

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DataSyncResult { result, requestId }

            handleMessage getRLSColumns CreateDataSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled (query.table)

                subscriptionId <- UUID.nextRandom

                -- Allocate the close handle as early as possible
                -- to make DeleteDataSubscription calls succeed even when the DataSubscription is
                -- not fully set up yet
                close <- MVar.newEmptyMVar
                atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId close))

                columnTypes <- columnTypeLookup query.table
                let theQuery@(CompiledQuery querySql queryCc) = compileQueryTyped (renamer query.table) columnTypes query
                let stmt = compiledQueryStatement theQuery

                result :: [[Field]] <- sqlQueryWithRLS hasqlPool stmt

                let tableName = query.table

                -- Compute "sensitive columns": the union of columns referenced in the
                -- WHERE clause and columns referenced in RLS policies. When an UPDATE
                -- only touches columns outside this set, the record cannot leave the
                -- result set or change RLS visibility, so we can skip the EXISTS check.
                rlsCols <- getRLSColumns tableName
                let whereColumns = maybe Set.empty conditionColumns (query.whereCondition)
                let whereColumnsDb = Set.map (renamer tableName).fieldToColumn whereColumns
                let sensitiveColumns = Set.union whereColumnsDb rlsCols

                -- We need to keep track of all the ids of entities we're watching to make
                -- sure that we only send update notifications to clients that can actually
                -- access the record (e.g. if a RLS policy denies access)
                let watchedRecordIds = recordIds result

                -- Store it in IORef as an INSERT requires us to add an id
                watchedRecordIdsRef <- newIORef (Set.fromList watchedRecordIds)

                -- Make sure the database triggers are there
                installTableChangeTriggers tableNameRLS

                let handleUpdate id getChanges = do
                        isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                        when isWatchingRecord do
                            changes <- getChanges
                            let changedCols = Set.fromList (map (.col) changes)
                            let affectsFilterOrRLS = not (Set.disjoint changedCols sensitiveColumns)
                            let (changeSetVal, appendSetVal) = changesToValue (renamer tableName) changes
                            if affectsFilterOrRLS
                                then do
                                    let (idPh, cc') = nextParam (uuidParam id) queryCc
                                    let existsStmt = toStatement ("SELECT EXISTS(SELECT * FROM (" <> querySql <> ") AS records WHERE records.id = " <> idPh <> " LIMIT 1)") cc' (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                                    isRecordInResultSet :: Bool <- sqlQueryScalarWithRLS hasqlPool existsStmt
                                    if isRecordInResultSet
                                        then sendJSON DidUpdate { subscriptionId, id, changeSet = changeSetVal, appendSet = appendSetVal }
                                        else do
                                            modifyIORef' watchedRecordIdsRef (Set.delete id)
                                            sendJSON DidDelete { subscriptionId, id }
                                else
                                    sendJSON DidUpdate { subscriptionId, id, changeSet = changeSetVal, appendSet = appendSetVal }

                let callback notification = case notification of
                            ChangeNotifications.DidInsert { id } -> do
                                -- The new record could not be accessible to the current user with a RLS policy
                                -- E.g. it could be a new record in a 'projects' table, but the project belongs
                                -- to a different user, and thus the current user should not be able to see it.
                                --
                                -- The new record could also be not part of the WHERE condition of the initial query.
                                -- Therefore we need to use the subscriptions WHERE condition to fetch the new record here.
                                --
                                -- To honor the RLS policies we therefore need to fetch the record as the current user
                                -- If the result set is empty, we know the record is not accesible to us
                                let (idPh, cc') = nextParam (uuidParam id) queryCc
                                let filterStmt = toStatement (wrapDynamicQuery ("SELECT * FROM (" <> querySql <> ") AS records WHERE records.id = " <> idPh <> " LIMIT 1")) cc' dynamicRowDecoder
                                newRecord :: [[Field]] <- sqlQueryWithRLS hasqlPool filterStmt

                                case headMay newRecord of
                                    Just record -> do
                                        -- Add the new record to 'watchedRecordIdsRef'
                                        -- Otherwise the updates and deletes will not be dispatched to the client
                                        modifyIORef' watchedRecordIdsRef (Set.insert id)

                                        sendJSON DidInsert { subscriptionId, record }
                                    Nothing -> pure ()
                            ChangeNotifications.DidUpdate { id, changeSet } ->
                                handleUpdate id (ChangeNotifications.retrieveChanges hasqlPool changeSet)
                            ChangeNotifications.DidUpdateLarge { id, payloadId } ->
                                handleUpdate id (ChangeNotifications.retrieveChanges hasqlPool (ChangeNotifications.ExternalChangeSet { largePgNotificationId = payloadId }))
                            ChangeNotifications.DidDelete { id } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    sendJSON DidDelete { subscriptionId, id }

                let subscribe = PGListener.subscribeJSON (ChangeNotifications.channelName tableNameRLS) callback pgListener
                let unsubscribe subscription = PGListener.unsubscribe subscription pgListener

                Exception.bracket subscribe unsubscribe \channelSubscription -> do
                    sendJSON DidCreateDataSubscription { subscriptionId, requestId, result }

                    MVar.takeMVar close

            handleMessage getRLSColumns CreateCountSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled query.table

                subscriptionId <- UUID.nextRandom

                -- Allocate the close handle as early as possible
                -- to make DeleteDataSubscription calls succeed even when the CountSubscription is
                -- not fully set up yet
                close <- MVar.newEmptyMVar
                atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId close))

                columnTypes <- columnTypeLookup query.table
                let CompiledQuery querySql queryCc = compileQueryTyped (renamer query.table) columnTypes query

                let countSql = "SELECT COUNT(*) FROM (" <> querySql <> ") AS _inner"
                let countDecoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))
                let countStmt = toStatement countSql queryCc countDecoder

                count :: Int <- sqlQueryScalarWithRLS hasqlPool countStmt
                countRef <- newIORef count

                installTableChangeTriggers tableNameRLS

                let
                    callback :: ChangeNotifications.ChangeNotification -> IO ()
                    callback _ = do
                        newCount :: Int <- sqlQueryScalarWithRLS hasqlPool countStmt
                        lastCount <- readIORef countRef

                        when (newCount /= lastCount) do
                            writeIORef countRef newCount
                            sendJSON DidChangeCount { subscriptionId, count = newCount }

                let subscribe = PGListener.subscribeJSON (ChangeNotifications.channelName tableNameRLS) callback pgListener
                let unsubscribe subscription = PGListener.unsubscribe subscription pgListener

                Exception.bracket subscribe unsubscribe \channelSubscription -> do
                    sendJSON DidCreateCountSubscription { subscriptionId, requestId, count }

                    MVar.takeMVar close

            handleMessage getRLSColumns DeleteDataSubscription { requestId, subscriptionId } = do
                DataSyncReady { subscriptions } <- getState
                case HashMap.lookup subscriptionId subscriptions of
                    Just closeSignalMVar -> do
                        -- Cancel table watcher
                        MVar.putMVar closeSignalMVar ()

                        atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.delete subscriptionId))

                        sendJSON DidDeleteDataSubscription { subscriptionId, requestId }
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "Failed to delete DataSubscription, could not find DataSubscription with id " <> tshow subscriptionId }

            handleMessage getRLSColumns CreateRecordMessage { table, record, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let pairsList = record
                        |> HashMap.toList
                        |> map (\(fieldName, val) ->
                            let col = (renamer table).fieldToColumn fieldName
                            in (col, lookupColumnType columnTypes col, val)
                        )

                let columns = map (\(c,_,_) -> c) pairsList
                let encodeOne st (_, colType, val) =
                        let (t, st') = typedAesonValueToSnippet colType val st in (st', t)
                let (cc, valueTexts) = List.mapAccumL encodeOne emptyCompilerState pairsList
                let insertResult = compileInsert table columns valueTexts cc (renamer table) columnTypes
                let stmt = compiledQueryStatement insertResult

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                case result of
                    [record] ->
                        sendJSON DidCreateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Unexpected result in CreateRecordMessage handler" }

                pure ()

            handleMessage getRLSColumns CreateRecordsMessage { table, records, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                case head records of
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "At least one record is required" }
                    Just firstRecord -> do
                        let fieldNames = HashMap.keys firstRecord
                        let columns = map (renamer table).fieldToColumn fieldNames

                        let encodeRow ccRow object = List.mapAccumL
                                (\st (fieldName, col) -> case typedAesonValueToSnippet (lookupColumnType columnTypes col) (fromMaybe Aeson.Null (HashMap.lookup fieldName object)) st of
                                    (t, st') -> (st', t))
                                ccRow
                                (zip fieldNames columns)
                        let (ccFinal, valueRows) = List.mapAccumL encodeRow emptyCompilerState records

                        let insertResult = compileInsertMany table columns valueRows ccFinal (renamer table) columnTypes
                        let stmt = compiledQueryStatement insertResult

                        records :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                        sendJSON DidCreateRecords { requestId, records }

                        pure ()

            handleMessage getRLSColumns UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let (setSql, cc0) = encodePatchToSetSql (renamer table) columnTypes patch
                let (idPh, cc1) = nextParam (uuidParam id) cc0
                let updateResult = compileUpdate table setSql ("id = " <> idPh) cc1 (renamer table) columnTypes
                let stmt = compiledQueryStatement updateResult

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                case result of
                    [record] ->
                        sendJSON DidUpdateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Could not apply the update to the given record. Are you sure the record ID you passed is correct? If the record ID is correct, likely the row level security policy is not making the record visible to the UPDATE operation." }

                pure ()

            handleMessage getRLSColumns UpdateRecordsMessage { table, ids, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let (setSql, cc0) = encodePatchToSetSql (renamer table) columnTypes patch
                let (cc1, idPhs) = List.mapAccumL (\st uuid -> case nextParam (uuidParam uuid) st of (t, st') -> (st', t)) cc0 ids
                let inList = mconcat $ List.intersperse ", " idPhs
                let updateResult = compileUpdate table setSql ("id IN (" <> inList <> ")") cc1 (renamer table) columnTypes
                let stmt = compiledQueryStatement updateResult

                records <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidUpdateRecords { requestId, records }

                pure ()

            handleMessage getRLSColumns DeleteRecordMessage { table, id, requestId, transactionId } = do
                ensureRLSEnabled table

                let (idPh, cc) = nextParam (uuidParam id) emptyCompilerState
                let stmt = Hasql.preparable ("DELETE FROM " <> quoteIdentifier table <> " WHERE id = " <> idPh) (ccEncoder cc) Decoders.noResult
                sqlExecWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidDeleteRecord { requestId }

            handleMessage getRLSColumns DeleteRecordsMessage { table, ids, requestId, transactionId } = do
                ensureRLSEnabled table

                let (ccFinal, idPhs) = List.mapAccumL (\st uuid -> case nextParam (uuidParam uuid) st of (t, st') -> (st', t)) emptyCompilerState ids
                let inList = mconcat $ List.intersperse ", " idPhs
                let stmt = Hasql.preparable ("DELETE FROM " <> quoteIdentifier table <> " WHERE id IN (" <> inList <> ")") (ccEncoder ccFinal) Decoders.noResult
                sqlExecWithRLSAndTransactionId hasqlPool transactionId stmt

                sendJSON DidDeleteRecords { requestId }

            handleMessage getRLSColumns StartTransaction { requestId } = do
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

            handleMessage getRLSColumns RollbackTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.script "ROLLBACK")
                MVar.putMVar close ()

                sendJSON DidRollbackTransaction { requestId, transactionId = id }

            handleMessage getRLSColumns CommitTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.script "COMMIT")
                MVar.putMVar close ()

                sendJSON DidCommitTransaction { requestId, transactionId = id }

            handleMessage _getRLSColumns otherwise = handleCustomMessage sendJSON otherwise

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
ensureBelowTransactionLimit :: (?state :: IORef DataSyncController, ?context :: ControllerContext) => IO ()
ensureBelowTransactionLimit = do
    transactions <- (.transactions) <$> readIORef ?state
    let transactionCount = HashMap.size transactions
    when (transactionCount >= maxTransactionsPerConnection) do
        Exception.throwIO (userError ("You've reached the transaction limit of " <> cs (tshow maxTransactionsPerConnection) <> " transactions"))

ensureBelowSubscriptionsLimit :: (?state :: IORef DataSyncController, ?context :: ControllerContext) => IO ()
ensureBelowSubscriptionsLimit = do
    subscriptions <- (.subscriptions) <$> readIORef ?state
    let subscriptionsCount = HashMap.size subscriptions
    when (subscriptionsCount >= maxSubscriptionsPerConnection) do
        Exception.throwIO (userError ("You've reached the subscriptions limit of " <> cs (tshow maxSubscriptionsPerConnection) <> " subscriptions"))

maxTransactionsPerConnection :: (?context :: ControllerContext) => Int
maxTransactionsPerConnection =
    case getAppConfig @DataSyncMaxTransactionsPerConnection of
        DataSyncMaxTransactionsPerConnection value -> value

maxSubscriptionsPerConnection :: (?context :: ControllerContext) => Int
maxSubscriptionsPerConnection =
    case getAppConfig @DataSyncMaxSubscriptionsPerConnection of
        DataSyncMaxSubscriptionsPerConnection value -> value

-- | Encode a JSON patch (field name -> value) into a SQL SET clause like @"col1" = $1, "col2" = $2@
-- and the accumulated 'CompilerState'.
encodePatchToSetSql :: Renamer -> ColumnTypeInfo -> HashMap Text Value -> (Text, CompilerState)
encodePatchToSetSql ren columnTypes patch =
    let pairsList = patch
            |> HashMap.toList
            |> map (\(fieldName, val) ->
                let col = ren.fieldToColumn fieldName
                in (col, lookupColumnType columnTypes col, val)
            )
        encodeSetClause st (col, colType, val) =
            let (valText, st') = typedAesonValueToSnippet colType val st in (st', quoteIdentifier col <> " = " <> valText)
        (cc, setTexts) = List.mapAccumL encodeSetClause emptyCompilerState pairsList
    in (mconcat $ List.intersperse ", " setTexts, cc)

sqlQueryWithRLSAndTransactionId ::
    ( ?context :: ControllerContext
    , Show (Id' (GetTableName CurrentUserRecord))
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
    ( ?context :: ControllerContext
    , Show (Id' (GetTableName CurrentUserRecord))
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
    ( ?context :: ControllerContext
    , Show (Id' (GetTableName CurrentUserRecord))
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
