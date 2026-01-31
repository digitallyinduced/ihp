{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.ControllerImpl where

import IHP.ControllerPrelude hiding (OrderByClause, sqlQuery, sqlExec, sqlQueryScalar)
import qualified Control.Exception.Safe as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson

import Data.Aeson.TH
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Pool
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession, runSessionOnConnection, withPoolConnection)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.TypedEncoder (makeCachedColumnTypeLookup, typedAesonValueToSnippet)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import qualified Data.ByteString.Char8 as ByteString
import qualified IHP.PGListener as PGListener
import qualified Data.Set as Set
import GHC.Conc (getNumCapabilities, ThreadId, myThreadId, atomically)
import qualified Data.HashSet as HashSet
import Control.Concurrent.QSemN
import Control.Concurrent.STM.TVar
import Control.Monad (void)
import IHP.RequestVault
import qualified Data.List as List

$(deriveFromJSON defaultOptions ''DataSyncMessage)
$(deriveToJSON defaultOptions 'DataSyncResult)

type EnsureRLSEnabledFn = Text -> IO TableWithRLS
type InstallTableChangeTriggerFn = TableWithRLS -> IO ()
type SendJSONFn = DataSyncResponse -> IO ()
type HandleCustomMessageFn = (DataSyncResponse -> IO ()) -> DataSyncMessage -> IO ()

runDataSyncController ::
    ( HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> IO ByteString -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> IO ()
runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers receiveData sendJSON handleCustomMessage renamer = do
    setState DataSyncReady { subscriptions = HashMap.empty, transactions = HashMap.empty }

    columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
    let handleMessage :: DataSyncMessage -> IO () = buildMessageHandler hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup


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
    , ?state :: IORef DataSyncController
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    )
    => Hasql.Pool.Pool -> EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (Text -> IO ColumnTypeMap) -> (DataSyncMessage -> IO ())
buildMessageHandler hasqlPool ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer columnTypeLookup = handleMessage
    where
            pgListener = ?context.request.pgListener
            handleMessage :: DataSyncMessage -> IO ()
            handleMessage DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (query.table)

                columnTypes <- columnTypeLookup query.table
                let theSnippet = compileQueryTyped (renamer query.table) columnTypes query

                rawResult :: [[Field]] <- sqlQueryWithRLSAndTransactionId hasqlPool transactionId (wrapDynamicQuery theSnippet) dynamicRowDecoder
                let result = map (map (renameField (renamer query.table))) rawResult

                sendJSON DataSyncResult { result, requestId }

            handleMessage CreateDataSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled (query.table)

                subscriptionId <- UUID.nextRandom

                -- Allocate the close handle as early as possible
                -- to make DeleteDataSubscription calls succeed even when the DataSubscription is
                -- not fully set up yet
                close <- MVar.newEmptyMVar
                atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId close))

                columnTypes <- columnTypeLookup query.table
                let theSnippet = compileQueryTyped (renamer query.table) columnTypes query

                rawResult :: [[Field]] <- sqlQueryWithRLS hasqlPool (wrapDynamicQuery theSnippet) dynamicRowDecoder
                let result = map (map (renameField (renamer query.table))) rawResult

                let tableName = query.table

                -- We need to keep track of all the ids of entities we're watching to make
                -- sure that we only send update notifications to clients that can actually
                -- access the record (e.g. if a RLS policy denies access)
                let watchedRecordIds = recordIds rawResult

                -- Store it in IORef as an INSERT requires us to add an id
                watchedRecordIdsRef <- newIORef (Set.fromList watchedRecordIds)

                -- Make sure the database triggers are there
                installTableChangeTriggers tableNameRLS

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
                                newRecord :: [[Field]] <- sqlQueryWithRLS hasqlPool (wrapDynamicQuery (Snippet.sql "SELECT * FROM (" <> theSnippet <> Snippet.sql ") AS records WHERE records.id = " <> Snippet.param id <> Snippet.sql " LIMIT 1")) dynamicRowDecoder

                                case headMay newRecord of
                                    Just rawRecord -> do
                                        -- Add the new record to 'watchedRecordIdsRef'
                                        -- Otherwise the updates and deletes will not be dispatched to the client
                                        modifyIORef' watchedRecordIdsRef (Set.insert id)

                                        let record = map (renameField (renamer tableName)) rawRecord
                                        sendJSON DidInsert { subscriptionId, record }
                                    Nothing -> pure ()
                            ChangeNotifications.DidUpdate { id, changeSet } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    -- The updated record could not be part of the query result set anymore
                                    -- E.g. if it's not matched anymore by the WHERE condition after the update
                                    isRecordInResultSet :: Bool <- sqlQueryScalarWithRLS hasqlPool (Snippet.sql "SELECT EXISTS(SELECT * FROM (" <> theSnippet <> Snippet.sql ") AS records WHERE records.id = " <> Snippet.param id <> Snippet.sql " LIMIT 1)") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

                                    changes <- ChangeNotifications.retrieveChanges hasqlPool changeSet
                                    if isRecordInResultSet
                                        then sendJSON DidUpdate { subscriptionId, id, changeSet = changesToValue (renamer tableName) changes }
                                        else sendJSON DidDelete { subscriptionId, id }
                            ChangeNotifications.DidUpdateLarge { id, payloadId } -> do
                                isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    isRecordInResultSet :: Bool <- sqlQueryScalarWithRLS hasqlPool (Snippet.sql "SELECT EXISTS(SELECT * FROM (" <> theSnippet <> Snippet.sql ") AS records WHERE records.id = " <> Snippet.param id <> Snippet.sql " LIMIT 1)") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                                    changes <- ChangeNotifications.retrieveChanges hasqlPool (ChangeNotifications.ExternalChangeSet { largePgNotificationId = payloadId })
                                    if isRecordInResultSet
                                        then sendJSON DidUpdate { subscriptionId, id, changeSet = changesToValue (renamer tableName) changes }
                                        else sendJSON DidDelete { subscriptionId, id }
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

            handleMessage CreateCountSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled query.table

                subscriptionId <- UUID.nextRandom

                -- Allocate the close handle as early as possible
                -- to make DeleteDataSubscription calls succeed even when the CountSubscription is
                -- not fully set up yet
                close <- MVar.newEmptyMVar
                atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId close))

                columnTypes <- columnTypeLookup query.table
                let theSnippet = compileQueryTyped (renamer query.table) columnTypes query

                let countSnippet = Snippet.sql "SELECT COUNT(*) FROM (" <> theSnippet <> Snippet.sql ") AS _inner"
                let countDecoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))

                count :: Int <- sqlQueryScalarWithRLS hasqlPool countSnippet countDecoder
                countRef <- newIORef count

                installTableChangeTriggers tableNameRLS

                let
                    callback :: ChangeNotifications.ChangeNotification -> IO ()
                    callback _ = do
                        newCount :: Int <- sqlQueryScalarWithRLS hasqlPool countSnippet countDecoder
                        lastCount <- readIORef countRef

                        when (newCount /= count) (sendJSON DidChangeCount { subscriptionId, count = newCount })

                let subscribe = PGListener.subscribeJSON (ChangeNotifications.channelName tableNameRLS) callback pgListener
                let unsubscribe subscription = PGListener.unsubscribe subscription pgListener

                Exception.bracket subscribe unsubscribe \channelSubscription -> do
                    sendJSON DidCreateCountSubscription { subscriptionId, requestId, count }

                    MVar.takeMVar close

            handleMessage DeleteDataSubscription { requestId, subscriptionId } = do
                DataSyncReady { subscriptions } <- getState
                case HashMap.lookup subscriptionId subscriptions of
                    Just closeSignalMVar -> do
                        -- Cancel table watcher
                        MVar.putMVar closeSignalMVar ()

                        atomicModifyIORef'' ?state (\state -> state |> modify #subscriptions (HashMap.delete subscriptionId))

                        sendJSON DidDeleteDataSubscription { subscriptionId, requestId }
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "Failed to delete DataSubscription, could not find DataSubscription with id " <> tshow subscriptionId }

            handleMessage CreateRecordMessage { table, record, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let pairs = record
                        |> HashMap.toList
                        |> map (\(fieldName, val) ->
                            let col = (renamer table).fieldToColumn fieldName
                            in (col, typedAesonValueToSnippet (HashMap.lookup col columnTypes) val)
                        )

                let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map (quoteIdentifier . fst) pairs)
                let valueSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map snd pairs)

                let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES (" <> valueSnippets <> Snippet.sql ") RETURNING *"

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId (wrapDynamicQuery snippet) dynamicRowDecoder

                case result of
                    [rawRecord] ->
                        let
                            record = map (renameField (renamer table)) rawRecord
                        in
                            sendJSON DidCreateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Unexpected result in CreateRecordMessage handler" }

                pure ()

            handleMessage CreateRecordsMessage { table, records, requestId, transactionId }  = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                case head records of
                    Nothing -> sendJSON DataSyncError { requestId, errorMessage = "At least one record is required" }
                    Just firstRecord -> do
                        let fieldNames = HashMap.keys firstRecord
                        let columns = map (renamer table).fieldToColumn fieldNames

                        let values = records
                                |> map (\object ->
                                        zip fieldNames columns
                                        |> map (\(fieldName, col) ->
                                            let val = fromMaybe Aeson.Null (HashMap.lookup fieldName object)
                                            in typedAesonValueToSnippet (HashMap.lookup col columnTypes) val
                                        )
                                    )

                        let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
                        let valueRowSnippets = map (\row -> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") row) <> Snippet.sql ")") values
                        let valuesSnippet = mconcat $ List.intersperse (Snippet.sql ", ") valueRowSnippets

                        let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES " <> valuesSnippet <> Snippet.sql " RETURNING *"

                        rawRecords :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId (wrapDynamicQuery snippet) dynamicRowDecoder
                        let records = map (map (renameField (renamer table))) rawRecords

                        sendJSON DidCreateRecords { requestId, records }

                        pure ()

            handleMessage UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let keyValues = patch
                        |> HashMap.toList
                        |> map (\(fieldName, val) ->
                            let col = (renamer table).fieldToColumn fieldName
                            in (col, typedAesonValueToSnippet (HashMap.lookup col columnTypes) val)
                        )

                let setCalls = keyValues
                        |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
                let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
                let snippet = Snippet.sql "UPDATE " <> quoteIdentifier table <> Snippet.sql " SET " <> setSnippet <> Snippet.sql " WHERE id = " <> Snippet.param id <> Snippet.sql " RETURNING *"

                result :: [[Field]] <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId (wrapDynamicQuery snippet) dynamicRowDecoder

                case result of
                    [rawRecord] ->
                        let
                            record = map (renameField (renamer table)) rawRecord
                        in
                            sendJSON DidUpdateRecord { requestId, record }
                    otherwise -> sendJSON DataSyncError { requestId, errorMessage = "Could not apply the update to the given record. Are you sure the record ID you passed is correct? If the record ID is correct, likely the row level security policy is not making the record visible to the UPDATE operation." }

                pure ()

            handleMessage UpdateRecordsMessage { table, ids, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                columnTypes <- columnTypeLookup table

                let keyValues = patch
                        |> HashMap.toList
                        |> map (\(fieldName, val) ->
                            let col = (renamer table).fieldToColumn fieldName
                            in (col, typedAesonValueToSnippet (HashMap.lookup col columnTypes) val)
                        )

                let setCalls = keyValues
                        |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
                let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
                let idSnippets = map Snippet.param ids
                let inList = mconcat $ List.intersperse (Snippet.sql ", ") idSnippets
                let snippet = Snippet.sql "UPDATE " <> quoteIdentifier table <> Snippet.sql " SET " <> setSnippet <> Snippet.sql " WHERE id IN (" <> inList <> Snippet.sql ") RETURNING *"

                rawRecords <- sqlQueryWriteWithRLSAndTransactionId hasqlPool transactionId (wrapDynamicQuery snippet) dynamicRowDecoder
                let records = map (map (renameField (renamer table))) rawRecords

                sendJSON DidUpdateRecords { requestId, records }

                pure ()

            handleMessage DeleteRecordMessage { table, id, requestId, transactionId } = do
                ensureRLSEnabled table

                sqlExecWithRLSAndTransactionId hasqlPool transactionId (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)

                sendJSON DidDeleteRecord { requestId }

            handleMessage DeleteRecordsMessage { table, ids, requestId, transactionId } = do
                ensureRLSEnabled table

                let idSnippets = map Snippet.param ids
                let inList = mconcat $ List.intersperse (Snippet.sql ", ") idSnippets
                sqlExecWithRLSAndTransactionId hasqlPool transactionId (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id IN (" <> inList <> Snippet.sql ")")

                sendJSON DidDeleteRecords { requestId }

            handleMessage StartTransaction { requestId } = do
                ensureBelowTransactionLimit

                transactionId <- UUID.nextRandom

                withPoolConnection hasqlPool \connection -> do
                    transactionSignal <- MVar.newEmptyMVar

                    runSessionOnConnection connection $ do
                        Session.sql "BEGIN"
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

            handleMessage RollbackTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.sql "ROLLBACK")
                MVar.putMVar close ()

                sendJSON DidRollbackTransaction { requestId, transactionId = id }

            handleMessage CommitTransaction { requestId, id } = do
                DataSyncTransaction { id, close, connection } <- findTransactionById id

                runSessionOnConnection connection (Session.sql "COMMIT")
                MVar.putMVar close ()

                sendJSON DidCommitTransaction { requestId, transactionId = id }

            handleMessage otherwise = handleCustomMessage sendJSON otherwise

changesToValue :: Renamer -> [ChangeNotifications.Change] -> Value
changesToValue renamer changes = object (map changeToPair changes)
    where
        changeToPair ChangeNotifications.Change { col, new } = (Aeson.fromText $ renamer.columnToField col) .= new

findTransactionById :: (?state :: IORef DataSyncController) => UUID -> IO DataSyncTransaction
findTransactionById transactionId = do
    transactions <- (.transactions) <$> readIORef ?state
    case HashMap.lookup transactionId transactions of
        Just transaction -> pure transaction
        Nothing -> Exception.throwIO (userError ("No transaction with id " <> cs (tshow transactionId)))

-- | Allow max 10 concurrent transactions per connection to avoid running out of database connections
--
-- Each transaction removes a database connection from the connection pool. If we don't limit the transactions,
-- a single user could take down the application by starting more than 'IHP.FrameworkConfig.DBPoolMaxConnections'
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

sqlQueryWithRLSAndTransactionId ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWithRLSAndTransactionId _pool (Just transactionId) snippet decoder = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (DynSession.dynamicallyParameterizedStatement snippet decoder True)
sqlQueryWithRLSAndTransactionId pool Nothing snippet decoder = runSession pool (sqlQueryWithRLSSession snippet decoder)

-- | Like 'sqlQueryWithRLSAndTransactionId', but uses a write transaction when no transaction ID is provided.
--
-- Use this for INSERT, UPDATE, or DELETE statements with RETURNING that need
-- to return results (e.g. wrapped with 'wrapDynamicQuery').
sqlQueryWriteWithRLSAndTransactionId ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWriteWithRLSAndTransactionId _pool (Just transactionId) snippet decoder = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (DynSession.dynamicallyParameterizedStatement snippet decoder True)
sqlQueryWriteWithRLSAndTransactionId pool Nothing snippet decoder = runSession pool (sqlQueryWriteWithRLSSession snippet decoder)

sqlExecWithRLSAndTransactionId ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?state :: IORef DataSyncController
    ) => Hasql.Pool.Pool -> Maybe UUID -> Snippet -> IO ()
sqlExecWithRLSAndTransactionId _pool (Just transactionId) snippet = do
    -- RLS role and user id were already set when the transaction was started
    DataSyncTransaction { connection } <- findTransactionById transactionId
    runSessionOnConnection connection
        (DynSession.dynamicallyParameterizedStatement snippet Decoders.noResult True)
sqlExecWithRLSAndTransactionId pool Nothing snippet = runSession pool (sqlExecWithRLSSession snippet)


instance SetField "subscriptions" DataSyncController (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "transactions" DataSyncController (HashMap UUID DataSyncTransaction) where
    setField transactions record = record { transactions }

atomicModifyIORef'' ref updateFn = atomicModifyIORef' ref (\value -> (updateFn value, ()))
