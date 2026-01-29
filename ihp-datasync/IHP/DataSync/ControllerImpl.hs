{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.ControllerImpl where

import IHP.ControllerPrelude hiding (OrderByClause)
import qualified Control.Exception.Safe as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson

import Data.Aeson.TH
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Session as Hasql
import qualified Hasql.Pool as HasqlPool
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.REST.Controller (aesonValueToSnippet)
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
    , DefaultParamEncoder (PrimaryKey (GetTableName CurrentUserRecord))
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    ) => EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> IO ByteString -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> IO ()
runDataSyncController ensureRLSEnabled installTableChangeTriggers receiveData sendJSON handleCustomMessage renamer = do
    setState DataSyncReady { subscriptions = HashMap.empty, transactions = HashMap.empty }

    let handleMessage :: DataSyncMessage -> IO () = buildMessageHandler ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer


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
                                    let errorMessage = case fromException e of
                                            Just (enhancedSqlError :: EnhancedSqlError) -> enhancedSqlError.sqlError
                                            Nothing -> cs (displayException e)
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
    , DefaultParamEncoder (PrimaryKey (GetTableName CurrentUserRecord))
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    )
    => EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (DataSyncMessage -> IO ())
buildMessageHandler ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer = handleMessage
    where
            pgListener = ?context.request.pgListener
            handleMessage :: DataSyncMessage -> IO ()
            handleMessage DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (query.table)

                let theSnippet = compileQueryWithRenamer (renamer query.table) query

                rawResult :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId (wrapDynamicQuery theSnippet) dynamicRowDecoder
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

                let theSnippet = compileQueryWithRenamer (renamer query.table) query

                rawResult :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery theSnippet) dynamicRowDecoder
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
                                newRecord :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery (Snippet.sql "SELECT * FROM (" <> theSnippet <> Snippet.sql ") AS records WHERE records.id = " <> Snippet.param id <> Snippet.sql " LIMIT 1")) dynamicRowDecoder

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
                                    isRecordInResultSet :: Bool <- sqlQueryScalarWithRLS (Snippet.sql "SELECT EXISTS(SELECT * FROM (" <> theSnippet <> Snippet.sql ") AS records WHERE records.id = " <> Snippet.param id <> Snippet.sql " LIMIT 1)") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

                                    changes <- ChangeNotifications.retrieveChanges changeSet
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

                let theSnippet = compileQueryWithRenamer (renamer query.table) query

                let countSnippet = Snippet.sql "SELECT COUNT(*) FROM (" <> theSnippet <> Snippet.sql ") AS _inner"
                let countDecoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))

                count :: Int <- sqlQueryScalarWithRLS countSnippet countDecoder
                countRef <- newIORef count

                installTableChangeTriggers tableNameRLS

                let
                    callback :: ChangeNotifications.ChangeNotification -> IO ()
                    callback _ = do
                        newCount :: Int <- sqlQueryScalarWithRLS countSnippet countDecoder
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
                    Nothing -> error ("Failed to delete DataSubscription, could not find DataSubscription with id " <> tshow subscriptionId)

            handleMessage CreateRecordMessage { table, record, requestId, transactionId }  = do
                ensureRLSEnabled table

                let columns = record
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn

                let values = record
                        |> HashMap.elems
                        |> map aesonValueToSnippet

                let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
                let valueSnippets = mconcat $ List.intersperse (Snippet.sql ", ") values

                let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES (" <> valueSnippets <> Snippet.sql ") RETURNING *"

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId (wrapDynamicQuery snippet) dynamicRowDecoder

                case result of
                    [rawRecord] ->
                        let
                            record = map (renameField (renamer table)) rawRecord
                        in
                            sendJSON DidCreateRecord { requestId, record }
                    otherwise -> error "Unexpected result in CreateRecordMessage handler"

                pure ()

            handleMessage CreateRecordsMessage { table, records, requestId, transactionId }  = do
                ensureRLSEnabled table

                let columns = records
                        |> head
                        |> \case
                            Just value -> value
                            Nothing -> error "Atleast one record is required"
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn

                let values = records
                        |> map (\object ->
                                object
                                |> HashMap.elems
                                |> map aesonValueToSnippet
                            )

                let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
                let valueRowSnippets = map (\row -> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") row) <> Snippet.sql ")") values
                let valuesSnippet = mconcat $ List.intersperse (Snippet.sql ", ") valueRowSnippets

                let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES " <> valuesSnippet <> Snippet.sql " RETURNING *"

                rawRecords :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId (wrapDynamicQuery snippet) dynamicRowDecoder
                let records = map (map (renameField (renamer table))) rawRecords

                sendJSON DidCreateRecords { requestId, records }

                pure ()

            handleMessage UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                let columns = patch
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn

                let values = patch
                        |> HashMap.elems
                        |> map aesonValueToSnippet

                let keyValues = zip columns values

                let setCalls = keyValues
                        |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
                let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
                let snippet = Snippet.sql "UPDATE " <> quoteIdentifier table <> Snippet.sql " SET " <> setSnippet <> Snippet.sql " WHERE id = " <> Snippet.param id <> Snippet.sql " RETURNING *"

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId (wrapDynamicQuery snippet) dynamicRowDecoder

                case result of
                    [rawRecord] ->
                        let
                            record = map (renameField (renamer table)) rawRecord
                        in
                            sendJSON DidUpdateRecord { requestId, record }
                    otherwise -> error "Could not apply the update to the given record. Are you sure the record ID you passed is correct? If the record ID is correct, likely the row level security policy is not making the record visible to the UPDATE operation."

                pure ()

            handleMessage UpdateRecordsMessage { table, ids, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                let columns = patch
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn

                let values = patch
                        |> HashMap.elems
                        |> map aesonValueToSnippet

                let keyValues = zip columns values

                let setCalls = keyValues
                        |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
                let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
                let idSnippets = map Snippet.param ids
                let inList = mconcat $ List.intersperse (Snippet.sql ", ") idSnippets
                let snippet = Snippet.sql "UPDATE " <> quoteIdentifier table <> Snippet.sql " SET " <> setSnippet <> Snippet.sql " WHERE id IN (" <> inList <> Snippet.sql ") RETURNING *"

                rawRecords <- sqlQueryWithRLSAndTransactionId transactionId (wrapDynamicQuery snippet) dynamicRowDecoder
                let records = map (map (renameField (renamer table))) rawRecords

                sendJSON DidUpdateRecords { requestId, records }

                pure ()

            handleMessage DeleteRecordMessage { table, id, requestId, transactionId } = do
                ensureRLSEnabled table

                sqlExecWithRLSAndTransactionId transactionId (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)

                sendJSON DidDeleteRecord { requestId }

            handleMessage DeleteRecordsMessage { table, ids, requestId, transactionId } = do
                ensureRLSEnabled table

                let idSnippets = map Snippet.param ids
                let inList = mconcat $ List.intersperse (Snippet.sql ", ") idSnippets
                sqlExecWithRLSAndTransactionId transactionId (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id IN (" <> inList <> Snippet.sql ")")

                sendJSON DidDeleteRecords { requestId }

            handleMessage StartTransaction { requestId } = do
                ensureBelowTransactionLimit

                transactionId <- UUID.nextRandom

                -- Acquire a dedicated connection for this transaction from the hasql pool
                let acquireConnection = do
                        result <- HasqlPool.use ?modelContext.connectionPool (Hasql.onLibpqConnection \_ -> pure ())
                        -- We need a dedicated connection. Use Hasql.acquire for this.
                        -- For now, we create a new connection.
                        -- NOTE: In a production setting, you'd want to extract the connection string
                        -- from the pool configuration.
                        error "StartTransaction: Acquiring dedicated connections from hasql pool requires pool-level support. Use withTransaction from ModelSupport instead."

                -- For now, use a simplified approach: acquire a new hasql connection
                -- and run the transaction on it
                let globalModelContext = ?modelContext

                -- We'll use the pool to run BEGIN/COMMIT/ROLLBACK as sessions
                -- Each transaction gets a dedicated connection
                conn <- do
                    result <- Hasql.acquire [HasqlSetting.connection (HasqlConnection.string (cs (getConnectionString globalModelContext)))]
                    case result of
                        Left err -> error ("StartTransaction: Failed to acquire connection: " <> tshow err)
                        Right conn -> pure conn

                let releaseConnection conn = do
                        -- Make sure there's no pending transaction in case something went wrong
                        _ <- Hasql.use conn (Hasql.script "ROLLBACK")
                        Hasql.release conn

                Exception.bracket (pure conn) releaseConnection \connection -> do
                    transactionSignal <- MVar.newEmptyMVar

                    let ?modelContext = globalModelContext { transactionConnection = Just connection } in sqlExecWithRLS "BEGIN"

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
                DataSyncTransaction { id, close } <- findTransactionById id

                sqlExecWithRLSAndTransactionId (Just id) "ROLLBACK"
                MVar.putMVar close ()

                sendJSON DidRollbackTransaction { requestId, transactionId = id }

            handleMessage CommitTransaction { requestId, id } = do
                DataSyncTransaction { id, close } <- findTransactionById id

                sqlExecWithRLSAndTransactionId (Just id) "COMMIT"
                MVar.putMVar close ()

                sendJSON DidCommitTransaction { requestId, transactionId = id }

            handleMessage otherwise = handleCustomMessage sendJSON otherwise

changesToValue :: Renamer -> [ChangeNotifications.Change] -> Value
changesToValue renamer changes = object (map changeToPair changes)
    where
        changeToPair ChangeNotifications.Change { col, new } = (Aeson.fromText $ renamer.columnToField col) .= new

runInModelContextWithTransaction :: (?state :: IORef DataSyncController, ?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO result) -> Maybe UUID -> IO result
runInModelContextWithTransaction function (Just transactionId) = do
    let globalModelContext = ?modelContext

    DataSyncTransaction { connection } <- findTransactionById transactionId
    let
            ?modelContext = globalModelContext { transactionConnection = Just connection }
        in
            function
runInModelContextWithTransaction function Nothing = function

findTransactionById :: (?state :: IORef DataSyncController) => UUID -> IO DataSyncTransaction
findTransactionById transactionId = do
    transactions <- (.transactions) <$> readIORef ?state
    case HashMap.lookup transactionId transactions of
        Just transaction -> pure transaction
        Nothing -> error "No transaction with that id"

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
        error ("You've reached the transaction limit of " <> tshow maxTransactionsPerConnection <> " transactions")

ensureBelowSubscriptionsLimit :: (?state :: IORef DataSyncController, ?context :: ControllerContext) => IO ()
ensureBelowSubscriptionsLimit = do
    subscriptions <- (.subscriptions) <$> readIORef ?state
    let subscriptionsCount = HashMap.size subscriptions
    when (subscriptionsCount >= maxSubscriptionsPerConnection) do
        error ("You've reached the subscriptions limit of " <> tshow maxSubscriptionsPerConnection <> " subscriptions")

maxTransactionsPerConnection :: (?context :: ControllerContext) => Int
maxTransactionsPerConnection =
    case getAppConfig @DataSyncMaxTransactionsPerConnection of
        DataSyncMaxTransactionsPerConnection value -> value

maxSubscriptionsPerConnection :: (?context :: ControllerContext) => Int
maxSubscriptionsPerConnection =
    case getAppConfig @DataSyncMaxSubscriptionsPerConnection of
        DataSyncMaxSubscriptionsPerConnection value -> value

sqlQueryWithRLSAndTransactionId ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , DefaultParamEncoder userId
    , ?state :: IORef DataSyncController
    ) => Maybe UUID -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWithRLSAndTransactionId transactionId snippet decoder = runInModelContextWithTransaction (sqlQueryWithRLS snippet decoder) transactionId

sqlQueryScalarWithRLS ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , DefaultParamEncoder userId
    ) => Snippet -> Decoders.Result result -> IO result
sqlQueryScalarWithRLS snippet decoder = do
    let queryWithRLS = wrapStatementWithRLS snippet
    sqlQueryScalar queryWithRLS decoder

sqlExecWithRLSAndTransactionId ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , DefaultParamEncoder userId
    , ?state :: IORef DataSyncController
    ) => Maybe UUID -> Snippet -> IO ()
sqlExecWithRLSAndTransactionId transactionId snippet = runInModelContextWithTransaction (sqlExecWithRLS snippet) transactionId

instance SetField "subscriptions" DataSyncController (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "transactions" DataSyncController (HashMap UUID DataSyncTransaction) where
    setField transactions record = record { transactions }

atomicModifyIORef'' ref updateFn = atomicModifyIORef' ref (\value -> (updateFn value, ()))
