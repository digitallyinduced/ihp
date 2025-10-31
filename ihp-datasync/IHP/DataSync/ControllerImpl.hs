{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.ControllerImpl where

import IHP.ControllerPrelude hiding (OrderByClause)
import qualified Control.Exception.Safe as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson

import Data.Aeson.TH
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.REST.Controller (aesonValueToPostgresValue)
import qualified Data.ByteString.Char8 as ByteString
import qualified IHP.PGListener as PGListener
import qualified Data.Set as Set
import qualified Data.Pool as Pool
import GHC.Conc (getNumCapabilities, ThreadId, myThreadId, atomically)
import qualified Data.HashSet as HashSet
import Control.Concurrent.QSemN
import Control.Concurrent.STM.TVar
import Control.Monad (void)
import IHP.Controller.RequestContext
import IHP.RequestVault

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
    , PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
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
                                            Just (enhancedSqlError :: EnhancedSqlError) -> cs enhancedSqlError.sqlError.sqlErrorMsg
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
    , PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    )
    => EnsureRLSEnabledFn -> InstallTableChangeTriggerFn -> SendJSONFn -> HandleCustomMessageFn -> (Text -> Renamer) -> (DataSyncMessage -> IO ())
buildMessageHandler ensureRLSEnabled installTableChangeTriggers sendJSON handleCustomMessage renamer = handleMessage
    where
            pgListener = ?context.requestContext.request.pgListener
            handleMessage :: DataSyncMessage -> IO ()
            handleMessage DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (query.table)

                let (theQuery, theParams) = compileQueryWithRenamer (renamer query.table) query

                rawResult :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId theQuery theParams
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

                let (theQuery, theParams) = compileQueryWithRenamer (renamer query.table) query

                rawResult :: [[Field]] <- sqlQueryWithRLS theQuery theParams
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
                                newRecord :: [[Field]] <- sqlQueryWithRLS ("SELECT * FROM (" <> theQuery <> ") AS records WHERE records.id = ? LIMIT 1") (theParams <> [PG.toField id])

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
                                    [(PG.Only isRecordInResultSet)] <- sqlQueryWithRLS ("SELECT EXISTS(SELECT * FROM (" <> theQuery <> ") AS records WHERE records.id = ? LIMIT 1)") (theParams <> [PG.toField id])

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

                let (theQuery, theParams) = compileQueryWithRenamer (renamer query.table) query

                let countQuery = "SELECT COUNT(*) FROM (" <> theQuery <> ") AS _inner"

                let
                    unpackResult :: [(Only Int)] -> Int
                    unpackResult [(Only value)] = value
                    unpackResult otherwise = error "DataSync.unpackResult: Expected INT, but got something else"

                count <- unpackResult <$> sqlQueryWithRLS countQuery theParams
                countRef <- newIORef count

                installTableChangeTriggers tableNameRLS

                let
                    callback :: ChangeNotifications.ChangeNotification -> IO ()
                    callback _ = do
                        newCount <- unpackResult <$> sqlQueryWithRLS countQuery theParams
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

                let query = "INSERT INTO ? ? VALUES ? RETURNING *"
                let columns = record
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn

                let values = record
                        |> HashMap.elems
                        |> map aesonValueToPostgresValue

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.In values)
                
                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId query params

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

                let query = "INSERT INTO ? ? ? RETURNING *"
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
                                |> map aesonValueToPostgresValue
                            )
                        

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.Values [] values)

                rawRecords :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId query params
                let records = map (map (renameField (renamer table))) rawRecords

                sendJSON DidCreateRecords { requestId, records }

                pure ()

            handleMessage UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                let columns = patch
                        |> HashMap.keys
                        |> map (renamer table).fieldToColumn
                        |> map PG.Identifier

                let values = patch
                        |> HashMap.elems
                        |> map aesonValueToPostgresValue

                let keyValues = zip columns values

                let setCalls = keyValues
                        |> map (\_ -> "? = ?")
                        |> ByteString.intercalate ", "
                let query = "UPDATE ? SET " <> setCalls <> " WHERE id = ? RETURNING *"

                let params = [PG.toField (PG.Identifier table)]
                        <> (join (map (\(key, value) -> [PG.toField key, value]) keyValues))
                        <> [PG.toField id]

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId (PG.Query query) params
                
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
                        |> map PG.Identifier

                let values = patch
                        |> HashMap.elems
                        |> map aesonValueToPostgresValue

                let keyValues = zip columns values

                let setCalls = keyValues
                        |> map (\_ -> "? = ?")
                        |> ByteString.intercalate ", "
                let query = "UPDATE ? SET " <> setCalls <> " WHERE id IN ? RETURNING *"

                let params = [PG.toField (PG.Identifier table)]
                        <> (join (map (\(key, value) -> [PG.toField key, value]) keyValues))
                        <> [PG.toField (PG.In ids)]

                rawRecords <- sqlQueryWithRLSAndTransactionId transactionId (PG.Query query) params
                let records = map (map (renameField (renamer table))) rawRecords
                
                sendJSON DidUpdateRecords { requestId, records }

                pure ()
            
            handleMessage DeleteRecordMessage { table, id, requestId, transactionId } = do
                ensureRLSEnabled table

                sqlExecWithRLSAndTransactionId transactionId "DELETE FROM ? WHERE id = ?" (PG.Identifier table, id)

                sendJSON DidDeleteRecord { requestId }
            
            handleMessage DeleteRecordsMessage { table, ids, requestId, transactionId } = do
                ensureRLSEnabled table

                sqlExecWithRLSAndTransactionId transactionId "DELETE FROM ? WHERE id IN ?" (PG.Identifier table, PG.In ids)

                sendJSON DidDeleteRecords { requestId }

            handleMessage StartTransaction { requestId } = do
                ensureBelowTransactionLimit

                transactionId <- UUID.nextRandom


                let takeConnection = ?modelContext.connectionPool
                                    |> Pool.takeResource

                let releaseConnection (connection, localPool) = do
                        PG.execute connection "ROLLBACK" () -- Make sure there's no pending transaction in case something went wrong
                        Pool.putResource localPool connection

                Exception.bracket takeConnection releaseConnection \(connection, localPool) -> do
                    transactionSignal <- MVar.newEmptyMVar

                    let globalModelContext = ?modelContext
                    let ?modelContext = globalModelContext { transactionConnection = Just connection } in sqlExecWithRLS "BEGIN" ()

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

                sqlExecWithRLSAndTransactionId (Just id) "ROLLBACK" ()
                MVar.putMVar close ()

                sendJSON DidRollbackTransaction { requestId, transactionId = id }

            handleMessage CommitTransaction { requestId, id } = do
                DataSyncTransaction { id, close } <- findTransactionById id

                sqlExecWithRLSAndTransactionId (Just id) "COMMIT" ()
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
    , PG.ToRow parameters
    , ?context :: ControllerContext
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , PG.ToField userId
    , FromRow result
    , ?state :: IORef DataSyncController
    ) => Maybe UUID -> PG.Query -> parameters -> IO [result]
sqlQueryWithRLSAndTransactionId transactionId theQuery theParams = runInModelContextWithTransaction (sqlQueryWithRLS theQuery theParams) transactionId

sqlExecWithRLSAndTransactionId ::
    ( ?modelContext :: ModelContext
    , PG.ToRow parameters
    , ?context :: ControllerContext
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , PG.ToField userId
    , ?state :: IORef DataSyncController
    ) => Maybe UUID -> PG.Query -> parameters -> IO Int64
sqlExecWithRLSAndTransactionId transactionId theQuery theParams = runInModelContextWithTransaction (sqlExecWithRLS theQuery theParams) transactionId

instance SetField "subscriptions" DataSyncController (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "transactions" DataSyncController (HashMap UUID DataSyncTransaction) where
    setField transactions record = record { transactions }

atomicModifyIORef'' ref updateFn = atomicModifyIORef' ref (\value -> (updateFn value, ()))
