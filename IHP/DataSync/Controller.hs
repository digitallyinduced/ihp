{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import qualified Control.Exception as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson

import Data.Aeson.TH
import Data.Aeson
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
import qualified Data.ByteString.Builder as ByteString
import qualified IHP.PGListener as PGListener
import IHP.ApplicationContext
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Pool as Pool

import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.Parser as GraphQL
import qualified IHP.GraphQL.Compiler as GraphQL
import IHP.GraphQL.JSON ()
import qualified Data.Attoparsec.Text as Attoparsec

instance (
    PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = do
        setState DataSyncReady { subscriptions = HashMap.empty, transactions = HashMap.empty, asyncs = [] }

        ensureRLSEnabled <- makeCachedEnsureRLSEnabled
        installTableChangeTriggers <- ChangeNotifications.makeCachedInstallTableChangeTriggers

        let pgListener = ?applicationContext |> get #pgListener

        let
            handleMessage :: DataSyncMessage -> IO ()
            handleMessage DataSyncQuery { query, requestId, transactionId } = do
                ensureRLSEnabled (get #table query)

                let (theQuery, theParams) = compileQuery query

                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId theQuery theParams

                sendJSON DataSyncResult { result, requestId }
            
            handleMessage GraphQLRequest { gql, variables, requestId, transactionId } = do
                let document = case Attoparsec.parseOnly GraphQL.parseDocument gql of
                        Left parserError -> error (cs $ tshow parserError)
                        Right statements -> statements

                let [(theQuery, theParams)] = GraphQL.compileDocument variables document

                [PG.Only graphQLResult] <- sqlQueryWithRLSAndTransactionId transactionId theQuery theParams

                sendJSON GraphQLResult { graphQLResult, requestId }
            
            handleMessage CreateDataSubscription { query, requestId } = do
                ensureBelowSubscriptionsLimit

                tableNameRLS <- ensureRLSEnabled (get #table query)

                subscriptionId <- UUID.nextRandom

                let (theQuery, theParams) = compileQuery query

                result :: [[Field]] <- sqlQueryWithRLS theQuery theParams

                let tableName = get #table query

                -- We need to keep track of all the ids of entities we're watching to make
                -- sure that we only send update notifications to clients that can actually
                -- access the record (e.g. if a RLS policy denies access)
                let watchedRecordIds = recordIds result

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
                                    Just record -> do
                                        -- Add the new record to 'watchedRecordIdsRef'
                                        -- Otherwise the updates and deletes will not be dispatched to the client
                                        modifyIORef' watchedRecordIdsRef (Set.insert id)

                                        sendJSON DidInsert { subscriptionId, record }
                                    Nothing -> pure ()
                            ChangeNotifications.DidUpdate { id, changeSet } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    sendJSON DidUpdate { subscriptionId, id, changeSet = changesToValue changeSet }
                            ChangeNotifications.DidDelete { id } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    sendJSON DidDelete { subscriptionId, id }

                let subscribe = PGListener.subscribeJSON (ChangeNotifications.channelName tableNameRLS) callback pgListener
                let unsubscribe subscription = PGListener.unsubscribe subscription pgListener

                Exception.bracket subscribe unsubscribe \channelSubscription -> do
                    close <- MVar.newEmptyMVar
                    modifyIORef' ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId close))

                    sendJSON DidCreateDataSubscription { subscriptionId, requestId, result }

                    MVar.takeMVar close

            handleMessage DeleteDataSubscription { requestId, subscriptionId } = do
                DataSyncReady { subscriptions } <- getState
                let (Just closeSignalMVar) = HashMap.lookup subscriptionId subscriptions
                
                -- Cancel table watcher
                MVar.putMVar closeSignalMVar ()

                modifyIORef' ?state (\state -> state |> modify #subscriptions (HashMap.delete subscriptionId))

                sendJSON DidDeleteDataSubscription { subscriptionId, requestId }

            handleMessage CreateRecordMessage { table, record, requestId, transactionId }  = do
                ensureRLSEnabled table

                let query = "INSERT INTO ? ? VALUES ? RETURNING *"
                let columns = record
                        |> HashMap.keys
                        |> map fieldNameToColumnName

                let values = record
                        |> HashMap.elems
                        |> map aesonValueToPostgresValue

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.In values)
                
                result :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId query params

                case result of
                    [record] -> sendJSON DidCreateRecord { requestId, record }
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
                        |> map fieldNameToColumnName

                let values = records
                        |> map (\object ->
                                object
                                |> HashMap.elems
                                |> map aesonValueToPostgresValue
                            )
                        

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.Values [] values)

                records :: [[Field]] <- sqlQueryWithRLSAndTransactionId transactionId query params

                sendJSON DidCreateRecords { requestId, records }

                pure ()

            handleMessage UpdateRecordMessage { table, id, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                let columns = patch
                        |> HashMap.keys
                        |> map fieldNameToColumnName
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
                    [record] -> sendJSON DidUpdateRecord { requestId, record }
                    otherwise -> error "Unexpected result in UpdateRecordMessage handler"

                pure ()

            handleMessage UpdateRecordsMessage { table, ids, patch, requestId, transactionId } = do
                ensureRLSEnabled table

                let columns = patch
                        |> HashMap.keys
                        |> map fieldNameToColumnName
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

                records <- sqlQueryWithRLSAndTransactionId transactionId (PG.Query query) params
                
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


                let takeConnection = ?modelContext
                                    |> get #connectionPool
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

                    modifyIORef' ?state (\state -> state |> modify #transactions (HashMap.insert transactionId transaction))

                    sendJSON DidStartTransaction { requestId, transactionId }

                    MVar.takeMVar transactionSignal

                    modifyIORef' ?state (\state -> state |> modify #transactions (HashMap.delete transactionId))

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



        forever do
            message <- Aeson.eitherDecodeStrict' <$> receiveData @ByteString

            case message of
                Right decodedMessage -> do
                    let requestId = get #requestId decodedMessage

                    Exception.mask \restore -> do
                        -- Handle the messages in an async way
                        -- This increases throughput as multiple queries can be fetched
                        -- in parallel
                        handlerProcess <- async $ restore do
                            result <- Exception.try (handleMessage decodedMessage)

                            case result of
                                Left (e :: Exception.SomeException) -> do
                                    let errorMessage = case fromException e of
                                            Just (enhancedSqlError :: EnhancedSqlError) -> cs (get #sqlErrorMsg (get #sqlError enhancedSqlError))
                                            Nothing -> cs (displayException e)
                                    Log.error (tshow e)
                                    sendJSON DataSyncError { requestId, errorMessage }
                                Right result -> pure ()

                        modifyIORef' ?state (\state -> state |> modify #asyncs (handlerProcess:))
                        pure ()
                Left errorMessage -> sendJSON FailedToDecodeMessageError { errorMessage = cs errorMessage }

    onClose = cleanupAllSubscriptions

cleanupAllSubscriptions :: _ => (?state :: IORef DataSyncController, ?applicationContext :: ApplicationContext) => IO ()
cleanupAllSubscriptions = do
    state <- getState
    let pgListener = ?applicationContext |> get #pgListener

    case state of
        DataSyncReady { asyncs } -> forEach asyncs uninterruptibleCancel
        _ -> pure ()

changesToValue :: [ChangeNotifications.Change] -> Value
changesToValue changes = object (map changeToPair changes)
    where
        changeToPair ChangeNotifications.Change { col, new } = (columnNameToFieldName col) .= new

runInModelContextWithTransaction :: (?state :: IORef DataSyncController, _) => ((?modelContext :: ModelContext) => IO result) -> Maybe UUID -> IO result
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
    transactions <- get #transactions <$> readIORef ?state
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
    transactions <- get #transactions <$> readIORef ?state
    let transactionCount = HashMap.size transactions
    when (transactionCount >= maxTransactionsPerConnection) do
        error ("You've reached the transaction limit of " <> tshow maxTransactionsPerConnection <> " transactions")

ensureBelowSubscriptionsLimit :: (?state :: IORef DataSyncController, ?context :: ControllerContext) => IO ()
ensureBelowSubscriptionsLimit = do
    subscriptions <- get #subscriptions <$> readIORef ?state
    let subscriptionsCount = HashMap.size subscriptions
    when (subscriptionsCount >= maxSubscriptionsPerConnection) do
        error ("You've reached the subscriptions limit of " <> tshow maxSubscriptionsPerConnection <> " subscriptions")

maxTransactionsPerConnection :: _ => Int
maxTransactionsPerConnection = 
    case getAppConfig @DataSyncMaxTransactionsPerConnection of
        DataSyncMaxTransactionsPerConnection value -> value

maxSubscriptionsPerConnection :: _ => Int
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

$(deriveFromJSON defaultOptions 'DataSyncQuery)
$(deriveToJSON defaultOptions 'DataSyncResult)

instance ToJSON GraphQLResult where
    toJSON GraphQLResult { requestId, graphQLResult = (UndecodedJSON json) } = object [ "tag" .= ("GraphQLResult" :: Text), "requestId" .= requestId, "graphQLResult" .= ("" :: Text) ]
    toEncoding GraphQLResult { requestId, graphQLResult = (UndecodedJSON json) } = Aeson.econcat
        [ Aeson.unsafeToEncoding "{\"tag\":\"GraphQLResult\",\"requestId\":"
        , Aeson.int requestId
        , Aeson.unsafeToEncoding ",\"graphQLResult\":"
        , Aeson.unsafeToEncoding (ByteString.byteString json)
        , Aeson.unsafeToEncoding "}"
        ]

instance SetField "subscriptions" DataSyncController (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "transactions" DataSyncController (HashMap UUID DataSyncTransaction) where
    setField transactions record = record { transactions }

instance SetField "asyncs" DataSyncController [Async ()] where
    setField asyncs record = record { asyncs }