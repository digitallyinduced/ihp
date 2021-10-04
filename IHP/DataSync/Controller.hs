{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude
import qualified Control.Exception as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson

import Data.Aeson.TH
import Data.Aeson
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications

instance (
    PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = do
        setState DataSyncReady { subscriptions = HashMap.empty }

        let maybeUserId = get #id <$> currentUserOrNothing

        let
            handleMessage :: DataSyncMessage -> IO ()
            handleMessage DataSyncQuery { query, requestId } = do
                ensureRLSEnabled (get #table query)

                let (theQuery, theParams) = compileQuery query

                result :: [[Field]] <- withRLS $ sqlQuery theQuery theParams

                sendJSON DataSyncResult { result, requestId }
            
            handleMessage CreateDataSubscription { query, requestId } = do
                tableNameRLS <- ensureRLSEnabled (get #table query)

                subscriptionId <- UUID.nextRandom

                let (theQuery, theParams) = compileQuery query

                result :: [[Field]] <- withRLS $ sqlQuery theQuery theParams

                let tableName = get #table query

                -- We need to keep track of all the ids of entities we're watching to make
                -- sure that we only send update notifications to clients that can actually
                -- access the record (e.g. if a RLS policy denies access)
                let watchedRecordIds = recordIds result

                -- Store it in IORef as an INSERT requires us to add an id
                watchedRecordIdsRef <- newIORef watchedRecordIds

                notificationStream <- ChangeNotifications.watchInsertOrUpdateTable tableNameRLS

                streamReader <- async do
                    forever do
                        notification <- MVar.takeMVar notificationStream
                        case notification of
                            ChangeNotifications.DidInsert { id } -> do
                                -- The new record could not be accessible to the current user with a RLS policy
                                -- E.g. it could be a new record in a 'projects' table, but the project belongs
                                -- to a different user, and thus the current user should not be able to see it.
                                --
                                -- To honor the RLS policies we therefore need to fetch the record as the current user
                                -- If the result set is empty, we know the record is not accesible to us
                                newRecord :: [[Field]] <- withRLS $ sqlQuery "SELECT * FROM ? WHERE id = ? LIMIT 1" (PG.Identifier (get #table query), id)

                                case headMay newRecord of
                                    Just record -> do
                                        -- Add the new record to 'watchedRecordIdsRef'
                                        -- Otherwise the updates and deletes will not be dispatched to the client
                                        modifyIORef watchedRecordIdsRef (id:)

                                        sendJSON DidInsert { subscriptionId, record }
                                    Nothing -> pure ()
                            ChangeNotifications.DidUpdate { id, changeSet } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- elem id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    sendJSON DidUpdate { subscriptionId, id, changeSet }
                            ChangeNotifications.DidDelete { id } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- elem id <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    sendJSON DidDelete { subscriptionId, id }

                    

                modifyIORef ?state (\state -> state |> modify #subscriptions (HashMap.insert subscriptionId Subscription { id = subscriptionId, tableWatcher = streamReader }))

                sendJSON DidCreateDataSubscription { subscriptionId, requestId, result }

            handleMessage DeleteDataSubscription { requestId, subscriptionId } = do
                DataSyncReady { subscriptions } <- getState
                let maybeSubscription :: Maybe Subscription = HashMap.lookup subscriptionId subscriptions
                
                -- Cancel table watcher
                case maybeSubscription of
                    Just subscription -> cancel (get #tableWatcher subscription)
                    Nothing -> pure ()

                modifyIORef ?state (\state -> state |> modify #subscriptions (HashMap.delete subscriptionId))

                sendJSON DidDeleteDataSubscription { subscriptionId, requestId }

        forever do
            message <- Aeson.decode <$> receiveData @LByteString

            case message of
                Just decodedMessage -> do
                    let requestId = get #requestId decodedMessage

                    -- Handle the messages in an async way
                    -- This increases throughput as multiple queries can be fetched
                    -- in parallel
                    async do
                        result <- Exception.try (handleMessage decodedMessage)

                        case result of
                            Left (e :: Exception.SomeException) -> do
                                Log.error (tshow e)
                                sendJSON DataSyncError { requestId }
                            Right result -> pure ()

                    pure ()
                Nothing -> sendJSON FailedToDecodeMessageError

    onClose = cleanupAllSubscriptions

cleanupAllSubscriptions :: (?state :: IORef DataSyncController) => IO ()
cleanupAllSubscriptions = do
    state <- getState
    case state of
        DataSyncReady { subscriptions } -> do
            forEach (subscriptions |> HashMap.elems) \subscription -> do
                cancel (get #tableWatcher subscription)

            pure ()
        _ -> pure ()

sendJSON payload = sendTextData (Aeson.encode payload)


queryFieldNamesToColumnNames :: SQLQuery -> SQLQuery
queryFieldNamesToColumnNames sqlQuery = sqlQuery
        |> modify #orderByClause (map convertOrderByClause)
    where
        convertOrderByClause OrderByClause { orderByColumn, orderByDirection } = OrderByClause { orderByColumn = cs (fieldNameToColumnName (cs orderByColumn)), orderByDirection }

$(deriveFromJSON defaultOptions 'DataSyncQuery)
$(deriveToJSON defaultOptions 'DataSyncResult)

instance SetField "subscriptions" DataSyncController (HashMap UUID Subscription) where
    setField subscriptions record = record { subscriptions }