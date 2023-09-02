-- | The IHP.DBEvent module is responsible for dispatching Server-Sent Events (SSE) with PostgreSQL notifications.
module IHP.DBEvent (respondDbEvent, initDbEvents) where

import IHP.Prelude
import IHP.ControllerSupport
import qualified Data.Set as Set
import IHP.ModelSupport ( withTableReadTracker, withRowLevelSecurityDisabled, sqlExec, trackTableRead )
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import Data.String.Interpolate.IsString ( i )
import qualified Network.Wai as Wai
import qualified Control.Exception as Exception
import qualified Data.ByteString.Builder as ByteString
import IHP.ApplicationContext ( ApplicationContext(pgListener) )
import IHP.Controller.Context ( fromContext, putContext )
import Network.HTTP.Types (status200, hConnection)
import Network.HTTP.Types.Header ( HeaderName, hContentType, hCacheControl  )
import Control.Concurrent (threadDelay)
import Database.PostgreSQL.Simple.Notification (notificationPid, Notification)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVar, writeTVar,modifyTVar')
import qualified IHP.Log as Log


-- | Initialize database events functionality. This makes the PostgreSQL listener 
-- from the application context available in the Controller context.
initDbEvents :: (?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initDbEvents = do
    putContext ?applicationContext.pgListener

-- | Required headers for SSE responses.
sseHeaders :: [(HeaderName, ByteString)]
sseHeaders = 
        [ (hCacheControl, "no-store")
        , (hConnection, "keep-alive")
        , (hContentType, "text/event-stream")
        ]


            
-- | Stream database change events to clients as Server-Sent Events.
-- This function sends updates to the client when the database tables tracked by the 
-- application change.
respondDbEvent :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?touchedTables::IORef (Set ByteString)) => ByteString -> IO ()
respondDbEvent eventName  = do
    touchedTables <- Set.toList <$> readIORef ?touchedTables
    pgListener <- fromContext @PGListener.PGListener
    -- Initialize the isActive TVar to True
    isActive <- newTVarIO True
    cleanupActions <- newTVarIO [] :: IO (TVar [IO ()])

    let addCleanupAction action = atomically $ modifyTVar' cleanupActions (action:)

    let streamBody sendChunk flush = do
             -- Notify the client that the connection is established
            initializeStream sendChunk >> flush
            -- For each touched table, create a trigger in the database and subscribe to notifications
            touchedTables 
                |> mapM \table -> do
                    let notificationCallback = handleNotificationTrigger sendChunk flush eventName table
                    createTriggerForTable table
                    subscription <- subscribeToTableChanges pgListener table notificationCallback
                    addCleanupAction $ PGListener.unsubscribe subscription pgListener           
                    
            -- Send a heartbeat to the client every 30 seconds to keep the connection alive
            sendHeartbeats sendChunk flush isActive 
                `Exception.finally` runCleanupActions cleanupActions

    -- Send the stream to the client
    respondAndExit $ Wai.responseStream status200 sseHeaders streamBody

-- | Executes all cleanup actions stored in the provided 'TVar'.
-- 
-- After executing the cleanup actions, the 'TVar' is emptied.
-- 
-- @param cleanupActions A 'TVar' containing a list of IO actions representing cleanup operations.
runCleanupActions :: TVar [IO a] -> IO ()
runCleanupActions cleanupActions = do
            actions <- atomically $ do
                a <- readTVar cleanupActions
                writeTVar cleanupActions []
                return a
            sequence_ actions

-- | Handle notifications triggered by table changes. Sends the notification data as an SSE.
handleNotificationTrigger :: (?context :: ControllerContext) => (ByteString.Builder -> IO a) -> IO () -> ByteString -> ByteString -> Notification -> IO ()
handleNotificationTrigger sendChunk flush eventName table notification = do
        let pid = notification.notificationPid |> show |> cs
        sendChunk (ByteString.stringUtf8 $
                -- Follows the SSE message spec defined on MDN
                -- https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
                        "id:" <> pid <> "\n" <>
                        "event:" <> cs eventName <> "\n" <>
                        "data: " <> cs table <> " change event triggered\n\n")
                        >> flush
                        `Exception.catch` (\e -> Log.error $ "Error sending chunk: " ++ show (e :: Exception.SomeException))
        pure ()

-- | Initializes the SSE stream with a connection established message.
initializeStream :: (ByteString.Builder -> IO ()) -> IO ()
initializeStream sendChunk = sendChunk (ByteString.stringUtf8 "data: Connection established!\n\n")

-- | Send periodic heartbeats to the client to keep the connection alive.
sendHeartbeats :: (?context :: ControllerContext) => (ByteString.Builder -> IO a) -> IO () -> TVar Bool -> IO ()
sendHeartbeats sendChunk flush isActive = do
    let heartbeatLoop = do
            active <- atomically $ readTVar isActive
            when active $ do
                threadDelay (30 * 1000000)
                handleDisconnect isActive $ do
                    sendChunk (ByteString.stringUtf8 ": heartbeat\n\n") >> flush
                heartbeatLoop

    heartbeatLoop

-- | Creates a database trigger that notifies on table changes (insert, update, delete).
createTriggerForTable :: (?modelContext::ModelContext) => ByteString -> IO ()
createTriggerForTable table = do
    let createTriggerSql = notificationTrigger table
    withRowLevelSecurityDisabled do
        sqlExec createTriggerSql ()
        pure ()

-- | Subscribes to changes in a table using the given callback for notification triggers.
subscribeToTableChanges :: PGListener.PGListener -> ByteString -> (Notification -> IO ()) -> IO PGListener.Subscription
subscribeToTableChanges pgListener table callback = PGListener.subscribe (channelName table) callback pgListener


-- A utility function to gracefully handle the client disconnect exception
handleDisconnect ::  (?context :: ControllerContext) =>  TVar Bool -> IO () -> IO ()
handleDisconnect isActive action = action `Exception.catch` \e ->
    if isDisconnectException e
        then do
            Log.info ("SSE client disconnected gracefully" :: Text)
            atomically $ writeTVar isActive False
        else Log.error $ "SSE Error: " ++ show (e :: Exception.SomeException)
    where
        isDisconnectException e = "Client closed connection prematurely" `isInfixOf` show (e :: Exception.SomeException)


-- | Generate the channel name for PostgreSQL notifications based on the table name.
channelName :: ByteString -> ByteString
channelName tableName = "dbe_did_change_" <> tableName


-- | Construct the SQL for creating triggers on table changes and sending notifications to the corresponding channel.
notificationTrigger :: ByteString -> PG.Query
notificationTrigger tableName = PG.Query [i|
        BEGIN;
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                BEGIN
                    PERFORM pg_notify('#{channelName tableName}', '');
                    RETURN new;
                END;
            $$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();
            
            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();
        
        COMMIT;
    |]
    where
        functionName = "dbe_notify_did_change_" <> tableName
        insertTriggerName = "dbe_did_insert_" <> tableName
        updateTriggerName = "dbe_did_update_" <> tableName
        deleteTriggerName = "dbe_did_delete_" <> tableName