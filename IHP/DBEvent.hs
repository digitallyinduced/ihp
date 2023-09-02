-- | The IHP.DBEvent module is responsible for dispatching Server-Sent Events (SSE) with PostgreSQL notifications.
module IHP.DBEvent (respondDbEvent, initDbEvents) where

import IHP.ApplicationContext ( ApplicationContext(pgListener) )
import IHP.Controller.Context ( fromContext, putContext )
import IHP.ControllerSupport
import IHP.ModelSupport ( withTableReadTracker, withRowLevelSecurityDisabled, sqlExec, trackTableRead )
import qualified IHP.Log as Log
import qualified IHP.PGListener as PGListener
import IHP.Prelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVar, writeTVar, modifyTVar')
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate.IsString ( i )
import Data.Text.Encoding (decodeUtf8)
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Notification (notificationPid, Notification)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, hConnection)
import Network.HTTP.Types.Header ( HeaderName, hContentType, hCacheControl )
import qualified Data.Set as Set



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


respondEventSource :: (?context::ControllerContext) => Wai.StreamingBody -> IO ()
respondEventSource streamBody = respondAndExit $ Wai.responseStream status200 sseHeaders streamBody

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
    respondEventSource streamBody


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
handleNotificationTrigger :: (?context :: ControllerContext) => (B.Builder -> IO a) -> IO () -> ByteString -> ByteString -> Notification -> IO ()
handleNotificationTrigger sendChunk flush eventName table notification = do
        -- This could have been more readable, but should be more performant this way
        let message :: B.Builder = mconcat 
                [ B.stringUtf8 "id:"
                , B.intDec (fromIntegral $ notificationPid notification)
                , B.stringUtf8 "\nevent:"
                , B.byteString eventName
                , B.stringUtf8 "\ndata: "
                , B.byteString table
                , B.stringUtf8 " change event triggered\n\n"
                ]
        sendChunk message >> flush
                        `Exception.catch` (\e -> Log.error $ "Error sending chunk: " ++ show (e :: Exception.SomeException))
        pure ()


-- | Initializes the SSE stream with a connection established message.
initializeStream :: (B.Builder -> IO ()) -> IO ()
initializeStream sendChunk = sendChunk (B.stringUtf8 "data: Connection established!\n\n")


-- | Send periodic heartbeats to the client to keep the connection alive.
sendHeartbeats :: (?context :: ControllerContext) => (B.Builder -> IO a) -> IO () -> TVar Bool -> IO ()
sendHeartbeats sendChunk flush isActive = do
    let heartbeatLoop = do
            active <- atomically $ readTVar isActive
            when active $ do
                threadDelay (30 * 1000000)
                handleDisconnect isActive $ do
                    sendChunk (B.stringUtf8 ": heartbeat\n\n") >> flush
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


-- | Constructs the SQL for creating triggers on table changes and sending notifications to the corresponding channel.
notificationTrigger :: ByteString -> PG.Query
notificationTrigger tableName = PG.Query $ BS.concat $ BL.toChunks $ B.toLazyByteString queryBuilder
  where
    -- These definitions provide naming conventions based on the table name.
    functionName       = "dbe_notify_did_change_" <> tableName
    insertTriggerName  = "dbe_did_insert_" <> tableName
    updateTriggerName  = "dbe_did_update_" <> tableName
    deleteTriggerName  = "dbe_did_delete_" <> tableName

    -- List of trigger actions and their corresponding names for easy iteration.
    triggerActions     = ["INSERT", "UPDATE", "DELETE"]
    triggerNames       = [insertTriggerName, updateTriggerName, deleteTriggerName]

    -- Generates the SQL for each trigger action.
    createTriggerSQL action triggerName = [i|
        DROP TRIGGER IF EXISTS #{triggerName} ON #{tableName};
        CREATE TRIGGER #{triggerName} AFTER #{action} ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();
    |]

    -- Construct the entire query.
    queryBuilder = mconcat
        [ B.stringUtf8 [i|BEGIN;
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                BEGIN
                    PERFORM pg_notify('#{channelName tableName}', '');
                    RETURN new;
                END;
            $$ language plpgsql;
        |]
        , mconcat $ zipWith createTriggerSQL triggerActions triggerNames
        , B.stringUtf8 "COMMIT;"
        ]

