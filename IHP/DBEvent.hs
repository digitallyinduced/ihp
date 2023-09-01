-- | The IHP.DBEvent module is responsible for dispatching Server-Sent Events (SSE) with PostgreSQL notifications.
module IHP.DBEvent (respondDbEvent, initDbEvents) where


import IHP.Prelude
import IHP.ControllerSupport
import qualified Data.Set as Set
import IHP.ModelSupport ( withTableReadTracker, withRowLevelSecurityDisabled, sqlExec, trackTableRead )
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import Data.String.Interpolate.IsString
import Network.Wai ( responseStream )
import qualified Network.Wai.Internal as Wai
import Application.Script.Prelude (runAction)
import qualified Control.Exception as Exception
import qualified Data.Binary.Builder as ByteString
import qualified Data.ByteString.Builder as ByteString
import IHP.ApplicationContext
import IHP.Controller.Context
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header ( hContentType )
import Control.Concurrent (threadDelay, forkIO)
import Database.PostgreSQL.Simple.Notification (notificationPid)


-- | Initialize database events functionality by making the PostgreSQL listener from the application context available in the Controller context.
initDbEvents :: (?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initDbEvents = do
    putContext ?applicationContext.pgListener


-- | Stream database change events to clients as Server-Sent Events.
respondDbEvent :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?touchedTables::IORef (Set ByteString)) => ByteString -> IO ()
respondDbEvent eventName  = do
    touchedTables <- Set.toList <$> readIORef ?touchedTables
    pgListener <- fromContext @PGListener.PGListener

    let streamBody sendChunk flush = do
             -- Notify the client that the connection is established
            sendChunk (ByteString.stringUtf8 "data: Connection established!\n\n") >> flush
            -- For each touched table, create a trigger in the database and subscribe to notifications
            touchedTables 
                |> mapM (\table -> do
                    let createTriggerSql = notificationTrigger table

                    -- Disable row-level security while creating the trigger
                    withRowLevelSecurityDisabled do
                        sqlExec createTriggerSql ()
                        pure ()

                    -- Subscribe to the table's change notifications
                    pgListener |> PGListener.subscribe (channelName table) \notification -> do
                        let pid = notification.notificationPid |> show |> cs
                        sendChunk (ByteString.stringUtf8 $
                                -- Follows the SSE message spec defined on MDN
                                -- https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
                                "id:" <> pid <> "\n" <>
                                "event:" <> cs eventName <> "\n" <>
                                "data: " <> cs table <> " change event triggered\n\n")
                                >> flush
                                `Exception.catch` (\e -> putStrLn $ "Error sending chunk: " ++ show (e :: Exception.SomeException)
                            )
                        )
            -- Send a heartbeat to the client every 30 seconds to keep the connection alive
            forever do
                threadDelay (30 * 1000000)
                sendChunk (ByteString.stringUtf8 $ ": heartbeat\n\n") >> flush `Exception.catch` (\e -> putStrLn $ "Error sending heartbeat: " ++ show (e :: Exception.SomeException))

    -- Send the stream to the client
    respondAndExit $
        responseStream
            status200
            [ ("Cache-Control", "no-store")
            , ("Connection", "keep-alive")
            , (hContentType, "text/event-stream")
            ]
            streamBody


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