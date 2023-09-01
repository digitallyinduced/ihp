module IHP.DBvent (respondDbEvent, initDbEvents) where


import IHP.Prelude
import IHP.ControllerSupport
-- import qualified Network.Wai.Internal as Wai
-- import qualified Data.Binary.Builder as ByteString
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



initDbEvents :: (?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initDbEvents = do
    putContext ?applicationContext.pgListener
        


respondDbEvent :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?touchedTables::IORef (Set ByteString)) => ByteString -> IO ()
respondDbEvent eventName  = do
    touchedTables <- Set.toList <$> readIORef ?touchedTables
    putStrLn $ "Registering notification trigger for tables: " <> show touchedTables

    let headers = 
                 [ ("Cache-Control", "no-store")
                 , ("Connection", "keep-alive")
                 , (hContentType, "text/event-stream")
                 ]

    let streamBody sendChunk flush = do
                    sendChunk (ByteString.stringUtf8 "data: Connection established!\n\n") >> flush

                        
                    pgListener <- fromContext @PGListener.PGListener
                    touchedTables |> mapM (\table -> do
                        let createTriggerSql = notificationTrigger table

                        withRowLevelSecurityDisabled do
                            sqlExec createTriggerSql ()
                            pure ()

                        -- pgListener |> PGListener.subscribe (channelName table) \notification -> do
                        --     let pid = notification.notificationPid |> show |> cs
                        --     sendChunk (ByteString.stringUtf8 $ "id:" <> pid <> "\nevent:" <> cs eventName <> "\ndata: " <> cs table <> " change event triggered\n\n") >> flush
                        --     )

                        pgListener |> PGListener.subscribe (channelName table) \notification -> do
                            let pid = notification.notificationPid |> show |> cs
                            (sendChunk (ByteString.stringUtf8 $ "id:" <> pid <> "\nevent:" <> cs eventName <> "\ndata: " <> cs table <> " change event triggered\n\n") >> flush) `Exception.catch ` (\e -> putStrLn $ "Error sending chunk: " ++ show (e :: Exception.SomeException)))

                    forever do
                        threadDelay (30 * 1000000)
                        sendChunk (ByteString.stringUtf8 ": heartbeat\n\n") >> flush
                    pure ()


    respondAndExit $ responseStream 
                            status200 
                            [ ("Cache-Control", "no-store")
                            , ("Connection", "keep-alive")
                            , (hContentType, "text/event-stream")
                            ] 
                            streamBody



channelName :: ByteString -> ByteString
channelName tableName = "dbe_did_change_" <> tableName

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