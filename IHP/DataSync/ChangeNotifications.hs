module IHP.DataSync.ChangeNotifications
( watchInsertOrUpdateTable
, eventName
, ChangeNotification (..)
) where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Control.Concurrent.Async
import IHP.ModelSupport
import Data.String.Interpolate.IsString (i)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as Text
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.DynamicQuery (transformColumnNamesToFieldNames)

data ChangeNotification
    = DidInsert { id :: UUID }
    | DidUpdate { id :: UUID, changeSet :: Value }
    | DidDelete { id :: UUID }

-- | Calls a callback every time something is inserted, updated or deleted in a given database table.
--
-- In the background this function creates a database trigger to notify this function about table changes
-- using pg_notify. When there are existing triggers, it will silently recreate them. So this will most likely
-- not fail.
--
-- This function returns a Async. Call 'cancel' on the async to stop watching the database.
--
-- __Example:__
--
-- > watchInsertOrUpdateTable "projects" do
-- >     putStrLn "Something changed in the projects table"
--
-- Now insert something into the @projects@ table. E.g. by running @make psql@ and then running @INSERT INTO projects (id, name) VALUES (DEFAULT, 'New project');@
-- You will see that @"Something changed in the projects table"@ is printed onto the screen.
--
watchInsertOrUpdateTable :: (?modelContext :: ModelContext) => ByteString -> IO (MVar.MVar ChangeNotification)
watchInsertOrUpdateTable tableName = do
    let listenStatement = "LISTEN " <> PG.Query (eventName tableName)
    
    latestNotification <- MVar.newEmptyMVar
    
    async do
        withDatabaseConnection \databaseConnection -> do
            PG.execute databaseConnection (PG.Query $ createNotificationFunction tableName) ()

        forever do
            notification <- withDatabaseConnection \databaseConnection -> do
                PG.execute databaseConnection listenStatement ()
                PG.getNotification databaseConnection

            case decode (cs $ get #notificationData notification) of
                Just notification -> MVar.putMVar latestNotification notification

                Nothing -> pure ()

    pure latestNotification

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
createNotificationFunction :: ByteString -> ByteString
createNotificationFunction tableName = [i|
    BEGIN;

    CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
        DECLARE
            updated_values jsonb;
        BEGIN
            CASE TG_OP
            WHEN 'UPDATE' THEN
                SELECT jsonb_object_agg(n.key, n.value)
                INTO updated_values
                FROM jsonb_each(to_jsonb(OLD)) o
                JOIN jsonb_each(to_jsonb(NEW)) n USING (key)
                WHERE n.value IS DISTINCT FROM o.value;

                PERFORM pg_notify(
                    '#{eventName tableName}',
                    json_build_object(
                      'UPDATE', NEW.id::text,
                      'CHANGESET', updated_values
                    )::text
                );
            WHEN 'DELETE' THEN
                PERFORM pg_notify(
                    '#{eventName tableName}',
                    (json_build_object('DELETE', OLD.id)::text)
                );
            WHEN 'INSERT' THEN
                PERFORM pg_notify(
                    '#{eventName tableName}',
                    json_build_object('INSERT', NEW.id)::text
                );
            END CASE;
            RETURN new;
        END;
    $$ language plpgsql;
    
    DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
    DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
    DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};


    CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();
    CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();
    CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

    COMMIT;

|]

    where
        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName
        deleteTriggerName = "did_delete_" <> tableName

-- | Returns the event name of the event that the pg notify trigger dispatches
eventName :: ByteString -> ByteString
eventName tableName = "did_change_" <> tableName


instance FromJSON ChangeNotification where
    parseJSON = withObject "ChangeNotification" $ \values -> insert values <|> update values <|> delete values
        where
            insert values = do
                id <- values .: "INSERT"
                pure DidInsert { id }
            update values = do
                id <- values .: "UPDATE"
                changeSet <- values .: "CHANGESET"
                pure $ DidUpdate id (transformColumnNamesToFieldNames changeSet)
            delete values = DidDelete <$> values .: "DELETE"

$(deriveToJSON defaultOptions 'DidInsert)
