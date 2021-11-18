module IHP.DataSync.ChangeNotifications
( watchInsertOrUpdateTable
, eventName
, ChangeNotification (..)
, Change (..)
) where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import IHP.ModelSupport
import Data.String.Interpolate.IsString (i)
import Data.Aeson
import Data.Aeson.TH
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.DynamicQuery (transformColumnNamesToFieldNames)
import qualified IHP.DataSync.RowLevelSecurity as RLS

data ChangeNotification
    = DidInsert { id :: !UUID }
    | DidUpdate { id :: !UUID, changeSet :: ![Change] }
    | DidDelete { id :: !UUID }
    deriving (Eq, Show)

data Change = Change
    { col :: !Text
    , new :: !Value
    } deriving (Eq, Show)

-- | The table is wrapped as a TableWithRLS to ensure that the RLS has been checked before calling this
watchInsertOrUpdateTable :: (?modelContext :: ModelContext) => RLS.TableWithRLS -> IO (MVar.MVar ChangeNotification)
watchInsertOrUpdateTable table = do
    let tableName = table |> get #tableName
    let (listenStatement, listenArgs) = ("LISTEN ?", [PG.Identifier (eventName tableName)])

    latestNotification <- MVar.newEmptyMVar

    async do

        withDatabaseConnection \databaseConnection -> do
            PG.execute databaseConnection (PG.Query $ cs $ createNotificationFunction tableName) ()

        forever do
            notification <- withDatabaseConnection \databaseConnection -> do
                PG.execute databaseConnection listenStatement listenArgs
                PG.getNotification databaseConnection

            case decode (cs $ get #notificationData notification) of
                Just notification -> MVar.putMVar latestNotification notification

                Nothing -> pure ()

    pure latestNotification

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
createNotificationFunction :: Text -> Text
createNotificationFunction tableName = [i|
    BEGIN;

    CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
        BEGIN
            CASE TG_OP
            WHEN 'UPDATE' THEN
                PERFORM pg_notify(
                    '#{eventName tableName}',
                    json_build_object(
                      'UPDATE', NEW.id::text,
                      'CHANGESET', (
                            SELECT json_agg(row_to_json(t))
                            FROM (
                                  SELECT pre.key AS "col", post.value AS "new"
                                  FROM jsonb_each(to_jsonb(OLD)) AS pre
                                  CROSS JOIN jsonb_each(to_jsonb(NEW)) AS post
                                  WHERE pre.key = post.key AND pre.value IS DISTINCT FROM post.value
                            ) t
                      )
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
eventName :: Text -> Text
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
                pure $ DidUpdate id changeSet
            delete values = DidDelete <$> values .: "DELETE"


instance FromJSON Change where
    parseJSON = withObject "Change" $ \values -> do
        col <- values .: "col"
        new <- values .: "new"
        pure Change { .. }

$(deriveToJSON defaultOptions 'DidInsert)
$(deriveToJSON defaultOptions 'Change)
