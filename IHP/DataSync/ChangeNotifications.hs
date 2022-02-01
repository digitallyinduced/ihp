module IHP.DataSync.ChangeNotifications
( channelName
, ChangeNotification (..)
, Change (..)
, createNotificationFunction
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
import qualified IHP.PGListener as PGListener

data ChangeNotification
    = DidInsert { id :: !UUID }
    | DidUpdate { id :: !UUID, changeSet :: ![Change] }
    | DidDelete { id :: !UUID }
    deriving (Eq, Show)

data Change = Change
    { col :: !Text
    , new :: !Value
    } deriving (Eq, Show)

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
createNotificationFunction :: RLS.TableWithRLS -> PG.Query
createNotificationFunction table = [i|
    DO $$
    BEGIN
            CREATE FUNCTION #{functionName}() RETURNS TRIGGER AS $BODY$
                BEGIN
                    CASE TG_OP
                    WHEN 'UPDATE' THEN
                        PERFORM pg_notify(
                            '#{channelName table}',
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
                            '#{channelName table}',
                            (json_build_object('DELETE', OLD.id)::text)
                        );
                    WHEN 'INSERT' THEN
                        PERFORM pg_notify(
                            '#{channelName table}',
                            json_build_object('INSERT', NEW.id)::text
                        );
                    END CASE;
                    RETURN new;
                END;
            $BODY$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};


            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();
        EXCEPTION
            WHEN duplicate_function THEN
            null;

    END; $$
|]

    where
        tableName = get #tableName table

        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName
        deleteTriggerName = "did_delete_" <> tableName

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: RLS.TableWithRLS -> ByteString
channelName table = "did_change_" <> (cs $ get #tableName table)


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
