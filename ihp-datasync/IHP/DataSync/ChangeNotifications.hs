module IHP.DataSync.ChangeNotifications
( channelName
, ChangeNotification (..)
, Change (..)
, ChangeSet (..)
, createNotificationFunction
, installTableChangeTriggers
, makeCachedInstallTableChangeTriggers
, retrieveChanges
) where

import IHP.Prelude
import qualified Hasql.Pool
import Data.String.Interpolate.IsString (i)
import Data.Aeson
import Data.Aeson.TH
import qualified IHP.DataSync.RowLevelSecurity as RLS
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import IHP.DataSync.Hasql (runHasql, runStatement)

data ChangeNotification
    = DidInsert { id :: !UUID }
    | DidUpdate { id :: !UUID, changeSet :: !ChangeSet }
    | DidUpdateLarge { id :: !UUID, payloadId :: !UUID }
    | DidDelete { id :: !UUID }
    deriving (Eq, Show)

data ChangeSet
    = InlineChangeSet { changeSet :: ![Change] } -- | When the patch fits into the 8000 bytes limit of @pg_notify@
    | ExternalChangeSet { largePgNotificationId :: !UUID } -- | The patch is over 8000 bytes, so we have stored it in the @large_pg_notifications@ table
    deriving (Eq, Show)

data Change = Change
    { col :: !Text
    , new :: !Value
    } deriving (Eq, Show)

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
createNotificationFunction :: RLS.TableWithRLS -> Snippet
createNotificationFunction table = Snippet.sql [i|
    DO $$
    BEGIN
            CREATE FUNCTION "#{functionName}"() RETURNS TRIGGER AS $BODY$
                DECLARE
                    payload TEXT;
                    large_pg_notification_id UUID;
                    changeset JSON;
                BEGIN
                    CASE TG_OP
                    WHEN 'UPDATE' THEN
                        SELECT coalesce(json_agg(row_to_json(t)), '[]'::json)
                                FROM (
                                      SELECT pre.key AS "col", post.value AS "new"
                                      FROM jsonb_each(to_jsonb(OLD)) AS pre
                                      CROSS JOIN jsonb_each(to_jsonb(NEW)) AS post
                                      WHERE pre.key = post.key AND pre.value IS DISTINCT FROM post.value
                                ) t INTO changeset;
                        payload := json_build_object(
                          'UPDATE', NEW.id::text,
                          'CHANGESET', changeset
                        )::text;
                        IF octet_length(payload) > 7800 THEN
                            INSERT INTO large_pg_notifications (payload) VALUES (changeset) RETURNING id INTO large_pg_notification_id;
                            payload := json_build_object(
                                'UPDATE', NEW.id::text,
                                'CHANGESET', large_pg_notification_id::text
                            )::text;
                            DELETE FROM large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';
                        END IF;
                        PERFORM pg_notify(
                            '#{channelName table}',
                            payload
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
            DROP TRIGGER IF EXISTS "#{insertTriggerName}" ON "#{tableName}";
            DROP TRIGGER IF EXISTS "#{updateTriggerName}" ON "#{tableName}";
            DROP TRIGGER IF EXISTS "#{deleteTriggerName}" ON "#{tableName}";


            CREATE TRIGGER "#{insertTriggerName}" AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
            CREATE TRIGGER "#{updateTriggerName}" AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
            CREATE TRIGGER "#{deleteTriggerName}" AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
        EXCEPTION
            WHEN duplicate_function THEN
            null;

        IF NOT EXISTS (
            SELECT FROM pg_catalog.pg_class c
            JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
            WHERE c.relname = 'large_pg_notifications'
              AND n.nspname = 'public'
        ) THEN
            CREATE UNLOGGED TABLE large_pg_notifications (
                id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                payload TEXT DEFAULT NULL,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
            );
            CREATE INDEX large_pg_notifications_created_at_index ON large_pg_notifications (created_at);
        END IF;
    END; $$
|]

    where
        tableName = table.tableName

        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName
        deleteTriggerName = "did_delete_" <> tableName

installTableChangeTriggers :: Hasql.Pool.Pool -> RLS.TableWithRLS -> IO ()
installTableChangeTriggers pool tableNameRLS = do
    runHasql pool (createNotificationFunction tableNameRLS) Decoders.noResult
    pure ()

makeCachedInstallTableChangeTriggers :: Hasql.Pool.Pool -> IO (RLS.TableWithRLS -> IO ())
makeCachedInstallTableChangeTriggers pool = do
    tables <- newIORef Set.empty
    pure \tableName -> do
        triggersInstalled <- Set.member tableName <$> readIORef tables

        unless triggersInstalled do
            installTableChangeTriggers pool tableName
            modifyIORef' tables (Set.insert tableName)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: RLS.TableWithRLS -> ByteString
channelName table = "did_change_" <> (cs $ table.tableName)


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

instance FromJSON ChangeSet where
    parseJSON array@(Array v) = do
            changeSet <- parseJSON array
            pure InlineChangeSet { changeSet }
    parseJSON (String id) = do
        case UUID.fromText id of
            Just largePgNotificationId -> pure ExternalChangeSet { largePgNotificationId }
            Nothing -> fail "Invalid UUID"
    parseJSON invalid = fail $ cs ("Expected Array or String for ChangeSet, got: " <> tshow invalid)

instance FromJSON Change where
    parseJSON = withObject "Change" $ \values -> do
        col <- values .: "col"
        new <- values .: "new"
        pure Change { .. }
-- | The @pg_notify@ function has a payload limit of 8000 bytes. When a record update is larger than the payload size
-- we store the patch in the @large_pg_notifications@ table and pass over the id to the patch.
--
-- This function retrieves the patch from the @large_pg_notifications@ table, or directly returns the patch
-- when it's less than 8000 bytes.
retrieveChanges :: Hasql.Pool.Pool -> ChangeSet -> IO [Change]
retrieveChanges _pool InlineChangeSet { changeSet } = pure changeSet
retrieveChanges pool ExternalChangeSet { largePgNotificationId } = do
    payload <- runStatement pool largePgNotificationId retrieveChangesStatement
    case eitherDecodeStrict' payload of
        Left e -> fail e
        Right changes -> pure changes

retrieveChangesStatement :: Statement.Statement UUID ByteString
retrieveChangesStatement = Statement.Statement
    "SELECT payload FROM large_pg_notifications WHERE id = $1 LIMIT 1"
    (Encoders.param (Encoders.nonNullable Encoders.uuid))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bytea)))
    True

$(deriveToJSON defaultOptions 'Change)
$(deriveToJSON defaultOptions 'InlineChangeSet)
$(deriveToJSON defaultOptions 'DidInsert)
