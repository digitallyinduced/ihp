module IHP.DataSync.ChangeNotifications
( channelName
, ChangeNotification (..)
, Change (..)
, ChangeSet (..)
, createNotificationFunction
, installTableChangeTriggers
, makeCachedInstallTableChangeTriggers
, makeInstallTableChangeTriggers
, retrieveChanges
, installTableChangeTriggersSession
, retrieveChangesSession
) where

import IHP.Prelude
import qualified Hasql.Pool
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
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
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)
import IHP.PGVersion (defaultUuidFunction)
import IHP.Environment (Environment(..))

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

data Change
    = Change { col :: !Text, new :: !Value }
    | AppendChange { col :: !Text, append :: !Text }
    deriving (Eq, Show)

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
createNotificationFunction :: Text -> RLS.TableWithRLS -> Snippet
createNotificationFunction uuidFunction table = Snippet.sql [i|
    DO $$
    BEGIN
        -- This inner block handles concurrent installations from multiple
        -- DataSync subscriptions racing to set up triggers for the same table.
        -- CREATE OR REPLACE FUNCTION can fail with 'tuple concurrently updated'
        -- (XX000) when two connections replace the function simultaneously.
        -- The table creation below must stay OUTSIDE this block so it always
        -- runs, even when the exception handler swallows a concurrent error.
        BEGIN
            CREATE OR REPLACE FUNCTION "#{functionName}"() RETURNS TRIGGER AS $BODY$
                DECLARE
                    payload TEXT;
                    large_pg_notification_id UUID;
                    changeset JSON;
                BEGIN
                    CASE TG_OP
                    WHEN 'UPDATE' THEN
                        SELECT coalesce(json_agg(
                            CASE
                                WHEN jsonb_typeof(pre.value) = 'string'
                                    AND jsonb_typeof(post.value) = 'string'
                                    AND length(post.value #>> '{}') > length(pre.value #>> '{}')
                                    AND starts_with(post.value #>> '{}', pre.value #>> '{}')
                                THEN json_build_object(
                                    'col', pre.key,
                                    'append', substring(post.value #>> '{}' from length(pre.value #>> '{}') + 1)
                                )
                                ELSE json_build_object('col', pre.key, 'new', post.value)
                            END
                        ), '[]'::json)
                        FROM jsonb_each(to_jsonb(OLD)) AS pre
                        CROSS JOIN jsonb_each(to_jsonb(NEW)) AS post
                        WHERE pre.key = post.key AND pre.value IS DISTINCT FROM post.value
                        INTO changeset;
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
            WHEN duplicate_function THEN null;
            WHEN duplicate_object THEN null;
            WHEN SQLSTATE 'XX000' THEN null; -- tuple concurrently updated
        END;

        IF NOT EXISTS (
            SELECT FROM pg_catalog.pg_class c
            JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
            WHERE c.relname = 'large_pg_notifications'
              AND n.nspname = 'public'
        ) THEN
            CREATE UNLOGGED TABLE large_pg_notifications (
                id UUID DEFAULT #{uuidFunction}() PRIMARY KEY NOT NULL,
                payload TEXT DEFAULT NULL,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
            );
            CREATE INDEX large_pg_notifications_created_at_index ON large_pg_notifications (created_at);
        END IF;
    END; $$
|]

    where
        tableName = Text.replace "\"" "\"\"" table.tableName

        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName
        deleteTriggerName = "did_delete_" <> tableName

-- Statements

retrieveChangesStatement :: Statement.Statement UUID Text
retrieveChangesStatement = Statement.preparable
    "SELECT payload FROM large_pg_notifications WHERE id = $1 LIMIT 1"
    (Encoders.param (Encoders.nonNullable Encoders.uuid))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

-- Sessions

installTableChangeTriggersSession :: Text -> RLS.TableWithRLS -> Session.Session ()
installTableChangeTriggersSession uuidFunction table =
    Snippet.toSession (createNotificationFunction uuidFunction table) Decoders.noResult

retrieveChangesSession :: UUID -> Session.Session Text
retrieveChangesSession uuid = Session.statement uuid retrieveChangesStatement

-- IO API (thin wrappers)

installTableChangeTriggers :: Hasql.Pool.Pool -> RLS.TableWithRLS -> IO ()
installTableChangeTriggers pool tableNameRLS = do
    uuidFunction <- defaultUuidFunction
    runSession pool (installTableChangeTriggersSession uuidFunction tableNameRLS)
    pure ()

-- | In development, always re-run trigger SQL because @make db@ drops and
-- recreates the database, destroying previously installed triggers.
-- In production, cache per table to avoid unnecessary work.
makeInstallTableChangeTriggers :: Environment -> Hasql.Pool.Pool -> IO (RLS.TableWithRLS -> IO ())
makeInstallTableChangeTriggers Development pool = pure (installTableChangeTriggers pool)
makeInstallTableChangeTriggers Production pool = makeCachedInstallTableChangeTriggers pool

-- | Wraps 'installTableChangeTriggers' with a per-table cache so each table's
-- triggers are only installed once per process lifetime.
makeCachedInstallTableChangeTriggers :: Hasql.Pool.Pool -> IO (RLS.TableWithRLS -> IO ())
makeCachedInstallTableChangeTriggers pool = do
    tables <- newIORef Set.empty
    pure \tableName -> do
        shouldInstall <- atomicModifyIORef' tables \set ->
            if Set.member tableName set
                then (set, False)
                else (Set.insert tableName set, True)
        when shouldInstall do
            installTableChangeTriggers pool tableName

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: RLS.TableWithRLS -> ByteString
channelName table = "did_change_" <> (cs $ Text.replace "\"" "\"\"" table.tableName)


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
        (Change col <$> values .: "new")
          <|> (AppendChange col <$> values .: "append")
-- | The @pg_notify@ function has a payload limit of 8000 bytes. When a record update is larger than the payload size
-- we store the patch in the @large_pg_notifications@ table and pass over the id to the patch.
--
-- This function retrieves the patch from the @large_pg_notifications@ table, or directly returns the patch
-- when it's less than 8000 bytes.
retrieveChanges :: Hasql.Pool.Pool -> ChangeSet -> IO [Change]
retrieveChanges _pool InlineChangeSet { changeSet } = pure changeSet
retrieveChanges pool ExternalChangeSet { largePgNotificationId } = do
    payload <- runSession pool (retrieveChangesSession largePgNotificationId)
    case eitherDecodeStrictText payload of
        Left e -> fail e
        Right changes -> pure changes

instance ToJSON Change where
    toJSON Change { col, new } = object ["col" .= col, "new" .= new]
    toJSON AppendChange { col, append } = object ["col" .= col, "append" .= append]
$(deriveToJSON defaultOptions 'InlineChangeSet)
$(deriveToJSON defaultOptions 'DidInsert)
