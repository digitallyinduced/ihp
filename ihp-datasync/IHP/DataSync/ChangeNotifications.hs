module IHP.DataSync.ChangeNotifications
( channelName
, globalInvalidationChannel
, relationInvalidationChannel
, invalidationChannelsForTable
, InvalidationPlan (..)
, resolveInvalidationPlan
, makeInstallInvalidationPlan
, ChangeNotification (..)
, Change (..)
, ChangeSet (..)
, createNotificationFunction
, installTableChangeTriggers
, installGlobalInvalidationTriggers
, makeCachedInstallGlobalInvalidationTriggers
, makeCachedInstallTableChangeTriggers
, makeInstallTableChangeTriggers
, retrieveChanges
, installTableChangeTriggersSession
, installGlobalInvalidationTriggersSession
, retrieveChangesSession
) where

import IHP.Prelude
import qualified Hasql.Pool
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.TH
import qualified IHP.DataSync.RowLevelSecurity as RLS
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar
import qualified Data.UUID as UUID
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)
import IHP.PGVersion (defaultUuidFunction)
import IHP.Environment (Environment(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)
import qualified Data.List as List
import qualified Data.Set as Set

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

data InvalidationPlan = InvalidationPlan
    { channels :: !(Set.Set ByteString)
    , relationOids :: ![Int64]
    , missingRelationOids :: ![Int64]
    , requiresGlobalFallback :: !Bool
    } deriving (Eq, Show)

data InvalidationInstallState = InvalidationInstallState
    { globalFingerprint :: !(Maybe Text)
    , functionReconciled :: !Bool
    }

-- | Returns the sql code to set up a database trigger. Mainly used by 'watchInsertOrUpdateTable'.
--
-- The function body is always updated via @CREATE OR REPLACE FUNCTION@ (no table lock needed).
-- The trigger DDL (@CREATE TRIGGER@) requires @ShareRowExclusiveLock@ on the table,
-- which conflicts with @RowExclusiveLock@ from writers (INSERT\/UPDATE\/DELETE\/COPY FROM).
-- It is only executed when the triggers don't already exist, with a short @lock_timeout@
-- to fail fast rather than block behind long-running writers.
createNotificationFunction :: Text -> RLS.TableWithRLS -> Text
createNotificationFunction uuidFunction table = [i|
    DO $$
    BEGIN
        -- Serialize concurrent trigger installation for the same table across
        -- multiple server processes. Without this, concurrent CREATE OR REPLACE
        -- FUNCTION calls cause "tuple concurrently updated" errors (XX000).
        PERFORM pg_advisory_xact_lock(hashtext('#{functionName}'));

        -- Always update the function body. CREATE OR REPLACE FUNCTION only locks
        -- the function in pg_proc, not the table, so this is safe to run
        -- unconditionally and ensures the function stays up to date.
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

        -- Only install triggers if they don't already exist. CREATE TRIGGER
        -- takes ShareRowExclusiveLock on the table, which conflicts with
        -- RowExclusiveLock from INSERT/UPDATE/DELETE. Skipping this when
        -- triggers are already in place avoids blocking behind long-running
        -- writers or COPY FROM.
        --
        -- When triggers ARE missing (first install, new table), use a short
        -- lock_timeout so we fail fast rather than blocking writers
        -- indefinitely. The Haskell-side MVar cache removes its entry on
        -- failure, so the next WebSocket connection will retry.
        IF NOT EXISTS (
            SELECT 1 FROM pg_trigger WHERE tgname = '#{insertTriggerName}'
                AND tgrelid = '#{tableName}'::regclass
        ) THEN
            -- Use a short lock_timeout so we fail fast if a long-running
            -- writer holds RowExclusiveLock on the table (which conflicts
            -- with CREATE TRIGGER's ShareRowExclusiveLock). The error
            -- propagates to Haskell where the MVar cache removes its entry,
            -- allowing the next WebSocket connection to retry.
            SET LOCAL lock_timeout = '5s';
            BEGIN
                CREATE TRIGGER "#{insertTriggerName}" AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
                CREATE TRIGGER "#{updateTriggerName}" AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
                CREATE TRIGGER "#{deleteTriggerName}" AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE "#{functionName}"();
            EXCEPTION
                WHEN duplicate_object THEN null;
            END;
        END IF;

        BEGIN
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
        EXCEPTION
            WHEN duplicate_table THEN null;
        END;
    END; $$
|]

    where
        tableName = Text.replace "\"" "\"\"" table.tableName

        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName
        deleteTriggerName = "did_delete_" <> tableName

-- | Creates the payload-free trigger function. Relation triggers are installed
-- separately so each table lock is released before attempting the next table.
createGlobalInvalidationFunction :: Text
createGlobalInvalidationFunction = [i|
    DO $$
    BEGIN
        PERFORM pg_advisory_xact_lock(hashtext('#{functionName}'));

        CREATE OR REPLACE FUNCTION public."#{functionName}"() RETURNS TRIGGER AS $BODY$
        DECLARE
            affected_relation OID;
        BEGIN
            -- Direct writes to an inherited/partition child use the child's
            -- TG_RELID. Also notify every ancestor so a subscription querying
            -- the parent relation cannot miss that change.
            FOR affected_relation IN
                WITH RECURSIVE affected_relations(oid) AS (
                    SELECT TG_RELID
                    UNION
                    SELECT inheritance.inhparent
                    FROM pg_catalog.pg_inherits inheritance
                    JOIN affected_relations ON inheritance.inhrelid = affected_relations.oid
                )
                SELECT oid FROM affected_relations
            LOOP
                PERFORM pg_notify('#{relationInvalidationChannelPrefix}' || affected_relation::text, '');
            END LOOP;
            PERFORM pg_notify('#{globalInvalidationChannel}', '');
            RETURN NULL;
        END;
        $BODY$ language plpgsql;
    END; $$
|]
    where
        functionName = "ihp_datasync_notify_invalidation"

-- | Install the global statement trigger on one relation OID. The catalog row
-- is revalidated inside the block because a migration may have dropped it
-- after the initial scan. Each invocation is a separate implicit transaction,
-- avoiding an accumulating schema-wide set of ShareRowExclusive locks.
createGlobalInvalidationTrigger :: Int64 -> Text
createGlobalInvalidationTrigger relationOid = [i|
    DO $$
    DECLARE
        relation_schema TEXT;
        relation_name TEXT;
    BEGIN
        PERFORM pg_advisory_xact_lock(hashtext('#{triggerName}:' || #{relationOid}::text));

        SELECT n.nspname, c.relname
        INTO relation_schema, relation_name
        FROM pg_catalog.pg_class c
        JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
        WHERE c.oid = #{relationOid}::oid
          AND c.relkind IN ('r', 'p')
          AND n.nspname <> 'information_schema'
          AND n.nspname NOT LIKE 'pg\\_%' ESCAPE '\\'
          AND has_table_privilege(c.oid, 'TRIGGER')
          AND NOT EXISTS (
              SELECT 1 FROM pg_catalog.pg_depend extension_dependency
              WHERE extension_dependency.classid = 'pg_catalog.pg_class'::regclass
                AND extension_dependency.objid = c.oid
                AND extension_dependency.deptype = 'e'
          );

        IF NOT FOUND THEN
            RETURN;
        END IF;

        IF NOT EXISTS (
            SELECT 1
            FROM pg_catalog.pg_trigger installed_trigger
            WHERE installed_trigger.tgname = '#{triggerName}'
              AND installed_trigger.tgrelid = #{relationOid}::oid
              AND NOT installed_trigger.tgisinternal
              AND installed_trigger.tgfoid = 'public.#{functionName}()'::regprocedure
              AND installed_trigger.tgtype = 60
              AND installed_trigger.tgenabled IN ('O', 'A')
        ) THEN
            IF EXISTS (
                SELECT 1 FROM pg_catalog.pg_trigger conflicting_trigger
                WHERE conflicting_trigger.tgname = '#{triggerName}'
                  AND conflicting_trigger.tgrelid = #{relationOid}::oid
                  AND NOT conflicting_trigger.tgisinternal
            ) THEN
                RAISE EXCEPTION 'Incompatible pre-existing trigger % on %.%',
                    '#{triggerName}', relation_schema, relation_name;
            END IF;
            PERFORM set_config('lock_timeout', '5s', true);
            EXECUTE format(
                'CREATE TRIGGER %I AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON %I.%I FOR EACH STATEMENT EXECUTE PROCEDURE public.%I()',
                '#{triggerName}',
                relation_schema,
                relation_name,
                '#{functionName}'
            );
        END IF;
    END; $$
|]
    where
        functionName = "ihp_datasync_notify_invalidation"
        triggerName = "ihp_datasync_invalidate"

-- Statements

retrieveChangesStatement :: Statement.Statement UUID Text
retrieveChangesStatement = Statement.preparable
    "SELECT payload FROM large_pg_notifications WHERE id = $1 LIMIT 1"
    (Encoders.param (Encoders.nonNullable Encoders.uuid))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

eligibleGlobalInvalidationRelationsStatement :: Statement.Statement () [Int64]
eligibleGlobalInvalidationRelationsStatement = Statement.preparable
    "SELECT c.oid::bigint FROM pg_catalog.pg_class c JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE c.relkind IN ('r', 'p') AND n.nspname <> 'information_schema' AND n.nspname NOT LIKE 'pg\\_%' ESCAPE '\\' AND has_table_privilege(c.oid, 'TRIGGER') AND NOT EXISTS (SELECT 1 FROM pg_catalog.pg_depend extension_dependency WHERE extension_dependency.classid = 'pg_catalog.pg_class'::regclass AND extension_dependency.objid = c.oid AND extension_dependency.deptype = 'e') ORDER BY c.oid"
    Encoders.noParams
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8)))

globalInvalidationSchemaFingerprintStatement :: Statement.Statement () Text
globalInvalidationSchemaFingerprintStatement = Statement.preparable
    "SELECT COALESCE(string_agg(c.oid::text || ':' || CASE WHEN EXISTS (SELECT 1 FROM pg_catalog.pg_trigger installed_trigger WHERE installed_trigger.tgrelid = c.oid AND installed_trigger.tgname = 'ihp_datasync_invalidate' AND NOT installed_trigger.tgisinternal AND installed_trigger.tgfoid = to_regprocedure('public.ihp_datasync_notify_invalidation()') AND installed_trigger.tgtype = 60 AND installed_trigger.tgenabled IN ('O', 'A')) THEN '1' ELSE '0' END, ',' ORDER BY c.oid), '') FROM pg_catalog.pg_class c JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE c.relkind IN ('r', 'p') AND n.nspname <> 'information_schema' AND n.nspname NOT LIKE 'pg\\_%' ESCAPE '\\' AND has_table_privilege(c.oid, 'TRIGGER') AND NOT EXISTS (SELECT 1 FROM pg_catalog.pg_depend extension_dependency WHERE extension_dependency.classid = 'pg_catalog.pg_class'::regclass AND extension_dependency.objid = c.oid AND extension_dependency.deptype = 'e')"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

invalidationDependenciesStatement :: Statement.Statement Text [(Int64, Bool, Bool, Bool, Bool)]
invalidationDependenciesStatement = Statement.preparable
    "WITH RECURSIVE selected_table AS (SELECT $1::regclass::oid AS relation_oid), policies AS (SELECT p.oid FROM pg_catalog.pg_policy p WHERE p.polrelid = (SELECT relation_oid FROM selected_table) AND p.polcmd IN ('r', '*')), policy_dependencies AS (SELECT dependency.* FROM policies JOIN pg_catalog.pg_depend dependency ON dependency.classid = 'pg_catalog.pg_policy'::regclass AND dependency.objid = policies.oid), root_dependencies AS (SELECT relation_oid FROM selected_table UNION SELECT dependency.refobjid FROM policy_dependencies dependency WHERE dependency.refclassid = 'pg_catalog.pg_class'::regclass), expanded_dependencies(relation_oid, is_root) AS (SELECT relation_oid, true FROM root_dependencies UNION SELECT inheritance.inhrelid, false FROM pg_catalog.pg_inherits inheritance JOIN expanded_dependencies ON inheritance.inhparent = expanded_dependencies.relation_oid), relation_dependencies AS (SELECT relation_oid, bool_or(is_root) AS is_root FROM expanded_dependencies GROUP BY relation_oid), opaque_policy AS (SELECT (EXISTS (SELECT 1 FROM policy_dependencies dependency JOIN pg_catalog.pg_proc referenced_function ON dependency.refclassid = 'pg_catalog.pg_proc'::regclass AND dependency.refobjid = referenced_function.oid JOIN pg_catalog.pg_namespace function_namespace ON function_namespace.oid = referenced_function.pronamespace WHERE function_namespace.nspname <> 'pg_catalog' AND NOT (function_namespace.nspname = 'public' AND referenced_function.proname = 'ihp_user_id' AND referenced_function.pronargs = 0 AND referenced_function.prolang = (SELECT oid FROM pg_catalog.pg_language WHERE lanname = 'sql') AND trim(trailing ';' from lower(regexp_replace(referenced_function.prosrc, '[[:space:]]+', '', 'g'))) = 'selectnullif(current_setting(''rls.ihp_user_id''),'''')::uuid')) OR EXISTS (SELECT 1 FROM policy_dependencies dependency JOIN pg_catalog.pg_class referenced_relation ON dependency.refclassid = 'pg_catalog.pg_class'::regclass AND dependency.refobjid = referenced_relation.oid WHERE referenced_relation.relkind = 'v') OR EXISTS (SELECT 1 FROM policy_dependencies dependency JOIN pg_catalog.pg_class referenced_relation ON dependency.refclassid = 'pg_catalog.pg_class'::regclass AND dependency.refobjid = referenced_relation.oid, selected_table WHERE referenced_relation.oid <> selected_table.relation_oid AND referenced_relation.relrowsecurity) OR EXISTS (SELECT 1 FROM policy_dependencies dependency JOIN pg_catalog.pg_operator referenced_operator ON dependency.refclassid = 'pg_catalog.pg_operator'::regclass AND dependency.refobjid = referenced_operator.oid JOIN pg_catalog.pg_namespace operator_namespace ON operator_namespace.oid = referenced_operator.oprnamespace WHERE operator_namespace.nspname <> 'pg_catalog')) AS required, EXISTS (SELECT 1 FROM policy_dependencies dependency JOIN pg_catalog.pg_class referenced_relation ON dependency.refclassid = 'pg_catalog.pg_class'::regclass AND dependency.refobjid = referenced_relation.oid WHERE referenced_relation.relkind NOT IN ('r', 'p', 'v')) AS has_unobservable_dependency) SELECT relation_dependencies.relation_oid::bigint, relation_dependencies.is_root, opaque_policy.required, (NOT opaque_policy.has_unobservable_dependency AND relation_namespace.nspname <> 'information_schema' AND relation_namespace.nspname NOT LIKE 'pg\\_%' ESCAPE '\\' AND has_table_privilege(relation.oid, 'TRIGGER') AND NOT EXISTS (SELECT 1 FROM pg_catalog.pg_depend extension_dependency WHERE extension_dependency.classid = 'pg_catalog.pg_class'::regclass AND extension_dependency.objid = relation.oid AND extension_dependency.deptype = 'e')), EXISTS (SELECT 1 FROM pg_catalog.pg_trigger installed_trigger WHERE installed_trigger.tgrelid = relation.oid AND installed_trigger.tgname = 'ihp_datasync_invalidate' AND NOT installed_trigger.tgisinternal AND installed_trigger.tgfoid = to_regprocedure('public.ihp_datasync_notify_invalidation()') AND installed_trigger.tgtype = 60 AND installed_trigger.tgenabled IN ('O', 'A')) FROM relation_dependencies JOIN pg_catalog.pg_class relation ON relation.oid = relation_dependencies.relation_oid JOIN pg_catalog.pg_namespace relation_namespace ON relation_namespace.oid = relation.relnamespace CROSS JOIN opaque_policy WHERE relation.relkind IN ('r', 'p') ORDER BY relation_dependencies.relation_oid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList ((,,,,) <$> Decoders.column (Decoders.nonNullable Decoders.int8) <*> Decoders.column (Decoders.nonNullable Decoders.bool) <*> Decoders.column (Decoders.nonNullable Decoders.bool) <*> Decoders.column (Decoders.nonNullable Decoders.bool) <*> Decoders.column (Decoders.nonNullable Decoders.bool)))

-- Sessions

installTableChangeTriggersSession :: Text -> RLS.TableWithRLS -> Session.Session ()
installTableChangeTriggersSession uuidFunction table =
    Session.script (createNotificationFunction uuidFunction table)

installGlobalInvalidationTriggersSession :: Session.Session ()
installGlobalInvalidationTriggersSession = do
    Session.script createGlobalInvalidationFunction
    relationOids <- Session.statement () eligibleGlobalInvalidationRelationsStatement
    forM_ relationOids (Session.script . createGlobalInvalidationTrigger)

installInvalidationTriggersSession :: [Int64] -> Session.Session ()
installInvalidationTriggersSession relationOids = do
    Session.script createGlobalInvalidationFunction
    forM_ relationOids (Session.script . createGlobalInvalidationTrigger)

globalInvalidationSchemaFingerprintSession :: Session.Session Text
globalInvalidationSchemaFingerprintSession =
    Session.statement () globalInvalidationSchemaFingerprintStatement

invalidationDependenciesSession :: Text -> Session.Session [(Int64, Bool, Bool, Bool, Bool)]
invalidationDependenciesSession tableName =
    Session.statement tableName invalidationDependenciesStatement

retrieveChangesSession :: UUID -> Session.Session Text
retrieveChangesSession uuid = Session.statement uuid retrieveChangesStatement

-- IO API (thin wrappers)

installTableChangeTriggers :: Hasql.Pool.Pool -> RLS.TableWithRLS -> IO ()
installTableChangeTriggers pool tableNameRLS = do
    uuidFunction <- defaultUuidFunction
    runSession pool (installTableChangeTriggersSession uuidFunction tableNameRLS)
    pure ()

installGlobalInvalidationTriggers :: Hasql.Pool.Pool -> IO ()
installGlobalInvalidationTriggers pool =
    runSession pool installGlobalInvalidationTriggersSession

-- | One reconciliation state per shared Hasql pool. The MVar prevents first
-- subscriptions on many WebSockets from consuming the pool while they wait on
-- database DDL locks. A catalog fingerprint detects newly created/recreated
-- relations and manually removed triggers, so long-lived controllers do not
-- retain a stale one-shot cache. Failed reconciliation leaves the old
-- fingerprint in place and the next subscription retries.
{-# NOINLINE globalInvalidationInstallStates #-}
globalInvalidationInstallStates :: MVar [(StableName Hasql.Pool.Pool, MVar InvalidationInstallState)]
globalInvalidationInstallStates = unsafePerformIO (newMVar [])

invalidationInstallStateForPool :: Hasql.Pool.Pool -> IO (MVar InvalidationInstallState)
invalidationInstallStateForPool pool = do
    poolName <- makeStableName pool
    modifyMVar globalInvalidationInstallStates \states ->
        case List.find (\(existingPoolName, _) -> existingPoolName == poolName) states of
            Just (_, existingState) -> pure (states, existingState)
            Nothing -> do
                state <- newMVar InvalidationInstallState
                    { globalFingerprint = Nothing
                    , functionReconciled = False
                    }
                pure ((poolName, state) : states, state)

makeCachedInstallGlobalInvalidationTriggers :: Hasql.Pool.Pool -> IO (IO ())
makeCachedInstallGlobalInvalidationTriggers pool = do
    installState <- invalidationInstallStateForPool pool
    pure $ modifyMVar_ installState \state -> do
        currentFingerprint <- runSession pool globalInvalidationSchemaFingerprintSession
        if state.functionReconciled && Just currentFingerprint == state.globalFingerprint
            then pure state
            else do
                installGlobalInvalidationTriggers pool
                reconciledFingerprint <- runSession pool globalInvalidationSchemaFingerprintSession
                pure state
                    { globalFingerprint = Just reconciledFingerprint
                    , functionReconciled = True
                    }

-- | Build a serialized installer for a resolved subscription plan. Normal
-- plans install only the base/RLS dependency relations (plus partition
-- descendants). Schema-wide reconciliation is reserved for opaque policies
-- whose function/view dependencies cannot be represented precisely.
makeInstallInvalidationPlan :: Hasql.Pool.Pool -> IO (InvalidationPlan -> IO ())
makeInstallInvalidationPlan pool = do
    installState <- invalidationInstallStateForPool pool
    pure \plan -> modifyMVar_ installState \state ->
        if plan.requiresGlobalFallback
            then do
                currentFingerprint <- runSession pool globalInvalidationSchemaFingerprintSession
                if state.functionReconciled && Just currentFingerprint == state.globalFingerprint
                    then pure state
                    else do
                        installGlobalInvalidationTriggers pool
                        reconciledFingerprint <- runSession pool globalInvalidationSchemaFingerprintSession
                        pure state
                            { globalFingerprint = Just reconciledFingerprint
                            , functionReconciled = True
                            }
            else if state.functionReconciled && null plan.missingRelationOids
                then pure state
                else do
                    -- Missing exact triggers can also mean the shared function
                    -- was dropped CASCADE while this process-local state stayed
                    -- warm. Reconcile the function whenever any trigger is missing.
                    runSession pool (installInvalidationTriggersSession plan.missingRelationOids)
                    pure state
                        { globalFingerprint = Nothing
                        , functionReconciled = True
                        }

-- | Resolve the relation-scoped invalidation channels required by a query on
-- this RLS table. PostgreSQL records direct table references in policy
-- expressions in @pg_depend@, including membership tables used by @EXISTS@.
-- Policies that call an application-defined function are conservatively also
-- subscribed to the global channel because dependencies hidden in a function
-- body are not necessarily represented on the policy itself. The framework's
-- relation-free @public.ihp_user_id()@ helper is explicitly safe-listed.
--
-- The global fallback observes DML on eligible ordinary and partitioned
-- application relations. It cannot make time-dependent policy expressions or
-- external state hidden inside an opaque function observable. Known direct
-- dependencies that cannot be instrumented are therefore rejected instead of
-- silently accepting a subscription that could become stale.
resolveInvalidationPlan :: Hasql.Pool.Pool -> RLS.TableWithRLS -> IO InvalidationPlan
resolveInvalidationPlan pool table = do
    dependencies <- runSession pool (invalidationDependenciesSession table.tableName)
    when (null dependencies || any (not . fourth) dependencies) do
        throwIO (userError ("DataSync cannot install a safe invalidation trigger for every RLS dependency of " <> cs table.tableName))
    let rootRelationOids = [relationOid | (relationOid, isRoot, _, _, _) <- dependencies, isRoot]
    let relationOids = [relationOid | (relationOid, _, _, _, _) <- dependencies]
    let missingRelationOids = [relationOid | (relationOid, _, _, _, isInstalled) <- dependencies, not isInstalled]
    let relationChannels = Set.fromList (map relationInvalidationChannel rootRelationOids)
    let requiresGlobalFallback = any third dependencies
    let channels = if requiresGlobalFallback
            then Set.insert globalInvalidationChannel relationChannels
            else relationChannels
    pure InvalidationPlan { channels, relationOids, missingRelationOids, requiresGlobalFallback }
    where
        third (_, _, value, _, _) = value
        fourth (_, _, _, value, _) = value

invalidationChannelsForTable :: Hasql.Pool.Pool -> RLS.TableWithRLS -> IO (Set.Set ByteString)
invalidationChannelsForTable pool table =
    (.channels) <$> resolveInvalidationPlan pool table

-- | In development, always re-run trigger SQL because @make db@ drops and
-- recreates the database, destroying previously installed triggers.
-- In production, cache per table to avoid unnecessary work.
makeInstallTableChangeTriggers :: Environment -> Hasql.Pool.Pool -> IO (RLS.TableWithRLS -> IO ())
makeInstallTableChangeTriggers Development pool = pure (installTableChangeTriggers pool)
makeInstallTableChangeTriggers Production pool = makeCachedInstallTableChangeTriggers pool

-- | Process-global lock map for trigger installation. Each table gets an MVar
-- that serializes installation in Haskell-land, so only one connection per
-- table hits the database while others wait cheaply without consuming a pool
-- connection.
-- See: https://github.com/digitallyinduced/ihp/issues/2467
{-# NOINLINE globalTriggerInstallLocks #-}
globalTriggerInstallLocks :: MVar (Map.Map RLS.TableWithRLS (MVar ()))
globalTriggerInstallLocks = unsafePerformIO (newMVar Map.empty)

-- | Wraps 'installTableChangeTriggers' with a process-global per-table lock
-- so each table's triggers are only installed once per process lifetime.
-- Concurrent callers for the same table block on an MVar in Haskell (not on
-- a database connection), preventing pool exhaustion from DDL lock waits.
-- If installation fails, the lock is removed so future connections can retry.
makeCachedInstallTableChangeTriggers :: Hasql.Pool.Pool -> IO (RLS.TableWithRLS -> IO ())
makeCachedInstallTableChangeTriggers pool = do
    pure \tableName -> do
        -- Atomically check if this table already has a lock entry.
        -- If not, create an empty MVar and register as the installer.
        (lock, weAreInstaller) <- modifyMVar globalTriggerInstallLocks \locks ->
            case Map.lookup tableName locks of
                Just existingLock ->
                    pure (locks, (existingLock, False))
                Nothing -> do
                    newLock <- newEmptyMVar
                    pure (Map.insert tableName newLock locks, (newLock, True))

        if weAreInstaller
            then do
                -- We won the race — do the actual install.
                -- On success, signal waiters. On failure, remove the lock
                -- so future connections can retry, then re-throw.
                installTableChangeTriggers pool tableName
                    `catch` \e -> do
                        modifyMVar_ globalTriggerInstallLocks \locks ->
                            pure (Map.delete tableName locks)
                        throwIO (e :: SomeException)
                putMVar lock ()
            else
                -- Another connection is installing (or has installed).
                -- This blocks until the installer calls putMVar, then
                -- immediately returns the () without taking it.
                readMVar lock

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: RLS.TableWithRLS -> ByteString
channelName table = "did_change_" <> (cs $ Text.replace "\"" "\"\"" table.tableName)

-- | Payload-free channel used to invalidate queries affected by writes to
-- relations other than the selected table (for example RLS membership tables).
globalInvalidationChannel :: ByteString
globalInvalidationChannel = "ihp_datasync_invalidate"

relationInvalidationChannelPrefix :: ByteString
relationInvalidationChannelPrefix = "ihp_datasync_invalidate_"

-- | Payload-free invalidation channel emitted only for writes to one relation.
relationInvalidationChannel :: Int64 -> ByteString
relationInvalidationChannel relationOid =
    relationInvalidationChannelPrefix <> cs (tshow relationOid)


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
