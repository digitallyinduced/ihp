{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.Simple.Types as PG
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.WebSocket
import IHP.Controller.Context
import IHP.Controller.Response
import qualified IHP.PGListener as PGListener
import qualified Hasql.Session as HasqlSession
import qualified IHP.Log as Log
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import qualified Data.TMap as TypeMap
import IHP.RequestVault (pgListenerVaultKey)
import IHP.FrameworkConfig.Types (FrameworkConfig(..))
import IHP.Environment (Environment(..))

{-# NOINLINE globalAutoRefreshServerVar #-}
globalAutoRefreshServerVar :: MVar.MVar (Maybe (IORef AutoRefreshServer))
globalAutoRefreshServerVar = unsafePerformIO (MVar.newMVar Nothing)

getOrCreateAutoRefreshServer :: (?request :: Request) => IO (IORef AutoRefreshServer)
getOrCreateAutoRefreshServer =
    MVar.modifyMVar globalAutoRefreshServerVar $ \case
        Just server -> pure (Just server, server)
        Nothing -> do
            let pgListener = case Vault.lookup pgListenerVaultKey ?request.vault of
                    Just pl -> pl
                    Nothing -> error "getOrCreateAutoRefreshServer: PGListener not found in request vault"
            server <- newIORef (newAutoRefreshServer pgListener)
            pure (Just server, server)

-- | Options for fine-grained auto refresh via 'autoRefreshWith'.
--
-- The callback should be fast and ideally avoid additional SQL queries. It runs on the server and decides whether a
-- received batch of row changes should trigger a re-render.
data AutoRefreshOptions = AutoRefreshOptions
    { shouldRefresh :: AutoRefreshChangeSet -> IO Bool
    -- ^ Return @True@ to re-render for the given batch of changes
    }

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction =
    autoRefreshInternal AutoRefreshSmartConfig runAction

-- | Like 'autoRefresh', but lets you decide (based on the changed rows) whether a refresh should re-render the page.
--
-- This is useful for pages that track high-churn tables but only care about a subset of rows (e.g. based on foreign keys
-- or current user id).
autoRefreshWith :: forall action. (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => AutoRefreshOptions -> ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefreshWith options runAction =
    autoRefreshInternal (AutoRefreshRowConfig options) runAction

-- | Shared setup for the initial render. The row-level path adds change tracking
-- and a refresh predicate, while the statement-level path only tracks table touches.
autoRefreshInternal :: forall action. (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => AutoRefreshConfig -> ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefreshInternal config runAction = do
    let autoRefreshState = Vault.lookup autoRefreshStateVaultKey ?request.vault
    autoRefreshServer <- getOrCreateAutoRefreshServer

    case autoRefreshState of
        Just (AutoRefreshEnabled {}) -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction
        _ -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom

            -- Update the vault with AutoRefreshEnabled so that autoRefreshMeta can read it
            let newRequest = ?request { vault = Vault.insert autoRefreshStateVaultKey (AutoRefreshEnabled id) ?request.vault }
            let ?request = newRequest
            -- Update request in controller context so freeze captures the updated state
            let ControllerContext { customFieldsRef } = ?context
            modifyIORef' customFieldsRef (TypeMap.insert @Network.Wai.Request newRequest)

            -- We save the current state of the controller context here. This includes e.g. all current
            -- flash messages, the current user, ...
            --
            -- This frozen context is used as a "template" inside renderView to make a new controller context
            -- with the exact same content we had when rendering the initial page, whenever we do a server-side re-rendering
            frozenControllerContext <- freeze ?context

            let originalRequest = ?request
            let renderView = \_waiRequest waiRespond -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext
                    let ?request = originalRequest
                    let ?respond = waiRespond
                    putContext originalRequest
                    action ?theAction

            -- We save the allowed session ids to the session cookie to only grant a client access
            -- to sessions it initially opened itself
            --
            -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

            withTableReadTracker do
                let handleResponse exception@(ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            tables <- readIORef ?touchedTables
                            trackedIds <- readIORef ?trackedIds
                            lastPing <- getCurrentTime

                            -- It's important that we evaluate the response to HNF here
                            -- Otherwise a response `error "fail"` will break auto refresh and cause
                            -- the action to be unreachable until the server is restarted.
                            --
                            -- Specifically a request like this will crash the action:
                            --
                            -- > curl --header 'Accept: application/json' http://localhost:8000/ShowItem?itemId=6bbe1a72-400a-421e-b26a-ff58d17af3e5
                            --
                            -- Let's assume that the view has no implementation for JSON responses. Then
                            -- it will render a 'error "JSON not implemented"'. After this curl request
                            -- all future HTML requests to the current action will fail with a 503.
                            --
                            lastResponse <- Exception.evaluate (ByteString.toLazyByteString builder)

                            event <- MVar.newEmptyMVar
                            session <- case config of
                                AutoRefreshStatementConfig ->
                                    pure AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing, trackedIds = Map.empty }
                                AutoRefreshRowConfig options -> do
                                    pendingChanges <- newIORef (Just mempty)
                                    pure AutoRefreshSessionWithChanges { id, renderView, event, tables, lastResponse, lastPing, pendingChanges, shouldRefresh = options.shouldRefresh, trackedIds = Map.empty }
                                AutoRefreshSmartConfig ->
                                    pure AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing, trackedIds }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            case config of
                                AutoRefreshStatementConfig -> registerNotificationTrigger ?touchedTables autoRefreshServer
                                AutoRefreshRowConfig {} -> registerRowNotificationTrigger ?touchedTables autoRefreshServer
                                AutoRefreshSmartConfig -> registerSmartNotificationTrigger ?touchedTables autoRefreshServer

                            throw exception
                        _ -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse

data AutoRefreshConfig
    = AutoRefreshStatementConfig
    | AutoRefreshRowConfig AutoRefreshOptions
    | AutoRefreshSmartConfig

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        autoRefreshServer <- getOrCreateAutoRefreshServer
        availableSessions <- getAvailableSessions autoRefreshServer

        when (sessionId `elem` availableSessions) do
            session <- getSessionById autoRefreshServer sessionId

            let handleResponseException (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do
                        let html = ByteString.toLazyByteString builder

                        when (html /= lastResponse session) do
                            sendTextData html
                            updateSession autoRefreshServer sessionId (\session -> session { lastResponse = html })
                    _ -> error "Unimplemented WAI response type."

            let handleOtherException :: SomeException -> IO ()
                handleOtherException ex = Log.error ("AutoRefresh: Failed to re-render view: " <> tshow ex)

            let currentRequest = ?request
            let dummyRespond _ = error "AutoRefresh: respond should not be called directly"
            -- Create a dummy respond function that does nothing, since actual response
            -- is handled by the handleResponseException handler
            let onRender = (renderView session currentRequest dummyRespond) `catch` handleResponseException

            case session of
                AutoRefreshSession { event } ->
                    async $ forever do
                        MVar.takeMVar event
                        onRender `catch` handleOtherException
                AutoRefreshSessionWithChanges { event, pendingChanges, shouldRefresh } ->
                    async $ forever do
                        MVar.takeMVar event
                        pending <- atomicModifyIORef' pendingChanges (\current -> (Just mempty, current))
                        (case pending of
                            Nothing -> onRender
                            Just changes -> do
                                shouldRender <- shouldRefresh changes
                                when shouldRender onRender
                            ) `catch` handleOtherException

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        autoRefreshServer <- getOrCreateAutoRefreshServer
        updateSession autoRefreshServer sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                autoRefreshServer <- getOrCreateAutoRefreshServer
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\session -> session.id /= sessionId) server.sessions })
            AwaitingSessionID -> pure ()

-- | Registers statement-level triggers that only notify about "something changed" per table.
registerNotificationTrigger :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)

    -- In development, always re-run trigger SQL for all touched tables because
    -- `make db` drops and recreates the database, destroying triggers that were
    -- previously installed. The trigger SQL is idempotent so re-running is safe.
    -- In production, only install triggers for newly seen tables.
    let isDevelopment = ?context.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

        pgListener |> PGListener.subscribe (channelName table) \notification -> do
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            sessions
                |> mapMaybe (\session -> case session of
                    AutoRefreshSession { tables, event } | table `Set.member` tables -> Just event
                    AutoRefreshSession {} -> Nothing
                    AutoRefreshSessionWithChanges {} -> Nothing)
                |> mapM (\event -> MVar.tryPutMVar event ())
            pure ())

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (\table -> subscribedTables |> Set.member table)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (HasqlSession.script (notificationTriggerSQL table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

-- | Registers row-level triggers that include row data for fine-grained refresh decisions.
registerRowNotificationTrigger :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerRowNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedRowTables <- (.subscribedRowTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> table `Set.notMember` subscribedRowTables)

    -- In development, always re-run trigger SQL for all touched tables because
    -- `make db` drops and recreates the database, destroying triggers that were
    -- previously installed. The trigger SQL is idempotent so re-running is safe.
    -- In production, only install triggers for newly seen tables.
    let isDevelopment = ?context.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedRowTables = server.subscribedRowTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

        pgListener |> PGListener.subscribeJSON (rowChannelName table) (\payload -> do
            resolvedPayload <- resolveAutoRefreshPayload payload
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            sessions |> mapM_ (handleRowChange table resolvedPayload)
            pure ()))

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (`Set.member` subscribedRowTables)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
    where
        handleRowChange table resolvedPayload session = case session of
            AutoRefreshSessionWithChanges { tables, pendingChanges, event }
                | table `Set.member` tables -> do
                    case resolvedPayload of
                        Nothing ->
                            writeIORef pendingChanges Nothing
                        Just payload ->
                            modifyIORef' pendingChanges (\pending -> case pending of
                                Nothing -> Nothing
                                Just current -> Just (insertRowChangeFromPayload table payload current))
                    _ <- MVar.tryPutMVar event ()
                    pure ()
            AutoRefreshSessionWithChanges {} -> pure ()
            AutoRefreshSession {} -> pure ()

-- | Registers row-level triggers with smart ID-based filtering.
--
-- Uses the same row-level PostgreSQL triggers as 'registerRowNotificationTrigger', but instead of
-- delegating filtering to a user-provided 'shouldRefresh' callback, it automatically filters
-- based on the row IDs that were fetched during the initial render.
--
-- For UPDATE/DELETE: extracts the row ID from the notification payload and checks if it's in the tracked set.
-- For INSERT: always refreshes (we can't know if the new row matches without re-querying).
-- For tables without ID tracking (raw SQL, fetchCount): always refreshes (today's behavior).
registerSmartNotificationTrigger :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerSmartNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedRowTables <- (.subscribedRowTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> table `Set.notMember` subscribedRowTables)

    let isDevelopment = ?context.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedRowTables = server.subscribedRowTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

        pgListener |> PGListener.subscribeJSON (rowChannelName table) (\payload -> do
            resolvedPayload <- resolveAutoRefreshPayload payload
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            sessions |> mapM_ (handleSmartRowChange table resolvedPayload)
            pure ()))

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (`Set.member` subscribedRowTables)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (mapM_ HasqlSession.script (notificationRowTriggerStatements table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
    where
        handleSmartRowChange table resolvedPayload session = case session of
            AutoRefreshSession { tables, event, trackedIds }
                | table `Set.member` tables -> do
                    let shouldRefreshNow = case Map.lookup table trackedIds of
                            Nothing -> True  -- table not tracked with IDs (raw SQL, fetchCount, etc.)
                            Just ids | Set.null ids -> True  -- empty ID set means can't filter
                            Just ids -> case resolvedPayload of
                                Nothing -> True  -- payload resolution failed, refresh to be safe
                                Just payload -> shouldRefreshForPayload ids payload
                    when shouldRefreshNow $
                        MVar.tryPutMVar event () >> pure ()
            -- Also handle AutoRefreshSessionWithChanges (from autoRefreshWith) and
            -- AutoRefreshSession without matching table
            AutoRefreshSessionWithChanges { tables, pendingChanges, event }
                | table `Set.member` tables -> do
                    case resolvedPayload of
                        Nothing ->
                            writeIORef pendingChanges Nothing
                        Just payload ->
                            modifyIORef' pendingChanges (\pending -> case pending of
                                Nothing -> Nothing
                                Just current -> Just (insertRowChangeFromPayload table payload current))
                    _ <- MVar.tryPutMVar event ()
                    pure ()
            _ -> pure ()

-- | Determines whether a notification payload should trigger a refresh based on tracked IDs.
--
-- For INSERT: always refresh (new row could match the query filters).
-- For UPDATE/DELETE: only refresh if the row's ID is in our tracked set.
shouldRefreshForPayload :: Set Text -> AutoRefreshRowChangePayload -> Bool
shouldRefreshForPayload trackedIds payload =
    case payload.payloadOperation of
        AutoRefreshInsert -> True  -- can't filter without re-evaluating query filters
        _ -> case extractRowId payload of
            Nothing -> True  -- can't extract ID, refresh to be safe
            Just rowId -> rowId `Set.member` trackedIds

-- | Extracts the row ID from a notification payload.
--
-- Looks for an "id" field in either the new or old row JSON.
extractRowId :: AutoRefreshRowChangePayload -> Maybe Text
extractRowId payload =
    let row = payload.payloadNewRow <|> payload.payloadOldRow
    in row >>= \case
        Aeson.Object obj -> case AesonKeyMap.lookup "id" obj of
            Just (Aeson.String s) -> Just s
            Just (Aeson.Number n) -> Just (tshow (round n :: Integer))
            _ -> Nothing
        _ -> Nothing

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?request :: Request) => IORef AutoRefreshServer -> IO [UUID]
getAvailableSessions autoRefreshServer = do
    allSessions <- (.sessions) <$> readIORef autoRefreshServer
    text <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (.id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

-- | Returns a session for a given session id. Errors in case the session does not exist.
getSessionById :: IORef AutoRefreshServer -> UUID -> IO AutoRefreshSession
getSessionById autoRefreshServer sessionId = do
    autoRefreshServer <- readIORef autoRefreshServer
    autoRefreshServer.sessions
        |> find (\session -> session.id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

-- | Applies a update function to a session specified by its session id
updateSession :: IORef AutoRefreshServer -> UUID -> (AutoRefreshSession -> AutoRefreshSession) -> IO ()
updateSession server sessionId updateFunction = do
    let updateSession' session = if session.id == sessionId then updateFunction session else session
    modifyIORef' server (\server -> server { sessions = map updateSession' server.sessions })
    pure ()

-- | Removes all expired sessions
--
-- This is useful to avoid dead sessions hanging around. This can happen when a websocket connection was never established
-- after the initial request. Then the onClose of the websocket app is never called and thus the session will not be
-- removed automatically.
gcSessions :: IORef AutoRefreshServer -> IO ()
gcSessions autoRefreshServer = do
    now <- getCurrentTime
    modifyIORef' autoRefreshServer (\autoRefreshServer -> autoRefreshServer { sessions = filter (not . isSessionExpired now) autoRefreshServer.sessions })

-- | A session is expired if it was not pinged in the last 60 seconds
isSessionExpired :: UTCTime -> AutoRefreshSession -> Bool
isSessionExpired now session = (now `diffUTCTime` session.lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_" <> cs tableName

-- | Returns a SQL script to set up database notification triggers.
--
-- Wrapped in a DO $$ block with EXCEPTION handler because concurrent requests
-- can race to CREATE OR REPLACE the same function, causing PostgreSQL to throw
-- 'tuple concurrently updated' (SQLSTATE XX000). This is safe to ignore: the
-- other connection's CREATE OR REPLACE will have succeeded.
notificationTriggerSQL :: Text -> Text
notificationTriggerSQL tableName =
        "DO $$\n"
        <> "BEGIN\n"
        <> "    CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $BODY$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> cs (channelName tableName) <> "', '');\n"
            <> "    RETURN new;\n"
            <> "END;\n"
            <> "$BODY$ language plpgsql;\n"
        <> "    DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "    DROP TRIGGER IF EXISTS " <> deleteTriggerName <> " ON " <> tableName <> ";\n"
        <> "    CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "EXCEPTION\n"
        <> "    WHEN SQLSTATE 'XX000' THEN null; -- 'tuple concurrently updated': another connection installed it first\n"
        <> "END; $$"
    where
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

notificationRowTriggerStatements :: Text -> [Text]
notificationRowTriggerStatements tableName =
        [ "BEGIN"
        , "CREATE UNLOGGED TABLE IF NOT EXISTS public.large_pg_notifications ("
            <> "id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL, "
            <> "payload TEXT DEFAULT NULL, "
            <> "created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL"
            <> ")"
        , "CREATE INDEX IF NOT EXISTS large_pg_notifications_created_at_index ON public.large_pg_notifications (created_at)"
        , "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
            <> "DECLARE\n"
            <> "    payload TEXT;\n"
            <> "    large_pg_notification_id UUID;\n"
            <> "BEGIN\n"
            <> "    IF (TG_OP = 'DELETE') THEN\n"
            <> "        payload := jsonb_build_object('op', lower(TG_OP), 'old', to_jsonb(OLD))::text;\n"
            <> "        IF octet_length(payload) > 7800 THEN\n"
            <> "            INSERT INTO public.large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;\n"
            <> "            payload := jsonb_build_object('op', lower(TG_OP), 'payloadId', large_pg_notification_id::text)::text;\n"
            <> "            DELETE FROM public.large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';\n"
            <> "        END IF;\n"
            <> "        PERFORM pg_notify('" <> cs (rowChannelName tableName) <> "', payload);\n"
            <> "        RETURN OLD;\n"
            <> "    ELSE\n"
            <> "        IF (TG_OP = 'UPDATE') THEN\n"
            <> "            payload := jsonb_build_object('op', lower(TG_OP), 'old', to_jsonb(OLD), 'new', to_jsonb(NEW))::text;\n"
            <> "        ELSE\n"
            <> "            payload := jsonb_build_object('op', lower(TG_OP), 'new', to_jsonb(NEW))::text;\n"
            <> "        END IF;\n"
            <> "        IF octet_length(payload) > 7800 THEN\n"
            <> "            INSERT INTO public.large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;\n"
            <> "            payload := jsonb_build_object('op', lower(TG_OP), 'payloadId', large_pg_notification_id::text)::text;\n"
            <> "            DELETE FROM public.large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';\n"
            <> "        END IF;\n"
            <> "        PERFORM pg_notify('" <> cs (rowChannelName tableName) <> "', payload);\n"
            <> "        RETURN NEW;\n"
            <> "    END IF;\n"
            <> "END;\n"
            <> "$$ language plpgsql"
        , "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
        , "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
        , "DROP TRIGGER IF EXISTS " <> deleteTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "()"
        , "COMMIT"
        ]
    where
        functionName = "ar_notify_row_change_" <> tableName
        insertTriggerName = "ar_did_insert_row_" <> tableName
        updateTriggerName = "ar_did_update_row_" <> tableName
        deleteTriggerName = "ar_did_delete_row_" <> tableName

rowChannelName :: Text -> ByteString
rowChannelName tableName = "ar_did_change_row_" <> cs tableName

-- | Internal: When the PostgreSQL trigger had to store the full JSON payload in @large_pg_notifications@
-- (because @pg_notify@ payloads are limited to ~8KB), this loads the full row json so the change set passed
-- to 'IHP.AutoRefresh.AutoRefreshOptions.shouldRefresh' still contains @old@/@new@.
--
-- Returns 'Nothing' when the payload cannot be loaded or decoded. In that case auto refresh will force a refresh
-- instead of calling the user-provided 'shouldRefresh'.
resolveAutoRefreshPayload :: (?modelContext :: ModelContext) => AutoRefreshRowChangePayload -> IO (Maybe AutoRefreshRowChangePayload)
resolveAutoRefreshPayload payload = case payload.payloadLargePayloadId of
    Nothing -> pure (Just payload)
    Just payloadId -> fetchAutoRefreshPayload payloadId

fetchAutoRefreshPayload :: (?modelContext :: ModelContext) => UUID.UUID -> IO (Maybe AutoRefreshRowChangePayload)
fetchAutoRefreshPayload payloadId = do
    payloadResult <- Exception.try (sqlQueryScalar "SELECT payload FROM public.large_pg_notifications WHERE id = ? LIMIT 1" (PG.Only payloadId) :: IO ByteString)
    case payloadResult of
        Left (_ :: Exception.SomeException) -> pure Nothing
        Right payload -> case Aeson.eitherDecodeStrict' payload of
            Left _ -> pure Nothing
            Right result -> pure (Just result)

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
