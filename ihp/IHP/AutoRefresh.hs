{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import qualified Control.Concurrent.MVar          as MVar
import qualified Control.Exception                as Exception
import           Control.Monad                    (void)
import qualified Data.Aeson                       as Aeson
import qualified Data.Binary.Builder              as ByteString
import qualified Data.ByteString.Char8            as B8
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V4                     as UUID
import qualified Data.Vault.Lazy                  as Vault
import qualified Database.PostgreSQL.Simple.Types as PG
import           IHP.AutoRefresh.Types
import           IHP.Controller.Context
import           IHP.Controller.Response
import           IHP.Controller.Session
import           IHP.ControllerSupport
import qualified IHP.Log                          as Log
import           IHP.ModelSupport
import qualified IHP.PGListener                   as PGListener
import           IHP.Prelude
import           IHP.WebSocket
import           Network.Wai
import qualified Network.Wai.Internal             as Wai
import           System.IO.Unsafe                 (unsafePerformIO)
import qualified System.Timeout                   as Timeout

initAutoRefresh :: (?context :: ControllerContext) => IO ()
initAutoRefresh = do
    putContext AutoRefreshDisabled

data AutoRefreshOptions = AutoRefreshOptions
    { shouldRefresh :: AutoRefreshChangeSet -> IO Bool
    }

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction =
    autoRefreshInternal AutoRefreshStatementConfig runAction

autoRefreshWith :: forall action. (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    , ?respond :: Respond
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
    autoRefreshState <- fromContext @AutoRefreshState
    let autoRefreshServer = autoRefreshServerFromRequest request

    case autoRefreshState of
        AutoRefreshDisabled -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom

            -- We save the current state of the controller context here. This includes e.g. all current
            -- flash messages, the current user, ...
            --
            -- This frozen context is used as a "template" inside renderView to make a new controller context
            -- with the exact same content we had when rendering the initial page, whenever we do a server-side re-rendering
            frozenControllerContext <- freeze ?context

            let renderView = \waiRequest waiRespond -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext
                    let ?request = waiRequest
                    let ?respond = waiRespond
                    putContext waiRequest
                    -- Re-renders should not restart the AutoRefresh setup, so we keep the state enabled.
                    putContext (AutoRefreshEnabled id)
                    action ?theAction

            putContext (AutoRefreshEnabled id)

            -- We save the allowed session ids to the session cookie to only grant a client access
            -- to sessions it initially opened itself
            --
            -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

            withTableReadTracker do
                let handleResponse exception@(ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            tables <- readIORef ?touchedTables
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

                            configSession <- buildSession config id renderView tables lastResponse lastPing

                            modifyIORef' autoRefreshServer (\s -> s { sessions = configSession:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerTriggers config ?touchedTables autoRefreshServer

                            throw exception
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction

data AutoRefreshConfig
    = AutoRefreshStatementConfig
    | AutoRefreshRowConfig AutoRefreshOptions

buildSession :: AutoRefreshConfig
    -> UUID
    -> (Request -> Respond -> IO ())
    -> Set ByteString
    -> LByteString
    -> UTCTime
    -> IO AutoRefreshSession
buildSession config id renderView tables lastResponse lastPing = do
    event <- MVar.newEmptyMVar
    case config of
        AutoRefreshStatementConfig ->
            pure AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
        AutoRefreshRowConfig options -> do
            pendingChanges <- newIORef mempty
            pure AutoRefreshSessionWithChanges { id, renderView, event, tables, lastResponse, lastPing, pendingChanges, shouldRefresh = options.shouldRefresh }

registerTriggers :: AutoRefreshConfig -> IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerTriggers config touchedTables autoRefreshServer = case config of
    AutoRefreshStatementConfig -> registerNotificationTrigger touchedTables autoRefreshServer
    AutoRefreshRowConfig {} -> registerRowNotificationTrigger touchedTables autoRefreshServer

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        availableSessions <- getAvailableSessions (autoRefreshServerFromRequest request)

        when (sessionId `elem` availableSessions) do
            let autoRefreshServer = autoRefreshServerFromRequest request
            maybeSession <- getSessionById autoRefreshServer sessionId

            -- The session can disappear between the cookie check and lookup (e.g. GC).
            -- In that case we just keep the socket open without wiring up handlers.
            forEach maybeSession \session -> do
                let handleResponseException (ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            let html = ByteString.toLazyByteString builder

                            Log.info ("AutoRefresh: inner = " <> show (status, headers, builder) <> " END")

                            when (html /= lastResponse session) do
                                sendTextData html
                                updateSession autoRefreshServer sessionId (\currentSession -> currentSession { lastResponse = html })
                        _   -> error "Unimplemented WAI response type."

                let currentRequest = ?request
                let dummyRespond _ = error "AutoRefresh: respond should not be called directly"
                -- We re-run the original action with the frozen context so the rerender
                -- shares flash messages, current user, etc. with the initial request.
                let onRender = (renderView session currentRequest dummyRespond) `catch` handleResponseException

                case session of
                    AutoRefreshSession { event } ->
                        async $ forever do
                            MVar.takeMVar event
                            -- Create a dummy respond function that does nothing, since actual response
                            -- is handled by the handleResponseException handler
                            onRender
                    AutoRefreshSessionWithChanges { event, pendingChanges, shouldRefresh } ->
                        async $ forever do
                            MVar.takeMVar event
                            changes <- atomicModifyIORef' pendingChanges (\current -> (mempty, current))
                            shouldRender <- shouldRefresh changes
                            when shouldRender onRender
                            pure ()

                pure ()

        -- Keep the connection open until it's killed and the onClose is called.
        -- We ignore messages because the client only uses this socket for server pushes.
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession (autoRefreshServerFromRequest request) sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                let autoRefreshServer = autoRefreshServerFromRequest request
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\session -> session.id /= sessionId) server.sessions })
            AwaitingSessionID -> pure ()


-- | Registers statement-level triggers that only notify about "something changed" per table.
registerNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        let createTriggerSql = notificationTrigger table

        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            void (sqlExec createTriggerSql ())

        pgListener |> PGListener.subscribe (channelName table) \_notification -> do
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                sessions
                    |> mapMaybe (\session -> case session of
                        AutoRefreshSession { tables, event } | table `Set.member` tables -> Just event
                        AutoRefreshSession {} -> Nothing
                        AutoRefreshSessionWithChanges {} -> Nothing)
                    |> mapM (\event -> MVar.tryPutMVar event ())
                pure ())
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

-- | Registers row-level triggers that include row data for fine-grained refresh decisions.
registerRowNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerRowNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedRowTables <- (.subscribedRowTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> table `Set.notMember` subscribedRowTables)
    modifyIORef' autoRefreshServer (\server -> server { subscribedRowTables = server.subscribedRowTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptionBatches <- subscriptionRequired |> mapM (\table -> do
        primaryKeyColumns <- fetchPrimaryKeyColumns table
        case NonEmpty.nonEmpty primaryKeyColumns of
            Nothing -> do
                -- Row-level updates need a stable identifier to de-duplicate and coalesce changes.
                -- We skip tables without primary keys to avoid emitting ambiguous notifications.
                let ?context = ?modelContext
                Log.warn ("AutoRefresh: skipping row trigger for table without primary key: " <> cs table)
                pure []
            Just columns -> do
                let createTriggerSql = notificationRowTrigger table (NonEmpty.toList columns)

                withRowLevelSecurityDisabled do
                    void (sqlExec createTriggerSql ())

                subscription <- pgListener |> PGListener.subscribeJSON (rowChannelName table) \payload -> do
                    resolvedPayload <- resolveAutoRefreshPayload payload
                    sessions <- (.sessions) <$> readIORef autoRefreshServer
                    sessions |> mapM_ (handleRowChange table resolvedPayload)
                    pure ()

                pure [subscription])

    let subscriptions = concat subscriptionBatches
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
    where
        handleRowChange table payload session = case session of
            AutoRefreshSessionWithChanges { tables, pendingChanges, event }
                | table `Set.member` tables -> do
                    modifyIORef' pendingChanges (insertRowChangeFromPayload table payload)
                    _ <- MVar.tryPutMVar event ()
                    pure ()
            AutoRefreshSessionWithChanges {} -> pure ()
            AutoRefreshSession {} -> pure ()

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie.
-- The session cookie can outlive the in-memory session list, so we filter against active sessions.
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

-- | Returns a session for a given session id. Empty when the session already expired.
getSessionById :: IORef AutoRefreshServer -> UUID -> IO (Maybe AutoRefreshSession)
getSessionById autoRefreshServer sessionId = do
    autoRefreshServer <- readIORef autoRefreshServer
    autoRefreshServer.sessions
        |> find (\session -> session.id == sessionId)
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
channelName :: ByteString -> ByteString
channelName tableName = "ar_did_change_" <> tableName

rowChannelName :: ByteString -> ByteString
rowChannelName tableName = "ar_did_change_row_" <> tableName

-- | Returns the sql code to set up a database trigger
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
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

notificationRowTrigger :: ByteString -> [ByteString] -> PG.Query
notificationRowTrigger tableName primaryKeyColumns = PG.Query [i|
        BEGIN;
            -- Store oversized payloads in a shared table so pg_notify stays within 8KB.
            CREATE UNLOGGED TABLE IF NOT EXISTS public.large_pg_notifications (
                id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                payload TEXT DEFAULT NULL,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
            );
            CREATE INDEX IF NOT EXISTS large_pg_notifications_created_at_index ON public.large_pg_notifications (created_at);
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                DECLARE
                    row_id jsonb;
                    payload TEXT;
                    large_pg_notification_id UUID;
                BEGIN
                    IF (TG_OP = 'DELETE') THEN
                        row_id := #{oldRowIdExpression};
                        payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'old', to_jsonb(OLD))::text;
                        IF octet_length(payload) > 7800 THEN
                            INSERT INTO public.large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'payloadId', large_pg_notification_id::text)::text;
                            DELETE FROM public.large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';
                        END IF;
                        PERFORM pg_notify('#{rowChannelName tableName}', payload);
                        RETURN OLD;
                    ELSE
                        row_id := #{newRowIdExpression};
                        IF (TG_OP = 'UPDATE') THEN
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'old', to_jsonb(OLD), 'new', to_jsonb(NEW))::text;
                        ELSE
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'new', to_jsonb(NEW))::text;
                        END IF;
                        IF octet_length(payload) > 7800 THEN
                            INSERT INTO public.large_pg_notifications (payload) VALUES (payload) RETURNING id INTO large_pg_notification_id;
                            payload := jsonb_build_object('op', lower(TG_OP), 'id', row_id, 'payloadId', large_pg_notification_id::text)::text;
                            DELETE FROM public.large_pg_notifications WHERE created_at < CURRENT_TIMESTAMP - interval '30s';
                        END IF;
                        PERFORM pg_notify('#{rowChannelName tableName}', payload);
                        RETURN NEW;
                    END IF;
                END;
            $$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH ROW EXECUTE PROCEDURE #{functionName}();

        COMMIT;
    |]
    where
        functionName = "ar_notify_row_change_" <> tableName
        insertTriggerName = "ar_did_insert_row_" <> tableName
        updateTriggerName = "ar_did_update_row_" <> tableName
        deleteTriggerName = "ar_did_delete_row_" <> tableName
        newRowIdExpression = rowIdExpression "NEW" primaryKeyColumns
        oldRowIdExpression = rowIdExpression "OLD" primaryKeyColumns

rowIdExpression :: ByteString -> [ByteString] -> ByteString
rowIdExpression recordAlias primaryKeyColumns =
    case primaryKeyColumns of
        [] -> error "notificationRowTrigger: No primary keys found"
        [column] -> "to_jsonb(" <> qualifiedColumn column <> ")"
        columns -> "jsonb_build_object(" <> B8.intercalate ", " (concatMap keyValuePair columns) <> ")"
    where
        -- Use key/value pairs for composite primary keys so ids remain stable even if
        -- column order changes between migrations.
        qualifiedColumn column = recordAlias <> ".\"" <> column <> "\""
        keyValuePair column = ["'" <> column <> "'", qualifiedColumn column]

fetchPrimaryKeyColumns :: (?modelContext :: ModelContext) => ByteString -> IO [ByteString]
fetchPrimaryKeyColumns tableName = do
    let query = "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = ?::regclass AND i.indisprimary ORDER BY array_position(i.indkey, a.attnum)"
    rows <- (sqlQuery (PG.Query query) (PG.Only (cs tableName :: Text)) :: IO [PG.Only Text])
    pure (map (cs . (\(PG.Only name) -> name)) rows)

resolveAutoRefreshPayload :: (?modelContext :: ModelContext) => AutoRefreshRowChangePayload -> IO AutoRefreshRowChangePayload
resolveAutoRefreshPayload payload = case payload.payloadLargePayloadId of
    Nothing        -> pure payload
    Just payloadId -> fetchAutoRefreshPayload payloadId

fetchAutoRefreshPayload :: (?modelContext :: ModelContext) => UUID.UUID -> IO AutoRefreshRowChangePayload
fetchAutoRefreshPayload payloadId = do
    (payload :: ByteString) <- sqlQueryScalar "SELECT payload FROM public.large_pg_notifications WHERE id = ? LIMIT 1" (PG.Only payloadId)
    case Aeson.eitherDecodeStrict' payload of
        Left errorMessage -> error ("AutoRefresh: Unable to decode payload: " <> cs errorMessage)
        Right result -> pure result

autoRefreshVaultKey :: Vault.Key (IORef AutoRefreshServer)
autoRefreshVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshVaultKey #-}

initAutoRefreshMiddleware :: PGListener.PGListener -> IO Middleware
initAutoRefreshMiddleware pgListener = do
    autoRefreshServer <- newIORef (newAutoRefreshServer pgListener)
    pure \app request respond -> do
        let request' = request { vault = Vault.insert autoRefreshVaultKey autoRefreshServer request.vault }
        app request' respond

autoRefreshServerFromRequest :: Request -> IORef AutoRefreshServer
autoRefreshServerFromRequest request =
    case Vault.lookup autoRefreshVaultKey request.vault of
        Just server -> server
        Nothing -> error "AutoRefresh middleware not initialized. Please make sure you have added the AutoRefresh middleware to your application."

