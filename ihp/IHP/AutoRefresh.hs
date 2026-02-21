{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh
( autoRefresh
, registerNotificationTrigger
, shouldRefreshForPayload
, matchesInsertPayload
, matchesInsertPayloadDynamic
, extractRowId
, lookupColumn
, jsonValueMatchesText
, getAvailableSessions
, getSessionById
, updateSession
, gcSessions
, channelName
, notificationTriggerStatements
, resolveAutoRefreshPayload
, autoRefreshStateVaultKey
, globalAutoRefreshServerVar
, AutoRefreshWSApp (..)
) where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Key as AesonKey
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
import Data.Dynamic (Dynamic, fromDynamic)
import IHP.QueryBuilder.Types (Condition(..), FilterOperator(..), getSnippetPrinterText)
import Hasql.DynamicStatements.Snippet (Snippet)

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

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , ?request :: Request
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction = do
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
                            trackedConditions <- readIORef ?trackedConditions
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
                            let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing, trackedIds, trackedConditions }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer

                            throw exception
                        _ -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        autoRefreshServer <- getOrCreateAutoRefreshServer
        availableSessions <- getAvailableSessions autoRefreshServer

        when (sessionId `elem` availableSessions) do
            AutoRefreshSession { renderView, event, lastResponse } <- getSessionById autoRefreshServer sessionId

            let handleResponseException (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do
                        let html = ByteString.toLazyByteString builder

                        when (html /= lastResponse) do
                            sendTextData html
                            updateSession autoRefreshServer sessionId (\session -> session { lastResponse = html })
                    _   -> error "Unimplemented WAI response type."

            let handleOtherException :: SomeException -> IO ()
                handleOtherException ex = Log.error ("AutoRefresh: Failed to re-render view: " <> tshow ex)

            let currentRequest = ?request
            let dummyRespond _ = error "AutoRefresh: respond should not be called directly"
            let onRender = (renderView currentRequest dummyRespond) `catch` handleResponseException

            async $ forever do
                MVar.takeMVar event
                onRender `catch` handleOtherException

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
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\AutoRefreshSession { id } -> id /= sessionId) server.sessions })
            AwaitingSessionID -> pure ()

-- | Registers row-level triggers with smart ID-based filtering.
--
-- Uses row-level PostgreSQL triggers that include the changed row data in the notification payload.
-- For UPDATE\/DELETE: extracts the row ID from the payload and checks if it's in the tracked set.
-- For INSERT: always refreshes (we can't know if the new row matches without re-querying).
-- For tables without ID tracking (raw SQL, fetchCount): always refreshes.
registerNotificationTrigger :: (?modelContext :: ModelContext, ?context :: ControllerContext) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> table `Set.notMember` subscribedTables)

    -- In development, always re-run trigger SQL for all touched tables because
    -- `make db` drops and recreates the database, destroying triggers that were
    -- previously installed. The trigger SQL is idempotent so re-running is safe.
    -- In production, only install triggers for newly seen tables.
    let isDevelopment = ?context.frameworkConfig.environment == Development

    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <- subscriptionRequired |> mapM (\table -> do
        withRowLevelSecurityDisabled do
            let pool = ?modelContext.hasqlPool
            runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

        pgListener |> PGListener.subscribeJSON (channelName table) (\payload -> do
            resolvedPayload <- resolveAutoRefreshPayload payload
            sessions <- (.sessions) <$> readIORef autoRefreshServer
            sessions |> mapM_ (handleSmartRowChange table resolvedPayload)
            pure ()))

    -- Re-run trigger SQL for already-subscribed tables in dev mode
    when isDevelopment do
        let alreadySubscribed = touchedTables |> filter (`Set.member` subscribedTables)
        forM_ alreadySubscribed \table -> do
            withRowLevelSecurityDisabled do
                let pool = ?modelContext.hasqlPool
                runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()
    where
        handleSmartRowChange table resolvedPayload session@AutoRefreshSession { tables, event, trackedIds, trackedConditions }
            | table `Set.member` tables = do
                let conditions = Map.lookup table trackedConditions
                let shouldRefreshNow = case Map.lookup table trackedIds of
                        Nothing -> True  -- table not tracked with IDs (raw SQL, fetchCount, etc.)
                        Just ids | Set.null ids -> True  -- empty ID set means can't filter
                        Just ids -> case resolvedPayload of
                            Nothing -> True  -- payload resolution failed, refresh to be safe
                            Just payload -> shouldRefreshForPayload ids conditions payload
                when shouldRefreshNow $
                    MVar.tryPutMVar event () >> pure ()
            | otherwise = pure ()

-- | Determines whether a notification payload should trigger a refresh based on tracked IDs
-- and WHERE conditions.
--
-- For INSERT: evaluates the INSERT payload against tracked WHERE conditions. If ANY condition
-- set matches the inserted row, we refresh. If no conditions are tracked, we refresh (safe fallback).
-- For UPDATE\/DELETE: only refresh if the row's ID is in our tracked set.
--
-- Note: For UPDATE, this means that if an UPDATE causes a row to newly match a WHERE filter
-- (e.g. a status change), the refresh will be skipped if that row wasn't already tracked.
-- This is an acceptable tradeoff: the page will catch up on the next INSERT or on the next
-- full page load.
shouldRefreshForPayload :: Set Text -> Maybe [Maybe Dynamic] -> AutoRefreshRowChangePayload -> Bool
shouldRefreshForPayload trackedIds maybeConditions payload =
    case payload.payloadOperation of
        AutoRefreshInsert -> case maybeConditions of
            Nothing -> True  -- no condition tracking for this table
            Just conditions -> any (matchesInsertPayloadDynamic newRow) conditions
          where
            newRow = case payload.payloadNewRow of
                Just (Aeson.Object obj) -> obj
                _ -> AesonKeyMap.empty
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

-- | Unwraps a 'Dynamic'-wrapped 'Maybe Condition' and evaluates it against an INSERT payload.
--
-- Returns 'True' (refresh) when:
--   * The 'Dynamic' cannot be cast to 'Condition' (unexpected type, safe fallback)
--   * The condition is 'Nothing' (unfiltered query)
--   * The condition matches the payload
matchesInsertPayloadDynamic :: AesonKeyMap.KeyMap Aeson.Value -> Maybe Dynamic -> Bool
matchesInsertPayloadDynamic _      Nothing            = True  -- no condition (unfiltered query)
matchesInsertPayloadDynamic newRow (Just condDynamic) =
    case fromDynamic condDynamic of
        Nothing        -> True  -- can't cast, safe fallback
        Just condition -> matchesInsertPayload condition newRow

-- | Evaluates a 'Condition' tree against a JSON object (the inserted row).
--
-- For each 'ColumnCondition':
--   * Strips the table prefix from the column name (e.g. @\"tasks.project_id\"@ → @\"project_id\"@)
--   * Extracts text values from the 'Snippet' via 'getSnippetPrinterText'
--   * Conditions with 'applyLeft' or 'applyRight' (e.g. @LOWER(col)@) cannot be evaluated → 'True'
--   * Unsupported operators (LIKE, regex, range comparisons, etc.) → 'True' (safe fallback)
--
-- For compound conditions:
--   * 'AndCondition': both sub-conditions must match
--   * 'OrCondition': at least one sub-condition must match
matchesInsertPayload :: Condition -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchesInsertPayload (AndCondition a b) row = matchesInsertPayload a row && matchesInsertPayload b row
matchesInsertPayload (OrCondition a b) row = matchesInsertPayload a row || matchesInsertPayload b row
matchesInsertPayload (ColumnCondition col op value applyLeft applyRight) row
    -- Can't evaluate conditions with SQL transforms (LOWER, etc.)
    | isJust applyLeft || isJust applyRight = True
    | otherwise = case op of
        EqOp    -> matchEq col value row
        IsOp    -> matchIs col value row
        InOp    -> matchIn col value row
        -- For all other operators (NotEq, Like, regex, range, etc.),
        -- we can't reliably evaluate → safe fallback to refresh
        _       -> True

-- | Match a column = value condition against a JSON row.
matchEq :: Text -> Snippet -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchEq col snippet row =
    case getSnippetPrinterText snippet of
        [filterText] -> jsonValueMatchesText (lookupColumn col row) filterText
        _            -> True  -- multi-value or empty, can't evaluate

-- | Match a column IS value condition (typically IS NULL).
matchIs :: Text -> Snippet -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchIs col snippet row =
    case getSnippetPrinterText snippet of
        [] ->
            -- IS NULL: empty printer means no params (literal NULL)
            case lookupColumn col row of
                Nothing         -> True   -- column not in payload, can't evaluate
                Just Aeson.Null -> True   -- matches IS NULL
                Just _          -> False  -- non-null, doesn't match IS NULL
        _ -> True  -- IS NOT NULL or other, safe fallback

-- | Match a column IN (values) condition against a JSON row.
matchIn :: Text -> Snippet -> AesonKeyMap.KeyMap Aeson.Value -> Bool
matchIn col snippet row =
    case lookupColumn col row of
        Nothing       -> True  -- column not in payload, can't evaluate
        Just jsonVal  -> any (jsonValueMatchesText (Just jsonVal)) filterTexts
    where filterTexts = getSnippetPrinterText snippet

-- | Look up a column in the JSON row, stripping any table prefix.
--
-- E.g. @\"tasks.project_id\"@ looks up key @\"project_id\"@.
lookupColumn :: Text -> AesonKeyMap.KeyMap Aeson.Value -> Maybe Aeson.Value
lookupColumn col row =
    let colName = case Text.breakOnEnd "." col of
            ("", c) -> c
            (_, c)  -> c
    in AesonKeyMap.lookup (AesonKey.fromText colName) row

-- | Compare a JSON value against a text representation from the encoder printer.
--
-- The hasql printer quotes text values (e.g. @"\"abc\""@) but leaves UUIDs and
-- numbers unquoted.  'unquote' strips surrounding double-quotes so that string
-- comparisons work correctly.
jsonValueMatchesText :: Maybe Aeson.Value -> Text -> Bool
jsonValueMatchesText Nothing _ = True  -- column not found, can't evaluate → refresh
jsonValueMatchesText (Just jsonVal) filterText = case jsonVal of
    Aeson.String s -> s == unquote filterText
    Aeson.Number n -> tshow (round n :: Integer) == filterText || tshow n == filterText
    Aeson.Bool b   -> (if b then "true" else "false") == Text.toLower filterText
                    || (if b then "t" else "f") == Text.toLower filterText
    Aeson.Null     -> Text.toLower filterText == "null"
    _              -> True  -- arrays, objects — can't compare, safe fallback
    where
        -- | Strip surrounding double-quotes added by the hasql printer for text values.
        unquote t
            | Text.length t >= 2
            , Text.head t == '"'
            , Text.last t == '"'
            = Text.init (Text.tail t)
            | otherwise = t

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
        |> find (\AutoRefreshSession { id } -> id == sessionId)
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
isSessionExpired now AutoRefreshSession { lastPing } = (now `diffUTCTime` lastPing) > (secondsToNominalDiffTime 60)

-- | Returns the event name of the event that the pg notify trigger dispatches
channelName :: Text -> ByteString
channelName tableName = "ar_did_change_row_" <> cs tableName

-- | Returns a list of SQL statements to set up row-level database notification triggers.
--
-- These triggers send a JSON payload with the operation type and old\/new row data via pg_notify.
-- For payloads exceeding ~8KB, the full JSON is stored in @large_pg_notifications@ and only
-- a reference ID is sent in the notification.
notificationTriggerStatements :: Text -> [Text]
notificationTriggerStatements tableName =
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
            <> "        PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
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
            <> "        PERFORM pg_notify('" <> cs (channelName tableName) <> "', payload);\n"
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

-- | Internal: When the PostgreSQL trigger had to store the full JSON payload in @large_pg_notifications@
-- (because @pg_notify@ payloads are limited to ~8KB), this loads the full row json so smart filtering
-- receives the full row data for ID extraction.
--
-- Returns 'Nothing' when the payload cannot be loaded or decoded. In that case auto refresh will force
-- a refresh.
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
