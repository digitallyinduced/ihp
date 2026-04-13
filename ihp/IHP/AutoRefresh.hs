{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport hiding (request)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.WebSocket
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import qualified IHP.PGListener as PGListener
import qualified Hasql.Session as HasqlSession
import qualified IHP.Log as Log
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
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

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: Request
    , ?request :: Request
    , ?respond :: Respond
    ) => ((?modelContext :: ModelContext, ?respond :: Respond) => IO ResponseReceived) -> IO ResponseReceived
autoRefresh runAction = do
    -- When PGListener is not available, degrade gracefully to a
    -- plain action without auto-refresh.
    case Vault.lookup pgListenerVaultKey ?request.vault of
        Nothing -> runAction
        Just _ -> do
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

                    -- Capture the current request and context for re-rendering. The
                    -- request vault carries all per-request state (current user, flash
                    -- messages, framework config, ...) so passing the closure-captured
                    -- values back into the renderView callback is enough.
                    let originalRequest = ?request
                    let originalContext = ?context
                    let renderView = \waiRequest waiRespond -> do
                            earlyReturnMiddleware (\_ respond -> do
                                let ?context = originalContext
                                let ?request = originalRequest
                                let ?respond = respond
                                action ?theAction
                                ) waiRequest waiRespond

                    -- We save the allowed session ids to the session cookie to only grant a client access
                    -- to sessions it initially opened itself
                    --
                    -- Otherwise you might try to guess session UUIDs to access other peoples auto refresh sessions
                    setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

                    withTableReadTracker do
                        (result, capturedResponse) <- captureResponseBody ?respond \respond -> do
                            let ?respond = respond
                            runAction

                        -- After the action completes, set up the auto refresh session
                        tables <- readIORef ?touchedTables
                        lastPing <- getCurrentTime
                        case capturedResponse of
                            Just lastResponse -> do
                                event <- MVar.newEmptyMVar
                                let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
                                modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                                async (gcSessions autoRefreshServer)
                                registerNotificationTrigger ?touchedTables autoRefreshServer
                            Nothing -> pure () -- Response wasn't a builder type, can't do auto refresh

                        pure result

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        autoRefreshServer <- getOrCreateAutoRefreshServer
        availableSessions <- getAvailableSessions autoRefreshServer

        when (sessionId `elem` availableSessions) do
            AutoRefreshSession { renderView, event } <- getSessionById autoRefreshServer sessionId

            let handleOtherException :: SomeException -> IO ()
                handleOtherException ex = Log.error ("AutoRefresh: Failed to re-render view: " <> tshow ex)

            async $ forever do
                MVar.takeMVar event
                let currentRequest = ?request
                (do
                    (_, capturedResponse) <- captureResponseBody (\_ -> pure (error "AutoRefresh: ResponseReceived placeholder")) \respond ->
                        renderView currentRequest respond
                    case capturedResponse of
                        Just html -> do
                            responseChanged <- sessionResponseHasChanged autoRefreshServer sessionId html
                            when responseChanged do
                                sendTextData html
                                updateSession autoRefreshServer sessionId (\session -> session { lastResponse = html })
                        Nothing -> pure ()
                    ) `catch` handleOtherException
                pure ()

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


-- | Runs an action while capturing the response body.
-- Returns the action's result and the captured body (if it was a ResponseBuilder).
-- Only captures ResponseBuilder responses (used by HSX/Blaze rendering).
captureResponseBody :: Respond -> (Respond -> IO a) -> IO (a, Maybe LByteString)
captureResponseBody originalRespond action = do
    bodyRef <- newIORef Nothing
    let capturingRespond response = do
            case response of
                Wai.ResponseBuilder _status _headers builder -> do
                    let body = ByteString.toLazyByteString builder
                    evaluatedBody <- Exception.evaluate body
                    writeIORef bodyRef (Just evaluatedBody)
                _ -> pure ()
            originalRespond response
    result <- action capturingRespond
    captured <- readIORef bodyRef
    pure (result, captured)

registerNotificationTrigger :: (?modelContext :: ModelContext, ?context :: Request) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
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
                    |> filter (\session -> table `Set.member` session.tables)
                    |> map (\session -> session.event)
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

-- | Returns 'True' when the rendered html differs from the session's latest
-- known response.
--
-- This must read the current session state instead of comparing against a
-- websocket-local snapshot, otherwise switching back to an earlier DOM state
-- can be incorrectly suppressed as "unchanged".
sessionResponseHasChanged :: IORef AutoRefreshServer -> UUID -> LByteString -> IO Bool
sessionResponseHasChanged autoRefreshServer sessionId html = do
    currentLastResponse <- (.lastResponse) <$> getSessionById autoRefreshServer sessionId
    pure (html /= currentLastResponse)

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

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
