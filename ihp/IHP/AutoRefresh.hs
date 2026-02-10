{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.ControllerSupport
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import Control.Exception (SomeException)
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
            let renderView = \waiRequest waiRespond -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext
                    -- Copy vault from original request to preserve layout and other middleware state
                    let waiRequest' = waiRequest { Wai.vault = Wai.vault originalRequest }
                    let ?request = waiRequest'
                    let ?respond = waiRespond
                    putContext waiRequest'
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
                            let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer

                            throw exception
                        _   -> error "Unimplemented WAI response type."

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

            async $ forever do
                MVar.takeMVar event
                let currentRequest = ?request
                -- Create a dummy respond function that does nothing, since actual response
                -- is handled by the handleResponseException handler
                let dummyRespond _ = error "AutoRefresh: respond should not be called directly"
                ((renderView currentRequest dummyRespond) `catch` handleResponseException) `catch` handleOtherException
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
            runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

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
                runSessionHasql pool (mapM_ HasqlSession.script (notificationTriggerStatements table))

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

-- | Returns individual SQL statements to set up a database trigger.
-- Split into separate statements because hasql's extended query protocol
-- only supports single statements per execution.
notificationTriggerStatements :: Text -> [Text]
notificationTriggerStatements tableName =
        [ "BEGIN"
        , "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
            <> "BEGIN\n"
            <> "    PERFORM pg_notify('" <> cs (channelName tableName) <> "', '');\n"
            <> "    RETURN new;\n"
            <> "END;\n"
            <> "$$ language plpgsql"
        , "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "()"
        , "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "()"
        , "DROP TRIGGER IF EXISTS " <> deleteTriggerName <> " ON " <> tableName
        , "CREATE TRIGGER " <> deleteTriggerName <> " AFTER DELETE ON \"" <> tableName <> "\" FOR EACH STATEMENT EXECUTE PROCEDURE " <> functionName <> "()"
        , "COMMIT"
        ]
    where
        functionName = "ar_notify_did_change_" <> tableName
        insertTriggerName = "ar_did_insert_" <> tableName
        updateTriggerName = "ar_did_update_" <> tableName
        deleteTriggerName = "ar_did_delete_" <> tableName

autoRefreshStateVaultKey :: Vault.Key AutoRefreshState
autoRefreshStateVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE autoRefreshStateVaultKey #-}
