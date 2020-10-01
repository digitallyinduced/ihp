{-|
Module: IHP.AutoRefresh
Description: Provides automatical diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import qualified Data.TMap as TypeMap
import IHP.ControllerSupport
import IHP.ApplicationContext
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Network.Wai.Internal as Wai
import IHP.ControllerSupport
import qualified Data.Binary.Builder as ByteString
import qualified Control.Concurrent as Concurrent
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import Control.Monad (void)
import Control.Concurrent.Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.WebSocket
import qualified IHP.PGNotify as PGNotify

initAutoRefresh :: (?applicationContext :: ApplicationContext) => TypeMap.TMap -> IO TypeMap.TMap
initAutoRefresh context = do
    autoRefreshStateVar :: IORef AutoRefreshState <- newIORef AutoRefreshDisabled
    let autoRefreshServer :: IORef AutoRefreshServer = ?applicationContext |> get #autoRefreshServer
    context
        |> TypeMap.insert @(IORef AutoRefreshState) autoRefreshStateVar
        |> TypeMap.insert @(IORef AutoRefreshServer) autoRefreshServer
        |> pure

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?controllerContext :: ControllerContext
    , ?modelContext :: ModelContext
    , ?requestContext :: RequestContext
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction = do
    let autoRefreshStateVar = fromControllerContext @(IORef AutoRefreshState)
    let autoRefreshServer = fromControllerContext @(IORef AutoRefreshServer)

    readIORef autoRefreshStateVar >>= \case
        AutoRefreshDisabled -> do
            availableSessions <- getAvailableSessions autoRefreshServer

            id <- UUID.nextRandom
            let requestContext = ?requestContext
            let renderView = \requestContext -> let ?requestContext = requestContext in action ?theAction
            

            let newState = AutoRefreshEnabled id
            writeIORef autoRefreshStateVar newState
            
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
                            let lastResponse = ByteString.toLazyByteString builder
                            event <- MVar.newEmptyMVar
                            let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
                            modifyIORef autoRefreshServer (\s -> s { sessions = session:(get #sessions s) } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer
                            
                            throw exception
                    
                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction


autoRefreshViewContext :: (?controllerContext :: ControllerContext) => IO AutoRefreshState
autoRefreshViewContext = readIORef (fromControllerContext @(IORef AutoRefreshState))

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        availableSessions <- ?applicationContext
                |> get #autoRefreshServer
                |> getAvailableSessions

        when (sessionId `elem` availableSessions) do
            AutoRefreshSession { renderView, event, lastResponse } <- getSessionById sessionId

            let handleResponseException (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do
                        let html = ByteString.toLazyByteString builder

                        when (html /= lastResponse) do
                            updateSession sessionId (\session -> session { lastResponse = html })
                            sendTextData html

            async $ forever do
                MVar.takeMVar event
                Concurrent.threadDelay (100000)
                (renderView ?requestContext) `catch` handleResponseException
                pure ()

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                let autoRefreshServer = ?applicationContext |> get #autoRefreshServer
                modifyIORef autoRefreshServer (\server -> server { sessions = filter (\AutoRefreshSession { id } -> id /= sessionId) (get #sessions server) })
            AwaitingSessionID -> pure ()


registerNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set Text) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (get #subscribedTables) <$> (autoRefreshServer |> readIORef)
    
    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    modifyIORef autoRefreshServer (\server -> server { subscribedTables = get #subscribedTables server <> Set.fromList subscriptionRequired })
    forEach subscriptionRequired \table -> do
        void $ PGNotify.watchInsertOrUpdateTable table do
            sessions <- (get #sessions) <$> readIORef autoRefreshServer
            sessions
                    |> filter (\session -> table `Set.member` (get #tables session))
                    |> map (\session -> get #event session)
                    |> mapM (\event -> MVar.tryPutMVar event ())
            pure ()



-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?requestContext :: RequestContext) => IORef AutoRefreshServer -> IO [UUID]
getAvailableSessions autoRefreshServer = do
    allSessions <- (get #sessions) <$> readIORef autoRefreshServer
    text <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (get #id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

-- | Returns a session for a given session id. Errors in case the session does not exist.
getSessionById :: (?applicationContext :: ApplicationContext) => UUID -> IO AutoRefreshSession
getSessionById sessionId = do
    autoRefreshServer <- ?applicationContext
            |> get #autoRefreshServer
            |> readIORef
    autoRefreshServer
        |> get #sessions
        |> find (\AutoRefreshSession { id } -> id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

-- | Applies a update function to a session specified by its session id
updateSession :: (?applicationContext :: ApplicationContext) => UUID -> (AutoRefreshSession -> AutoRefreshSession) -> IO ()
updateSession sessionId updateFunction = do
    let server = ?applicationContext |> get #autoRefreshServer
    let updateSession' session = if get #id session == sessionId then updateFunction session else session
    modifyIORef server (\server -> server { sessions = map updateSession' (get #sessions server) })
    pure ()

-- | Removes all expired sessions
--
-- This is useful to avoid dead sessions hanging around. This can happen when a websocket connection was never established
-- after the inital request. Then the onClose of the websocket app is never called and thus the session will not be
-- removed automatically.
gcSessions :: IORef AutoRefreshServer -> IO ()
gcSessions autoRefreshServer = do
    now <- getCurrentTime
    modifyIORef autoRefreshServer (\autoRefreshServer -> autoRefreshServer { sessions = filter (not . isSessionExpired now) (get #sessions autoRefreshServer) })

-- | A session is expired if it wasn't pinged in the last 60 seconds
isSessionExpired :: UTCTime -> AutoRefreshSession -> Bool
isSessionExpired now AutoRefreshSession { lastPing } = (now `diffUTCTime` lastPing) > (secondsToNominalDiffTime 60)
