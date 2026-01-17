{-|
Module: IHP.AutoRefresh
Description: Provides automatically diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import IHP.ControllerSupport
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import IHP.WebSocket
import IHP.Controller.Context
import IHP.Controller.Response 
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import Data.String.Interpolate.IsString
import qualified IHP.Log as Log
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import Network.HTTP.Types.Header (hContentType)
import qualified Text.Blaze.Html.Renderer.Text as BlazeText

initAutoRefresh :: (?context :: ControllerContext) => IO ()
initAutoRefresh = do
    putContext AutoRefreshDisabled

-- | Limits the client-side morphing to the DOM node matching the given CSS selector.
-- Useful when combining Auto Refresh with fragment based renderers such as HTMX.
setAutoRefreshTarget :: (?context :: ControllerContext) => Text -> IO ()
setAutoRefreshTarget selector = putContext (AutoRefreshTarget selector)

autoRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefresh runAction = do
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

            let renderView = \requestContext -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext { requestContext }
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
                            rawResponse <- Exception.evaluate (ByteString.toLazyByteString builder)
                            lastResponse <- ensureAutoRefreshMeta headers rawResponse
                            let response' =
                                    if lastResponse == rawResponse
                                        then response
                                        else Wai.ResponseBuilder status headers (ByteString.fromLazyByteString lastResponse)

                            event <- MVar.newEmptyMVar
                            let session = AutoRefreshSession { id, renderView, event, tables, lastResponse, lastPing }
                            modifyIORef' autoRefreshServer (\s -> s { sessions = session:s.sessions } )
                            async (gcSessions autoRefreshServer)

                            registerNotificationTrigger ?touchedTables autoRefreshServer

                            throw (ResponseException response')
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction

data AutoRefreshWSApp = AwaitingSessionID | AutoRefreshActive { sessionId :: UUID }
instance WSApp AutoRefreshWSApp where
    initialState = AwaitingSessionID

    run = do
        sessionId <- receiveData @UUID
        setState AutoRefreshActive { sessionId }

        availableSessions <- getAvailableSessions (autoRefreshServerFromRequest request)

        when (sessionId `elem` availableSessions) do
            AutoRefreshSession { renderView, event, lastResponse } <- getSessionById (autoRefreshServerFromRequest request) sessionId

            let handleResponseException (ResponseException response) = case response of
                    Wai.ResponseBuilder status headers builder -> do                        
                        rawHtml <- pure (ByteString.toLazyByteString builder)
                        html <- ensureAutoRefreshMeta headers rawHtml

                        Log.info ("AutoRefresh: inner = " <> show (status, headers, builder) <> " END")

                        when (html /= lastResponse) do
                            sendTextData html
                            updateSession (autoRefreshServerFromRequest request) sessionId (\session -> session { lastResponse = html })
                    _   -> error "Unimplemented WAI response type."

            async $ forever do
                MVar.takeMVar event
                let requestContext = ?context.requestContext
                (renderView requestContext) `catch` handleResponseException
                pure ()

            pure ()

        -- Keep the connection open until it's killed and the onClose is called
        forever receiveDataMessage

    onPing = do
        now <- getCurrentTime
        AutoRefreshActive { sessionId } <- getState
        updateSession (autoRefreshServerFromRequest request) sessionId (\session -> session { lastPing = now })

    onClose = do
        getState >>= \case
            AutoRefreshActive { sessionId } -> do
                let autoRefreshServer = autoRefreshServerFromRequest request
                modifyIORef' autoRefreshServer (\server -> server { sessions = filter (\AutoRefreshSession { id } -> id /= sessionId) server.sessions })
            AwaitingSessionID -> pure ()


registerNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef AutoRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar autoRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (autoRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    modifyIORef' autoRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired })

    pgListener <- (.pgListener) <$> readIORef autoRefreshServer
    subscriptions <-  subscriptionRequired |> mapM (\table -> do
        let createTriggerSql = notificationTrigger table

        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            sqlExec createTriggerSql ()

        pgListener |> PGListener.subscribe (channelName table) \notification -> do
                sessions <- (.sessions) <$> readIORef autoRefreshServer
                sessions
                    |> filter (\session -> table `Set.member` session.tables)
                    |> map (\session -> session.event)
                    |> mapM (\event -> MVar.tryPutMVar event ())
                pure ())
    modifyIORef' autoRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions })
    pure ()

-- | Returns the ids of all sessions available to the client based on what sessions are found in the session cookie
getAvailableSessions :: (?context :: ControllerContext) => IORef AutoRefreshServer -> IO [UUID]
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
channelName :: ByteString -> ByteString
channelName tableName = "ar_did_change_" <> tableName

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

ensureAutoRefreshMeta :: (?context :: ControllerContext) => ResponseHeaders -> LByteString -> IO LByteString
ensureAutoRefreshMeta headers html
    | not (isHtmlResponse headers) = pure html
    | LBS.isInfixOf "ihp-auto-refresh-id" html = pure html
    | otherwise = do
        meta <- renderAutoRefreshMeta
        if LBS.null meta
            then pure html
            else pure (insertAutoRefreshMeta meta html)

renderAutoRefreshMeta :: (?context :: ControllerContext) => IO LByteString
renderAutoRefreshMeta = do
    frozenContext <- freeze ?context
    let ?context = frozenContext
    let metaText = BlazeText.renderHtml autoRefreshMeta
    pure (encodeUtf8Text metaText)

insertAutoRefreshMeta :: LByteString -> LByteString -> LByteString
insertAutoRefreshMeta meta html =
    let metaText = decodeUtf8Text meta
        htmlText = decodeUtf8Text html
        resultText = insertMetaText metaText htmlText
    in encodeUtf8Text resultText

insertMetaText :: Text -> Text -> Text
insertMetaText meta html =
    case findHeadInsertionIndex html of
        Just index -> Text.take index html <> meta <> Text.drop index html
        Nothing -> meta <> html

findHeadInsertionIndex :: Text -> Maybe Int
findHeadInsertionIndex html =
    let lowerHtml = Text.toLower html
        (beforeHead, rest) = Text.breakOn "<head" lowerHtml
    in if Text.null rest
        then Nothing
        else
            let afterHead = Text.dropWhile (/= '>') rest
            in if Text.null afterHead
                then Nothing
                else
                    let headOpenLen = Text.length rest - Text.length afterHead
                    in Just (Text.length beforeHead + headOpenLen + 1)

isHtmlResponse :: ResponseHeaders -> Bool
isHtmlResponse headers = case lookup hContentType headers of
    Just value -> "text/html" `BS.isPrefixOf` value
    Nothing -> False

decodeUtf8Text :: LByteString -> Text
decodeUtf8Text = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . LBS.toStrict

encodeUtf8Text :: Text -> LByteString
encodeUtf8Text = LBS.fromStrict . TextEncoding.encodeUtf8

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
