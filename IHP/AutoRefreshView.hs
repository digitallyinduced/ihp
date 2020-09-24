{-|
Module: IHP.AutoRefreshView
Description: Provides automatical diff-based refreshing views after page load
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefreshView where

import IHP.Prelude
import IHP.AutoRefreshView.Types
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

initAutoRefreshView :: (?applicationContext :: ApplicationContext) => TypeMap.TMap -> IO TypeMap.TMap
initAutoRefreshView context = do
    autoRefreshStateVar :: IORef AutoRefreshState <- newIORef AutoRefreshDisabled
    let sessions :: IORef [AutoRefreshSession] = ?applicationContext |> get #autoRefreshSessions
    context
        |> TypeMap.insert @(IORef AutoRefreshState) autoRefreshStateVar
        |> TypeMap.insert @(IORef [AutoRefreshSession]) sessions
        |> pure

autoRefreshView :: (
    ?theAction :: action
    , Controller action
    , ?controllerContext :: ControllerContext
    , ?modelContext :: ModelContext
    , ?requestContext :: RequestContext
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
autoRefreshView runAction = do
    -- TODO: Check that this function not called twice

    let autoRefreshStateVar = fromControllerContext @(IORef AutoRefreshState)
    readIORef autoRefreshStateVar >>= \case
        AutoRefreshDisabled -> do
            let sessionsVar = fromControllerContext @(IORef [AutoRefreshSession])

            availableSessions <- readIORef sessionsVar >>= getAvailableSessions

            id <- UUID.nextRandom

            let requestContext = ?requestContext
            let renderView = \requestContext -> let ?requestContext = requestContext in action ?theAction
            event <- MVar.newEmptyMVar
            let session = AutoRefreshSession { id, renderView, event }

            let newState = AutoRefreshEnabled id
            writeIORef autoRefreshStateVar newState
            modifyIORef sessionsVar (\sessions -> session:sessions)

            
            setSession "autoRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")


            -- Track all tables in SELECT queries
            touchedTablesVar <- newIORef Set.empty
            let trackTableReadCallback = Just \tableName -> modifyIORef touchedTablesVar (Set.insert tableName)
            let modelContextWithTracker = ?modelContext { trackTableReadCallback }
            let ?modelContext = modelContextWithTracker
            
            runAction `Exception.finally` (registerNotificationTrigger touchedTablesVar)
        AutoRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction
    -- TODO: fix infinite loop

autoRefreshViewContext :: (?controllerContext :: ControllerContext) => IO AutoRefreshState
autoRefreshViewContext = readIORef (fromControllerContext @(IORef AutoRefreshState))

startDatabaseChangeListener :: (?modelContext :: ModelContext, ?applicationContext :: ApplicationContext) => IO (Async ())
startDatabaseChangeListener = async $ do
    let ModelContext { databaseConnection } = ?modelContext

    let tableNames :: [Text] = ["check_repository_jobs", "create_container_jobs", "deployments", "build_container_jobs", "deployment_logs"]
    forEach tableNames \tableName -> do
        let listenStatement = "LISTEN " <> PG.Query (cs $ eventName tableName)
        void $ async $ forever do
            void $ PG.execute databaseConnection listenStatement ()
            notification <- PG.getNotification databaseConnection

            sessions <- ?applicationContext
                    |> get #autoRefreshSessions
                    |> readIORef
            forEach sessions \session -> void $ MVar.tryPutMVar (get #event session) ()


websocketServer :: (?requestContext :: RequestContext, ?applicationContext :: ApplicationContext) => Websocket.Connection -> IO ()
websocketServer connection = do
    putStrLn "CONNETION"
    -- TODO: don't override multiple autoRefreshSessionIds (because multiple tabs)
    sessionId :: UUID <- Websocket.receiveData connection

    allSessions <- ?applicationContext
            |> get #autoRefreshSessions
            |> readIORef
    availableSessions <- getAvailableSessions allSessions

    async $ forever do
        (Websocket.receiveDataMessage connection) `Exception.onException` (closeSession sessionId)

    when (sessionId `elem` availableSessions) do
        AutoRefreshSession { renderView, event } <- getSessionById sessionId

        let handleResponseException (ResponseException response) = case response of
                Wai.ResponseBuilder status headers builder -> do
                    let html = ByteString.toLazyByteString builder
                    Websocket.sendTextData connection html
                    pure ()

        forever do
            MVar.takeMVar event
            putStrLn "RERENDER"
            Concurrent.threadDelay (100000)
            (renderView ?requestContext) `catch` handleResponseException
            pure ()

registerNotificationTrigger touchedTablesVar = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    
    let statements = touchedTables
            |> map createNotificationTrigger
            |> map cs
            |> map PG.Query
    let ModelContext { databaseConnection } = ?modelContext
    forEach statements \statement -> do
        logQuery statement ()
        PG.execute_ databaseConnection statement
        pure ()

createNotificationTrigger :: Text -> Text
createNotificationTrigger tableName = "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
        <> "BEGIN\n"
        <> "    PERFORM pg_notify('" <> eventName tableName <> "', row_to_json(new)::text);\n"
        <> "    RETURN new;"
        <> "END;\n"
        <> "$$ language plpgsql;"
        <> "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
    where
        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName

eventName tableName = "did_change_" <> tableName

getAvailableSessions :: (?requestContext :: RequestContext) => [AutoRefreshSession] -> IO [UUID]
getAvailableSessions allSessions = do
    text <- fromMaybe "" <$> getSession "autoRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (get #id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

getSessionById :: (?applicationContext :: ApplicationContext) => UUID -> IO AutoRefreshSession
getSessionById sessionId = do
    sessions <- ?applicationContext
            |> get #autoRefreshSessions
            |> readIORef
    sessions
        |> find (\AutoRefreshSession { id } -> id == sessionId)
        |> Maybe.fromMaybe (error "getSessionById: Could not find the session")
        |> pure

closeSession :: (?applicationContext :: ApplicationContext) => UUID -> IO ()
closeSession sessionId = do
    let sessionsRef = ?applicationContext |> get #autoRefreshSessions
    modifyIORef sessionsRef (filter (\AutoRefreshSession { id } -> id /= sessionId))

instance Websocket.WebSocketsData UUID where
    fromDataMessage (Websocket.Text byteString _) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromDataMessage (Websocket.Binary byteString) = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    fromLazyByteString byteString = UUID.fromLazyASCIIBytes byteString |> Maybe.fromJust
    toLazyByteString = UUID.toLazyASCIIBytes