module IHP.Server (run, appDatabaseUrl) where
import IHP.Prelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import qualified Data.Time.Clock
import IHP.ModelSupport
import IHP.ApplicationContext
import qualified IHP.ControllerSupport as ControllerSupport
import Database.PostgreSQL.Simple
import qualified IHP.LoginSupport.Middleware
import qualified IHP.Environment as Env
import System.Info

import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.FrameworkConfig (FrameworkConfig, appDatabaseUrl)
import IHP.RouterSupport (frontControllerToWAIApp, HasPath, CanRoute, FrontController)
import qualified IHP.ErrorController as ErrorController

import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.AutoRefresh.Types as AutoRefresh
import qualified IHP.WebSocket as WS

run :: (FrameworkConfig, FrontController FrameworkConfig.RootApplication) => IO ()
run = do
    databaseUrl <- appDatabaseUrl
    session <- Vault.newKey
    port <- FrameworkConfig.initAppPort
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let isDevelopment = Env.isDevelopment FrameworkConfig.environment
    modelContext <- (\modelContext -> modelContext { queryDebuggingEnabled = isDevelopment }) <$> createModelContext FrameworkConfig.dbPoolIdleTime FrameworkConfig.dbPoolMaxConnections databaseUrl
    let ?modelContext = modelContext
    autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
    let ?applicationContext = ApplicationContext { modelContext = ?modelContext, session, autoRefreshServer }
    let application :: Application = \request respond -> do
            requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp FrameworkConfig.RootApplication ErrorController.handleNotFound

    let sessionMiddleware :: Middleware = withSession store "SESSION" FrameworkConfig.sessionCookie session

    libDirectory <- cs <$> FrameworkConfig.findLibDirectory
    let staticMiddleware :: Middleware = staticPolicy (addBase "static/") . staticPolicy (addBase (libDirectory <> "static/"))

    let runServer = if isDevelopment
            then
                let settings = Warp.defaultSettings
                        |> Warp.setBeforeMainLoop (putStrLn "Server started")
                        |> Warp.setPort port
                in Warp.runSettings settings
            else Warp.runEnv port
    runServer $
        staticMiddleware $
                sessionMiddleware $
                    ihpWebsocketMiddleware $
                        FrameworkConfig.requestLoggerMiddleware $
                                methodOverridePost $
                                    application

ihpWebsocketMiddleware :: (?applicationContext :: ApplicationContext) => Middleware
ihpWebsocketMiddleware (next :: Application) (request :: Request) respond = do
        (Websocket.websocketsOr
            Websocket.defaultConnectionOptions
            (websocketServer request respond)
            next) request respond

websocketServer :: (?applicationContext :: ApplicationContext) => Request -> _ -> Websocket.ServerApp
websocketServer request respond pendingConnection = do
    requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
    let ?requestContext = requestContext

    connection <- Websocket.acceptRequest pendingConnection

    WS.startWSApp @AutoRefresh.AutoRefreshWSApp connection