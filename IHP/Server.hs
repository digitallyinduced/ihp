module IHP.Server (run) where
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

import IHP.FrameworkConfig
import IHP.RouterSupport (frontControllerToWAIApp, HasPath, CanRoute, FrontController)
import qualified IHP.ErrorController as ErrorController

import qualified IHP.Controller.RequestContext as RequestContext
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.AutoRefresh.Types as AutoRefresh
import qualified IHP.WebSocket as WS

run :: (FrontController RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    frameworkConfig@(FrameworkConfig { environment, appPort, dbPoolMaxConnections, dbPoolIdleTime, databaseUrl, sessionCookie, requestLoggerMiddleware }) <- buildFrameworkConfig configBuilder
    session <- Vault.newKey
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let isDevelopment = environment == Env.Development
    modelContext <- (\modelContext -> modelContext { queryDebuggingEnabled = isDevelopment }) <$> createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl
    let ?modelContext = modelContext
    autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
    let ?applicationContext = ApplicationContext { modelContext = ?modelContext, session, autoRefreshServer, frameworkConfig }
    let application :: Application = \request respond -> do
            requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
            let ?context = requestContext
            frontControllerToWAIApp RootApplication ErrorController.handleNotFound

    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie session

    libDirectory <- cs <$> findLibDirectory
    let staticMiddleware :: Middleware = staticPolicy (addBase "static/") . staticPolicy (addBase (libDirectory <> "static/"))

    let runServer = if isDevelopment
            then
                let settings = Warp.defaultSettings
                        |> Warp.setBeforeMainLoop (putStrLn "Server started")
                        |> Warp.setPort appPort
                in Warp.runSettings settings
            else Warp.runEnv appPort
    runServer $
        staticMiddleware $
                sessionMiddleware $
                    ihpWebsocketMiddleware $
                        requestLoggerMiddleware $
                                methodOverridePost $
                                    application

ihpWebsocketMiddleware :: (?applicationContext :: ApplicationContext) => Middleware
ihpWebsocketMiddleware (next :: Application) (request :: Request) respond = do
        (Websocket.websocketsOr
            Websocket.defaultConnectionOptions
            (websocketServer request respond)
            next) request respond

websocketServer :: (?applicationContext :: ApplicationContext) => Request -> RequestContext.Respond -> Websocket.ServerApp
websocketServer request respond pendingConnection = do
    requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
    let ?requestContext = requestContext

    connection <- Websocket.acceptRequest pendingConnection

    WS.startWSApp @AutoRefresh.AutoRefreshWSApp connection
