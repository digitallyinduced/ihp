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
import IHP.LibDir
import qualified IHP.Job.Runner as Job
import qualified IHP.Job.Types as Job
import qualified Control.Concurrent.Async as Async

run :: (FrontController RootApplication, Job.Worker RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    frameworkConfig <- buildFrameworkConfig configBuilder
    
    sessionVault <- Vault.newKey
    autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
    modelContext <- initModelContext frameworkConfig

    let ?modelContext = modelContext
    let ?applicationContext = ApplicationContext { modelContext = ?modelContext, session = sessionVault, autoRefreshServer, frameworkConfig }

    sessionMiddleware <- initSessionMiddleware sessionVault frameworkConfig            
    staticMiddleware <- initStaticMiddleware
    let requestLoggerMiddleware = get #requestLoggerMiddleware frameworkConfig

    withBackgroundWorkers frameworkConfig do
        runServer frameworkConfig $
            staticMiddleware $
                    sessionMiddleware $
                        ihpWebsocketMiddleware $
                            requestLoggerMiddleware $
                                    methodOverridePost $
                                        application
{-# INLINE run #-}

withBackgroundWorkers :: (Job.Worker RootApplication, ?modelContext :: ModelContext) => FrameworkConfig -> _
withBackgroundWorkers frameworkConfig app = do
    let jobWorkers = Job.workers RootApplication
    let isDevelopment = get #environment frameworkConfig == Env.Development
    if isDevelopment && not (isEmpty jobWorkers)
            then Async.withAsync (let ?context = frameworkConfig in Job.runJobWorkers jobWorkers) (\_ -> app)
            else app

initStaticMiddleware :: IO Middleware
initStaticMiddleware = do
    libDirectory <- cs <$> findLibDirectory
    let middleware = staticPolicy (addBase "static/") . staticPolicy (addBase (libDirectory <> "static/"))
    pure middleware

initSessionMiddleware :: _ -> FrameworkConfig -> IO Middleware
initSessionMiddleware sessionVault FrameworkConfig { sessionCookie } = do
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie sessionVault
    pure sessionMiddleware

initModelContext :: FrameworkConfig -> IO ModelContext
initModelContext FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl } = do
    let isDevelopment = environment == Env.Development
    modelContext <- (\modelContext -> modelContext { queryDebuggingEnabled = isDevelopment }) <$> createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl
    pure modelContext

application :: (FrontController RootApplication, ?applicationContext :: ApplicationContext) => Application
application request respond = do
        requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
        let ?context = requestContext
        frontControllerToWAIApp RootApplication ErrorController.handleNotFound

runServer :: FrameworkConfig -> _
runServer FrameworkConfig { environment = Env.Development, appPort } = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setBeforeMainLoop (putStrLn "Server started")
                    |> Warp.setPort appPort
runServer FrameworkConfig { environment = Env.Production, appPort } = Warp.runEnv appPort

ihpWebsocketMiddleware :: (?applicationContext :: ApplicationContext) => Middleware
ihpWebsocketMiddleware (next :: Application) (request :: Request) respond = do
        (Websocket.websocketsOr
            Websocket.defaultConnectionOptions
            (websocketServer request respond)
            next) request respond
{-# INLINE ihpWebsocketMiddleware #-}

websocketServer :: (?applicationContext :: ApplicationContext) => Request -> RequestContext.Respond -> Websocket.ServerApp
websocketServer request respond pendingConnection = do
    requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
    let ?requestContext = requestContext

    connection <- Websocket.acceptRequest pendingConnection

    WS.startWSApp @AutoRefresh.AutoRefreshWSApp connection
