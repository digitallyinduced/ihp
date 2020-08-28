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
import qualified Web.Cookie as Cookie
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

run :: (FrameworkConfig, FrontController FrameworkConfig.RootApplication) => IO ()
run = do
    databaseUrl <- appDatabaseUrl
    databaseConnection <- connectPostgreSQL databaseUrl 
    session <- Vault.newKey
    port <- FrameworkConfig.initAppPort
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let isDevelopment = Env.isDevelopment FrameworkConfig.environment
    let modelContext = ModelContext { databaseConnection, queryDebuggingEnabled = isDevelopment }
    let applicationContext = ApplicationContext { modelContext, session }
    let application :: Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp FrameworkConfig.RootApplication ErrorController.handleNotFound
            
    let sessionCookie = def
                { Cookie.setCookiePath = Just "/"
                , Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 30))
                , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                }
    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie session

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
                FrameworkConfig.requestLoggerMiddleware $
                        methodOverridePost $
                            application
