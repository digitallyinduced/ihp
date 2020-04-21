module TurboHaskell.Server (run, appDatabaseUrl) where
import TurboHaskell.Prelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session.Map (mapStore_)
import qualified Web.Cookie as Cookie
import qualified Data.Time.Clock
import TurboHaskell.ModelSupport
import TurboHaskell.ApplicationContext
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import Database.PostgreSQL.Simple
import qualified TurboHaskell.LoginSupport.Middleware
import TurboHaskell.Environment (isDevelopment)

import qualified TurboHaskell.FrameworkConfig as FrameworkConfig
import TurboHaskell.FrameworkConfig (FrameworkConfig, appDatabaseUrl)
import TurboHaskell.RouterSupport (frontControllerToWAIApp, HasPath, CanRoute, FrontController)
import qualified TurboHaskell.ErrorController as ErrorController

run :: (FrameworkConfig, FrontController FrameworkConfig.RootApplication) => IO ()
run = do
    databaseUrl <- appDatabaseUrl
    conn <- connectPostgreSQL databaseUrl 
    session <- Vault.newKey
    port <- FrameworkConfig.initAppPort
    --store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    store <- mapStore_
    let applicationContext = ApplicationContext { modelContext = (ModelContext conn), session }
    let application :: Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp @FrameworkConfig.RootApplication ErrorController.handleNotFound
            
    let sessionCookie = def
                { Cookie.setCookiePath = Just "/"
                , Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 30))
                , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                }
    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie session
    let logMiddleware :: Middleware = logStdoutDev
    let staticMiddleware :: Middleware = staticPolicy (addBase "static/") . staticPolicy (addBase "TurboHaskell/TurboHaskell/static/")
    let runServer = if isDevelopment FrameworkConfig.environment
            then
                let settings = Warp.defaultSettings
                        |> Warp.setBeforeMainLoop (putStrLn "Server started")
                        |> Warp.setPort port
                in Warp.runSettings settings
            else Warp.runEnv port
    runServer $
        staticMiddleware $
            sessionMiddleware $
                logMiddleware $            
                        methodOverridePost $
                            application
