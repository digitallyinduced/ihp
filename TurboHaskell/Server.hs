module TurboHaskell.Server (run) where
import ClassyPrelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status (status404)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import Data.Default (def)
import Network.Wai.Session.Map (mapStore_)
import qualified Web.Cookie
import qualified Data.Time.Clock

import TurboHaskell.ModelSupport
import TurboHaskell.ApplicationContext
import qualified TurboHaskell.ControllerSupport as ControllerSupport

import Database.PostgreSQL.Simple


import qualified TurboHaskell.LoginSupport.Middleware
import Unsafe.Coerce
import TurboHaskell.Environment (isDevelopment)
import qualified System.Process as Process
import TurboHaskell.HaskellSupport
import qualified System.Environment as Environment
import System.Directory (getCurrentDirectory)
import Data.String.Conversions (cs)

import qualified TurboHaskell.FrameworkConfig as FrameworkConfig
import TurboHaskell.FrameworkConfig (FrameworkConfig)
import TurboHaskell.RouterSupport (frontControllerToWAIApp, HasPath, CanRoute, FrontController)

defaultPort :: Int
defaultPort = 8000

run :: (FrameworkConfig, FrontController FrameworkConfig.RootApplication) => IO ()
run = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    databaseUrl <- (Environment.lookupEnv "DATABASE_URL") >>= (return . maybe defaultDatabaseUrl cs )
    conn <- connectPostgreSQL databaseUrl 
    session <- Vault.newKey
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let applicationContext = ApplicationContext { modelContext = (ModelContext conn), session }
    let application :: Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp @FrameworkConfig.RootApplication
            
    let sessionMiddleware :: Middleware = withSession store "SESSION" (def { Web.Cookie.setCookiePath = Just "/", Web.Cookie.setCookieMaxAge = Just ((unsafeCoerce (Data.Time.Clock.secondsToDiffTime 60 * 60 * 24 * 30))) }) session
    let logMiddleware :: Middleware = logStdoutDev
    let staticMiddleware :: Middleware = staticPolicy (addBase "static/") . staticPolicy (addBase "TurboHaskell/TurboHaskell/static/")
    let frameworkMiddleware :: Middleware = TurboHaskell.LoginSupport.Middleware.middleware applicationContext
    let runServer = if isDevelopment FrameworkConfig.environment
            then
                let settings = Warp.defaultSettings
                    |> Warp.setBeforeMainLoop pingDevServer
                    |> Warp.setPort defaultPort
                in Warp.runSettings settings
            else Warp.runEnv defaultPort
    runServer $
        staticMiddleware $
            sessionMiddleware $
                logMiddleware $            
                     frameworkMiddleware $
                        methodOverridePost $
                            application

pingDevServer :: IO ()
pingDevServer = do
    _ <- Process.system "(lsof -i :8002|awk '{print $2}'|tail -n1|xargs kill -SIGINT) || true"
    return ()
