module Foundation.Server (run) where
import ClassyPrelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status (status404)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Web.ClientSession (getDefaultKey)
import qualified Data.Vault.Lazy as Vault
import Data.Default (def)
import Network.Wai.Session.Map (mapStore_)
import qualified Web.Cookie
import qualified Data.Time.Clock

import Foundation.ModelSupport
import Foundation.ApplicationContext
import qualified Foundation.ControllerSupport as ControllerSupport

import Database.PostgreSQL.Simple

import qualified Routes
import qualified Config
import qualified Foundation.LoginSupport.Middleware
import Unsafe.Coerce
import Foundation.Environment (isDevelopment)
import qualified System.Process as Process

import Foundation.RouterSupport (prepareWAIApp)

defaultPort :: Int
defaultPort = 8000



run :: IO ()
run = do
    putStrLn "Start"
    conn <- connectPostgreSQL Config.postgreSQLUrl
    session <- Vault.newKey
    store <- fmap clientsessionStore getDefaultKey
    let applicationContext = ApplicationContext (ModelContext conn) session
    let application :: Application = \request respond -> do
            theAction <- ControllerSupport.withContext (prepareWAIApp @Routes.RootApplication) applicationContext request respond
            theAction
    let sessionMiddleware :: Middleware = withSession store "SESSION" (def { Web.Cookie.setCookiePath = Just "/", Web.Cookie.setCookieMaxAge = Just ((unsafeCoerce (Data.Time.Clock.secondsToDiffTime 60 * 60 * 24 * 30))) }) session
    let logMiddleware :: Middleware = logStdoutDev
    let staticMiddleware :: Middleware = staticPolicy (addBase "static/") . staticPolicy (addBase "src/Foundation/static/")
    let frameworkMiddleware :: Middleware = Foundation.LoginSupport.Middleware.middleware applicationContext
    if isDevelopment Config.environment
        then pingDevServer
        else return ()
    Warp.runEnv defaultPort $
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
