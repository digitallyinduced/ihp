module IHP.IDE.ToolServer where

import IHP.Prelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import IHP.HaskellSupport
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.Types
import IHP.IDE.PortConfig
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.ErrorController as ErrorController
import IHP.ApplicationContext
import IHP.ModelSupport
import IHP.RouterSupport hiding (get)
import qualified Web.Cookie as Cookie
import Network.Wai.Session.Map (mapStore_)
import qualified Data.Time.Clock
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.Wai.Session (withSession, Session)
import qualified System.Directory as Directory

import qualified IHP.FrameworkConfig as Config
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.Controller.EnumValues
import IHP.IDE.SchemaDesigner.Controller.Enums
import IHP.IDE.SchemaDesigner.Controller.Columns
import IHP.IDE.SchemaDesigner.Controller.Schema
import IHP.IDE.SchemaDesigner.Controller.Tables
import IHP.IDE.Data.Controller
import IHP.IDE.Logs.Controller
import IHP.IDE.CodeGen.Controller
import IHP.IDE.ToolServer.Types
import Control.Concurrent.Async
import IHP.IDE.ToolServer.Routes
import qualified System.Process as Process
import System.Info
import qualified System.Environment as Env

startToolServer :: (?context :: Context) => IO ()
startToolServer = do
    let port = ?context
            |> get #portConfig
            |> get #toolServerPort
            |> fromIntegral

    let isDebugMode = ?context |> get #isDebugMode

    thread <- async (startToolServer' port isDebugMode)

    dispatch (UpdateToolServerState (ToolServerStarted { thread }))
    
startToolServer' port isDebugMode = do
    writeIORef Config.portRef port

    session <- Vault.newKey
    store <- case os of
        "linux" -> mapStore_
        _ -> fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let sessionCookie = def
                { Cookie.setCookiePath = Just "/"
                , Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 30))
                , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                }
    let sessionMiddleware :: Wai.Middleware = withSession store "SESSION" sessionCookie session    
    let applicationContext = ApplicationContext { modelContext = (ModelContext (error "Not connected")), session }
    let toolServerApplication = ToolServerApplication { devServerContext = ?context }
    let application :: Wai.Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp toolServerApplication ErrorController.handleNotFound
            
    libDirectory <- cs <$> Config.findLibDirectory
    let staticMiddleware :: Wai.Middleware = staticPolicy (addBase (libDirectory <> "static/"))

    let openAppUrl = openUrl ("http://localhost:" <> tshow port <> "/")
    let warpSettings = Warp.defaultSettings
            |> Warp.setPort port
            |> Warp.setBeforeMainLoop openAppUrl

    let logMiddleware = if isDebugMode then logStdoutDev else IHP.Prelude.id
    
    Warp.runSettings warpSettings $ 
            staticMiddleware $ logMiddleware $ methodOverridePost $ sessionMiddleware $ application

stopToolServer ToolServerStarted { thread } = uninterruptibleCancel thread
stopToolServer ToolServerNotStarted = pure ()

openUrl :: Text -> IO ()
openUrl url = do
    selectedBrowser <- Env.lookupEnv "IHP_BROWSER"
    let defaultOSBrowser = case os of
            "linux" -> "xdg-open"
            "darwin" -> "open"
    let browser = selectedBrowser |> fromMaybe defaultOSBrowser
    async $ Process.callCommand (browser <> " " <> cs url)
    pure ()

instance FrontController ToolServerApplication where
    controllers =
        [ parseRoute @SchemaController
        , parseRoute @TablesController
        , parseRoute @ColumnsController
        , parseRoute @EnumsController
        , parseRoute @EnumValuesController
        , parseRoute @LogsController
        , parseRoute @DataController
        , parseRoute @CodeGenController
        , startPage TablesAction
        ]

instance ControllerSupport.InitControllerContext ToolServerApplication where