module IHP.IDE.ToolServer where

import IHP.Prelude
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import IHP.IDE.Types
import IHP.IDE.PortConfig
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.ErrorController as ErrorController
import IHP.ApplicationContext
import IHP.ModelSupport
import IHP.RouterSupport hiding (get)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.Wai.Session (withSession)
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket

import qualified IHP.FrameworkConfig as Config
import IHP.IDE.SchemaDesigner.Controller.EnumValues ()
import IHP.IDE.SchemaDesigner.Controller.Enums ()
import IHP.IDE.SchemaDesigner.Controller.Columns ()
import IHP.IDE.SchemaDesigner.Controller.Policies ()
import IHP.IDE.SchemaDesigner.Controller.Schema ()
import IHP.IDE.SchemaDesigner.Controller.Tables ()
import IHP.IDE.Data.Controller ()
import IHP.IDE.Logs.Controller ()
import IHP.IDE.CodeGen.Controller ()
import IHP.IDE.Repl.Controller ()
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Helper.Controller as Helper
import IHP.IDE.ToolServer.Routes ()
import qualified System.Process as Process
import System.Info
import qualified System.Environment as Env
import qualified IHP.AutoRefresh.Types as AutoRefresh
import IHP.Controller.Context
import qualified IHP.IDE.ToolServer.Layout as Layout
import IHP.Controller.Layout
import qualified IHP.LibDir as LibDir
import qualified IHP.IDE.LiveReloadNotificationServer as LiveReloadNotificationServer
import qualified IHP.Version as Version
import qualified IHP.IDE.Types
import qualified IHP.PGListener as PGListener

startToolServer :: (?context :: Context) => IO ()
startToolServer = do
    let port = ?context
            |> get #portConfig
            |> get #toolServerPort
            |> fromIntegral

    let isDebugMode = ?context |> get #isDebugMode

    thread <- async (startToolServer' port isDebugMode)

    dispatch (UpdateToolServerState (ToolServerStarted { thread }))

startToolServer' :: (?context :: Context) => Int -> Bool -> IO ()
startToolServer' port isDebugMode = do

    frameworkConfig <- Config.buildFrameworkConfig do
        Config.option $ Config.AppHostname "localhost"
        Config.option $ Config.AppPort port
        Config.option $ Config.AssetVersion Version.ihpVersion

        ihpIdeBaseUrlEnvVar <- liftIO (Env.lookupEnv "IHP_IDE_BASEURL")
        case ihpIdeBaseUrlEnvVar of
            Just baseUrl -> Config.option $ Config.BaseUrl (cs baseUrl)
            Nothing -> pure ()

    session <- Vault.newKey
    store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    let sessionMiddleware :: Wai.Middleware = withSession store "SESSION" (get #sessionCookie frameworkConfig) session
    let modelContext = notConnectedModelContext undefined
    pgListener <- PGListener.init modelContext
    autoRefreshServer <- newIORef (AutoRefresh.newAutoRefreshServer pgListener)
    let applicationContext = ApplicationContext { modelContext, session, autoRefreshServer, frameworkConfig, pgListener }
    let toolServerApplication = ToolServerApplication { devServerContext = ?context }
    let application :: Wai.Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?context = requestContext
            frontControllerToWAIApp toolServerApplication [] ErrorController.handleNotFound

    libDirectory <- cs <$> LibDir.findLibDirectory
    let staticMiddleware :: Wai.Middleware = staticPolicy (addBase (libDirectory <> "static/"))

    let openAppUrl = openUrl ("http://localhost:" <> tshow port <> "/")
    let warpSettings = Warp.defaultSettings
            |> Warp.setPort port
            |> Warp.setBeforeMainLoop openAppUrl

    let logMiddleware = if isDebugMode then get #requestLoggerMiddleware frameworkConfig else IHP.Prelude.id

    liveReloadNotificationServerState <- ?context
            |> get #appStateRef
            |> readIORef
            >>= pure . get #liveReloadNotificationServerState

    Warp.runSettings warpSettings $
            staticMiddleware $ logMiddleware $ methodOverridePost $ sessionMiddleware application
                -- $ Websocket.websocketsOr
                --     Websocket.defaultConnectionOptions
                --     (LiveReloadNotificationServer.app liveReloadNotificationServerState)
                --     application

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
        , parseRoute @PoliciesController
        , parseRoute @EnumsController
        , parseRoute @EnumValuesController
        , parseRoute @LogsController
        , parseRoute @DataController
        , parseRoute @CodeGenController
        , parseRoute @ReplController
        , webSocketApp @ReplWSApp
        , webSocketApp @ReplOutputApp
        , startPage TablesAction
        ]

instance ControllerSupport.InitControllerContext ToolServerApplication where
    initContext = do
        availableApps <- AvailableApps <$> findApplications
        webControllers <- WebControllers <$> findWebControllers

        let defaultAppUrl = "http://localhost:" <> tshow Helper.appPort
        appUrl :: Text <- fromMaybe defaultAppUrl <$> fmap cs <$> Env.lookupEnv "IHP_BASEURL"

        putContext availableApps
        putContext webControllers
        putContext (AppUrl appUrl)
        setLayout Layout.toolServerLayout

        databaseNeedsMigration <- readDatabaseNeedsMigration
        putContext (DatabaseNeedsMigration databaseNeedsMigration)


readDatabaseNeedsMigration :: (?context :: ControllerContext) => IO Bool
readDatabaseNeedsMigration = do
    context <- theDevServerContext
    state <- readIORef (get #appStateRef context)
    readIORef (get #databaseNeedsMigration state)
