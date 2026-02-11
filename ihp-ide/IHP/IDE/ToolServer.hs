module IHP.IDE.ToolServer (runToolServer, withToolServerApplication, ToolServerApplicationWithConfig(..)) where

import IHP.Prelude
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import IHP.IDE.Types
import IHP.IDE.PortConfig
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import IHP.RouterSupport hiding (get)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
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
import IHP.IDE.SchemaDesigner.Controller.Migrations ()
import IHP.IDE.SchemaDesigner.Controller.Indexes ()
import IHP.IDE.Data.Controller ()
import IHP.IDE.Logs.Controller ()
import IHP.IDE.CodeGen.Controller ()
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Helper.Controller as Helper
import IHP.IDE.ToolServer.Routes ()
import qualified System.Process as Process
import System.Info
import qualified IHP.EnvVar as EnvVar
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.IDE.ToolServer.Layout as Layout
import IHP.Controller.Layout
import qualified IHP.IDE.LiveReloadNotificationServer as LiveReloadNotificationServer
import qualified IHP.Version as Version
import qualified Control.Exception.Safe as Exception

import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Middleware.Approot as Approot
import qualified WaiAppStatic.Types as Static
import IHP.Controller.NotFound (handleNotFound)
import IHP.Controller.Session (sessionVaultKey)
import Paths_ihp_ide (getDataFileName)
import IHP.RequestVault
import qualified Data.Vault.Lazy as Vault
import IHP.Controller.Response (responseHeadersVaultKey)
import IHP.ControllerSupport (rlsContextVaultKey)
import Wai.Request.Params.Middleware (requestBodyMiddleware)

runToolServer :: (?context :: Context) => ToolServerApplication -> _ -> IO ()
runToolServer toolServerApplication liveReloadClients = do
    let port = ?context.portConfig.toolServerPort |> fromIntegral
    let isDebugMode = ?context.isDebugMode

    startToolServer' toolServerApplication port isDebugMode liveReloadClients

startToolServer' :: (?context :: Context) => ToolServerApplication -> Int -> Bool -> _ -> IO ()
startToolServer' toolServerApplication port isDebugMode liveReloadClients = do
    withToolServerApplication toolServerApplication port liveReloadClients \weightedApp -> do
        let openAppUrl = openUrl ("http://localhost:" <> tshow port <> "/")
        let warpSettings = Warp.defaultSettings
                |> Warp.setPort port
                |> Warp.setBeforeMainLoop openAppUrl

        let logMiddleware = if isDebugMode then weightedApp.frameworkConfig.requestLoggerMiddleware else IHP.Prelude.id

        Warp.runSettings warpSettings (logMiddleware weightedApp.application)

-- | Result of building the ToolServer application
data ToolServerApplicationWithConfig = ToolServerApplicationWithConfig
    { application :: Wai.Application
    , frameworkConfig :: Config.FrameworkConfig
    }

-- | Builds the full ToolServer WAI application with all middlewares applied
-- and runs the given action with it. The model context is released when the
-- action completes.
--
-- The application includes:
-- - methodOverridePost (for PUT/DELETE via POST)
-- - sessionMiddleware (for session handling)
-- - approotMiddleware (for app root path)
-- - viewLayoutMiddleware (for setLayout/getLayout support)
-- - frameworkConfigMiddleware (for framework config in request vault)
-- - requestBodyMiddleware (for parsing form params - required for controllers)
-- - websocket support (for live reload)
withToolServerApplication :: ToolServerApplication -> Int -> _ -> (ToolServerApplicationWithConfig -> IO result) -> IO result
withToolServerApplication toolServerApplication port liveReloadClients action = do
    frameworkConfig <- Config.buildFrameworkConfig do
        Config.option $ Config.AppHostname "localhost"
        Config.option $ Config.AppPort port
        Config.option $ Config.AssetVersion Version.ihpVersion

        ihpIdeBaseUrlEnvVar <- EnvVar.envOrNothing "IHP_IDE_BASEURL"
        case ihpIdeBaseUrlEnvVar of
            Just baseUrl -> Config.option $ Config.BaseUrl baseUrl
            Nothing -> pure ()

    withModelContext frameworkConfig.databaseUrl frameworkConfig.logger \modelContext -> do
        store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
        let sessionMiddleware :: Wai.Middleware = withSession store "SESSION" (frameworkConfig.sessionCookie) sessionVaultKey

        approotMiddleware <- Approot.envFallbackNamed "IDE_APPROOT"

        staticApp <- initStaticApp

        let innerApplication :: Wai.Application = \request respond -> do
                frontControllerToWAIApp @ToolServerApplication @AutoRefresh.AutoRefreshWSApp (\app -> app) toolServerApplication staticApp request respond

        let responseHeadersMiddleware = insertNewIORefVaultMiddleware responseHeadersVaultKey []
        let rlsContextMiddleware = insertNewIORefVaultMiddleware rlsContextVaultKey Nothing

        let toolServerVaultMiddleware app req respond = do
                availableApps <- AvailableApps <$> findApplications
                webControllers <- WebControllers <$> findWebControllers
                let defaultAppUrl = "http://localhost:" <> tshow toolServerApplication.appPort
                appUrl <- AppUrl <$> EnvVar.envOrDefault "IHP_BASEURL" defaultAppUrl
                databaseNeedsMigration <- DatabaseNeedsMigration <$> readIORef toolServerApplication.databaseNeedsMigration
                let req' = req { Wai.vault = Vault.insert availableAppsVaultKey availableApps
                                       . Vault.insert webControllersVaultKey webControllers
                                       . Vault.insert appUrlVaultKey appUrl
                                       . Vault.insert databaseNeedsMigrationVaultKey databaseNeedsMigration
                                       $ req.vault }
                app req' respond

        let application =
                methodOverridePost $ sessionMiddleware $ approotMiddleware
                    $ viewLayoutMiddleware
                    $ responseHeadersMiddleware
                    $ rlsContextMiddleware
                    $ toolServerVaultMiddleware
                    $ modelContextMiddleware modelContext
                    $ frameworkConfigMiddleware frameworkConfig
                    $ requestBodyMiddleware frameworkConfig.parseRequestBodyOptions
                    $ Websocket.websocketsOr
                        Websocket.defaultConnectionOptions
                        (LiveReloadNotificationServer.app liveReloadClients)
                        innerApplication

        action ToolServerApplicationWithConfig { application, frameworkConfig }

initStaticApp :: IO Wai.Application
initStaticApp = do
    toolServerStatic <- getDataFileName "static"
    ihpStatic <- EnvVar.env "IHP_STATIC"

    let ssMaxAge = Static.MaxAgeSeconds (60 * 60 * 24 * 30) -- 30 days
    let ihpStaticSettings = (Static.defaultWebAppSettings ihpStatic)
            { Static.ss404Handler = Just handleNotFound
            , Static.ssMaxAge = ssMaxAge
            }
    let toolServerStaticSettings = (Static.defaultWebAppSettings toolServerStatic)
            { Static.ss404Handler = Just (Static.staticApp ihpStaticSettings)
            , Static.ssMaxAge = ssMaxAge
            }
    pure (Static.staticApp toolServerStaticSettings)

openUrl :: Text -> IO ()
openUrl url = do
    selectedBrowser <- EnvVar.envOrNothing "IHP_BROWSER"
    let defaultOSBrowser = case os of
            "linux" -> "xdg-open"
            "darwin" -> "open"
            _ -> "xdg-open"
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
        , parseRoute @MigrationsController
        , parseRoute @IndexesController
        , startPage TablesAction
        ]

instance ControllerSupport.InitControllerContext ToolServerApplication where
    initContext = do
        setLayout Layout.toolServerLayout
