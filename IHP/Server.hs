{-# LANGUAGE IncoherentInstances #-}

module IHP.Server (run, application) where
import IHP.Prelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import IHP.ApplicationContext
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.Environment as Env
import qualified IHP.PGListener as PGListener

import IHP.FrameworkConfig
import IHP.RouterSupport (frontControllerToWAIApp, FrontController, webSocketApp, webSocketAppWithCustomPath)
import qualified IHP.ErrorController as ErrorController
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.AutoRefresh.Types as AutoRefresh
import IHP.LibDir
import qualified IHP.Job.Runner as Job
import qualified IHP.Job.Types as Job
import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Control.Exception as Exception

import qualified System.Environment as Env
import qualified System.Directory as Directory
import qualified GHC.IO.Encoding as IO
import qualified System.IO as IO

import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static

import IHP.Controller.NotFound (handleNotFound)

run :: (FrontController RootApplication, Job.Worker RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    -- We cannot use 'Main.Utf8.withUtf8' here, as this for some reason breaks live reloading
    -- in the dev server. So we switch the file handles to utf8 manually
    IO.setLocaleEncoding IO.utf8

    withFrameworkConfig configBuilder \frameworkConfig -> do
        modelContext <- IHP.FrameworkConfig.initModelContext frameworkConfig
        let withPGListener = Exception.bracket (PGListener.init modelContext) PGListener.stop

        withInitalizers frameworkConfig modelContext do
            withPGListener \pgListener -> do
                sessionVault <- Vault.newKey

                autoRefreshServer <- newIORef (AutoRefresh.newAutoRefreshServer pgListener)

                let ?modelContext = modelContext
                let ?applicationContext = ApplicationContext { modelContext = ?modelContext, session = sessionVault, autoRefreshServer, frameworkConfig, pgListener }

                sessionMiddleware <- initSessionMiddleware sessionVault frameworkConfig
                staticApp <- initStaticApp frameworkConfig
                let corsMiddleware = initCorsMiddleware frameworkConfig
                let requestLoggerMiddleware = frameworkConfig.requestLoggerMiddleware
                let CustomMiddleware customMiddleware = frameworkConfig.customMiddleware

                withBackgroundWorkers pgListener frameworkConfig
                    . runServer frameworkConfig
                    . customMiddleware
                    . corsMiddleware
                    . sessionMiddleware
                    . requestLoggerMiddleware
                    . methodOverridePost
                    $ application staticApp

{-# INLINABLE run #-}

withBackgroundWorkers :: (Job.Worker RootApplication, ?modelContext :: ModelContext) => PGListener.PGListener -> FrameworkConfig -> IO a -> IO a
withBackgroundWorkers pgListener frameworkConfig app = do
    let jobWorkers = Job.workers RootApplication
    let isDevelopment = frameworkConfig.environment == Env.Development
    if isDevelopment && not (isEmpty jobWorkers)
            then withAsync (Job.devServerMainLoop frameworkConfig pgListener jobWorkers) (const app)
            else app

-- | Returns a WAI app that servers files stored in the app's @static/@ directory and IHP's own @static/@  directory
--
-- HTTP Cache headers are set automatically. This includes Cache-Control, Last-Mofified and ETag
--
-- The cache strategy works like this:
-- - In dev mode we disable the browser cache for the app's @static/@ directory to make sure that always the latest CSS and JS is used
-- - In production mode: We cache files forever. IHP's 'assetPath' helper will add a hash to files to cache bust when something has changed.
initStaticApp :: FrameworkConfig -> IO Application
initStaticApp frameworkConfig = do
    libDir <- cs <$> findLibDirectory

    let
        maxAge = case frameworkConfig.environment of
            Env.Development -> Static.MaxAgeSeconds 0
            Env.Production -> Static.MaxAgeForever


        frameworkStaticDir = libDir <> "/static/"
        frameworkSettings = (Static.defaultWebAppSettings frameworkStaticDir)
                { Static.ss404Handler = Just handleNotFound
                , Static.ssMaxAge = maxAge
                }
        appSettings = (Static.defaultWebAppSettings "static/")
                { Static.ss404Handler = Just (Static.staticApp frameworkSettings)
                , Static.ssMaxAge = maxAge
                }

    pure (Static.staticApp appSettings)

initSessionMiddleware :: Vault.Key (Session IO ByteString ByteString) -> FrameworkConfig -> IO Middleware
initSessionMiddleware sessionVault FrameworkConfig { sessionCookie } = do
    let path = "Config/client_session_key.aes"

    hasSessionSecretEnvVar <- isJust <$> Env.lookupEnv "IHP_SESSION_SECRET"
    doesConfigDirectoryExist <- Directory.doesDirectoryExist "Config"
    store <- clientsessionStore <$>
            if hasSessionSecretEnvVar || not doesConfigDirectoryExist
                then ClientSession.getKeyEnv "IHP_SESSION_SECRET"
                else ClientSession.getKey path
    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie sessionVault
    pure sessionMiddleware

initCorsMiddleware :: FrameworkConfig -> Middleware
initCorsMiddleware FrameworkConfig { corsResourcePolicy } = case corsResourcePolicy of
        Just corsResourcePolicy -> Cors.cors (const (Just corsResourcePolicy))
        Nothing -> id

application :: (FrontController RootApplication, ?applicationContext :: ApplicationContext) => Application -> Application
application staticApp request respond = do
        requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
        let ?context = requestContext
        let builtinControllers = let ?application = () in
                [ webSocketApp @AutoRefresh.AutoRefreshWSApp
                , webSocketAppWithCustomPath @AutoRefresh.AutoRefreshWSApp "" -- For b.c. with older versions of ihp-auto-refresh.js
                ]

        frontControllerToWAIApp RootApplication builtinControllers (staticApp request respond)
{-# INLINABLE application #-}

runServer :: (?applicationContext :: ApplicationContext) => FrameworkConfig -> Application -> IO ()
runServer config@FrameworkConfig { environment = Env.Development, appPort } = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setBeforeMainLoop (do
                            ByteString.putStrLn "Server started"
                            IO.hFlush IO.stdout
                        )
                    |> Warp.setPort appPort
runServer FrameworkConfig { environment = Env.Production, appPort, exceptionTracker } = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setPort appPort
                    |> Warp.setOnException exceptionTracker.onException

instance ControllerSupport.InitControllerContext () where
    initContext = pure ()

withInitalizers :: FrameworkConfig -> ModelContext -> _ -> IO ()
withInitalizers frameworkConfig modelContext continue = do
        let ?context = frameworkConfig
        let ?modelContext = modelContext
        withInitalizers' frameworkConfig.initializers
    where
        withInitalizers' :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => [Initializer] -> IO ()
        withInitalizers' (Initializer { onStartup } : rest) = withAsync onStartup (\async -> link async >> withInitalizers' rest)
        withInitalizers' [] = continue
