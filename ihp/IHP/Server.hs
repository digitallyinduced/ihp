{-# LANGUAGE IncoherentInstances #-}

module IHP.Server (run, application) where
import IHP.Prelude
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Systemd as Systemd
import Network.Wai
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Session (withSession)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Network.Wai.Middleware.HealthCheckEndpoint as HealthCheckEndpoint
import qualified Web.ClientSession as ClientSession
import IHP.Controller.Session (sessionVaultKey)
import qualified IHP.Environment as Env
import qualified IHP.PGListener as PGListener

import IHP.FrameworkConfig
import IHP.RouterSupport (frontControllerToWAIApp, FrontController)
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.Job.Runner as Job
import qualified IHP.Job.Types as Job
import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Approot as Approot
import qualified Network.Wai.Middleware.AssetPath as AssetPath

import qualified System.Directory as Directory
import qualified GHC.IO.Encoding as IO
import qualified System.IO as IO

import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified IHP.EnvVar as EnvVar
import qualified Network.Wreq as Wreq
import qualified Data.Function as Function
import IHP.RequestVault

import IHP.Controller.NotFound (handleNotFound)
import Paths_ihp (getDataFileName)

run :: (FrontController RootApplication, Job.Worker RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    -- We cannot use 'Main.Utf8.withUtf8' here, as this for some reason breaks live reloading
    -- in the dev server. So we switch the file handles to utf8 manually
    IO.setLocaleEncoding IO.utf8

    withFrameworkConfig configBuilder \frameworkConfig -> do
        IHP.FrameworkConfig.withModelContext frameworkConfig \modelContext -> do
            approotMiddleware <- Approot.envFallback
            assetPathMiddleware <- AssetPath.assetPathFromEnvMiddleware
                    -- | The asset version is used for cache busting
                    --
                    -- If you deploy IHP on your own, you should provide the IHP_ASSET_VERSION
                    -- env variable with e.g. the git commit hash of the production build.
                    --
                    -- If IHP cannot figure out an asset version, it will fallback to the static
                    -- string @"dev"@.
                    --
                    "IHP_ASSET_VERSION"
                    -- | Base URL used by the 'assetPath' helper. Useful to move your static files to a CDN                    
                    "IHP_ASSET_BASEURL"

            withInitalizers frameworkConfig modelContext do
                PGListener.withPGListener modelContext \pgListener -> do
                    autoRefreshMiddleware <- AutoRefresh.initAutoRefreshMiddleware pgListener

                    let ?modelContext = modelContext

                    sessionMiddleware <- initSessionMiddleware frameworkConfig
                    staticApp <- initStaticApp frameworkConfig
                    let corsMiddleware = initCorsMiddleware frameworkConfig
                    let requestLoggerMiddleware = frameworkConfig.requestLoggerMiddleware
                    let CustomMiddleware customMiddleware = frameworkConfig.customMiddleware

                    useSystemd <- EnvVar.envOrDefault "IHP_SYSTEMD" False

                    withBackgroundWorkers pgListener frameworkConfig
                        . runServer frameworkConfig useSystemd
                        . (if useSystemd then HealthCheckEndpoint.healthCheck else Function.id)
                        . customMiddleware
                        . corsMiddleware
                        . methodOverridePost
                        . sessionMiddleware
                        . approotMiddleware
                        . autoRefreshMiddleware
                        . modelContextMiddleware modelContext
                        . frameworkConfigMiddleware frameworkConfig
                        . pgListenerMiddleware pgListener
                        . assetPathMiddleware
                        $ application staticApp requestLoggerMiddleware

{-# INLINABLE run #-}

withBackgroundWorkers :: (Job.Worker RootApplication, ?modelContext :: ModelContext) => PGListener.PGListener -> FrameworkConfig -> IO () -> IO ()
withBackgroundWorkers pgListener frameworkConfig app = do
    let jobWorkers = Job.workers RootApplication
    let isDevelopment = frameworkConfig.environment == Env.Development
    if isDevelopment && not (isEmpty jobWorkers)
            then race_ (Job.devServerMainLoop frameworkConfig pgListener jobWorkers) app
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
    frameworkStaticDir <- getDataFileName "static"
    appStaticDir <- EnvVar.envOrDefault "APP_STATIC" "static/"
    let
        maxAge = case frameworkConfig.environment of
            Env.Development -> Static.MaxAgeSeconds 0
            Env.Production -> Static.MaxAgeForever

        frameworkSettings = (Static.defaultWebAppSettings frameworkStaticDir)
                { Static.ss404Handler = Just (frameworkConfig.requestLoggerMiddleware handleNotFound)
                , Static.ssMaxAge = maxAge
                }
        appSettings = (Static.defaultWebAppSettings appStaticDir)
                { Static.ss404Handler = Just (Static.staticApp frameworkSettings)
                , Static.ssMaxAge = maxAge
                }

    pure (Static.staticApp appSettings)

initSessionMiddleware :: FrameworkConfig -> IO Middleware
initSessionMiddleware FrameworkConfig { sessionCookie } = do
    let path = "Config/client_session_key.aes"

    hasSessionSecretEnvVar <- EnvVar.hasEnvVar "IHP_SESSION_SECRET"
    hasSessionSecretFileEnvVar <- EnvVar.hasEnvVar "IHP_SESSION_SECRET_FILE"
    doesConfigDirectoryExist <- Directory.doesDirectoryExist "Config"
    store <- clientsessionStore <$>
            if hasSessionSecretFileEnvVar
                then do
                    path <- EnvVar.env "IHP_SESSION_SECRET_FILE"
                    ClientSession.getKey path
                else
                    if hasSessionSecretEnvVar || not doesConfigDirectoryExist
                        then ClientSession.getKeyEnv "IHP_SESSION_SECRET"
                        else ClientSession.getKey path
    let sessionMiddleware :: Middleware = withSession store "SESSION" sessionCookie sessionVaultKey
    pure sessionMiddleware

initCorsMiddleware :: FrameworkConfig -> Middleware
initCorsMiddleware FrameworkConfig { corsResourcePolicy } = case corsResourcePolicy of
        Just corsResourcePolicy -> Cors.cors (const (Just corsResourcePolicy))
        Nothing -> id

application :: (FrontController RootApplication) => Application -> Middleware -> Application
application staticApp middleware request respond = do
    frontControllerToWAIApp @RootApplication @AutoRefresh.AutoRefreshWSApp middleware RootApplication staticApp request respond
{-# INLINABLE application #-}

runServer :: FrameworkConfig -> Bool -> Application -> IO ()
runServer config@FrameworkConfig { environment = Env.Development, appPort } useSystemd = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setBeforeMainLoop (do
                            ByteString.putStrLn "Server started"
                            IO.hFlush IO.stdout
                        )
                    |> Warp.setPort appPort
runServer FrameworkConfig { environment = Env.Production, appPort, exceptionTracker } useSystemd =
    let
        warpSettings =  Warp.defaultSettings
            |> Warp.setPort appPort
            |> Warp.setOnException exceptionTracker.onException
            |> Warp.setGracefulShutdownTimeout (Just 6)
            |> Warp.setFdCacheDuration (5 * 60)
            |> Warp.setFileInfoCacheDuration (5 * 60)
        heartbeatCheck = do
                response <- Wreq.get ("http://127.0.0.1:" <> cs (show appPort) <> "/_healthz")
                pure ()
        systemdSettings = Systemd.defaultSystemdSettings
            |> Systemd.setRequireSocketActivation True
            |> Systemd.setHeartbeatInterval (Just 30)
            |> Systemd.setHeartbeatCheck heartbeatCheck
    in
        if useSystemd
            then Systemd.runSystemdWarp systemdSettings warpSettings
            else Warp.runSettings warpSettings

withInitalizers :: FrameworkConfig -> ModelContext -> IO () -> IO ()
withInitalizers frameworkConfig modelContext continue = do
        let ?context = frameworkConfig
        let ?modelContext = modelContext
        withInitalizers' frameworkConfig.initializers
    where
        withInitalizers' :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => [Initializer] -> IO ()
        withInitalizers' (Initializer { onStartup } : rest) = withAsync onStartup (\async -> link async >> withInitalizers' rest)
        withInitalizers' [] = continue
