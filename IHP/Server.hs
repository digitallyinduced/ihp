{-# LANGUAGE IncoherentInstances #-}

module IHP.Server (run, application) where
import IHP.Prelude
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import IHP.ModelSupport
import IHP.ApplicationContext
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.Environment as Env
import IHP.Log.Types

import IHP.FrameworkConfig
import IHP.RouterSupport (frontControllerToWAIApp, FrontController, webSocketApp, webSocketAppWithCustomPath)
import qualified IHP.ErrorController as ErrorController
import Control.Exception (finally)
import qualified IHP.AutoRefresh as AutoRefresh
import qualified IHP.AutoRefresh.Types as AutoRefresh
import IHP.LibDir
import qualified IHP.Job.Runner as Job
import qualified IHP.Job.Types as Job
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Control.Exception as Exception

import qualified System.Environment as Env
import qualified System.Directory as Directory

run :: (FrontController RootApplication, Job.Worker RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    frameworkConfig <- buildFrameworkConfig configBuilder

    sessionVault <- Vault.newKey
    autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
    modelContext <- initModelContext frameworkConfig

    let ?modelContext = modelContext
    let ?applicationContext = ApplicationContext { modelContext = ?modelContext, session = sessionVault, autoRefreshServer, frameworkConfig }

    sessionMiddleware <- initSessionMiddleware sessionVault frameworkConfig
    staticMiddleware <- initStaticMiddleware frameworkConfig
    let corsMiddleware = initCorsMiddleware frameworkConfig
    let requestLoggerMiddleware = get #requestLoggerMiddleware frameworkConfig

    let run = withBackgroundWorkers frameworkConfig $
           runServer frameworkConfig $
                staticMiddleware $
                    corsMiddleware $
                        sessionMiddleware $
                                requestLoggerMiddleware $
                                        methodOverridePost $
                                            application

    run `finally` do
        frameworkConfig |> get #logger |> get #cleanup
        AutoRefresh.stopAutoRefreshServer autoRefreshServer

{-# INLINABLE run #-}

withBackgroundWorkers :: (Job.Worker RootApplication, ?modelContext :: ModelContext) => FrameworkConfig -> IO a -> IO a
withBackgroundWorkers frameworkConfig app = do
    let jobWorkers = Job.workers RootApplication
    let isDevelopment = get #environment frameworkConfig == Env.Development
    if isDevelopment && not (isEmpty jobWorkers)
            then do
                workerAsync <- async (let ?context = frameworkConfig in Job.runJobWorkersKillOnExit jobWorkers)
                Async.link workerAsync
                app
            else app

-- | Returns a middleware that returns files stored in the app's @static/@ directory and IHP's own @static/@  directory
--
-- HTTP Cache headers are set automatically. This includes Cache-Control, Last-Mofified and ETag
--
-- The cache strategy works like this:
-- - In dev mode we disable the browser cache for the app's @static/@ directory to make sure that always the latest CSS and JS is used
-- - In production mode: If the files are stored in @static/vendor@/ we cache up to 30 days. For all other files we cache up to one day. It's best for vendor files to have e.g. the version as part of the file name. So @static/vendor/jquery.js@ should become @static/vendor/jquery-3.6.1.js@. That way when updating jquery you will have no issues with the cache.
-- - Static files in IHP's @static/@ directory can be cached up to 30 days
initStaticMiddleware :: FrameworkConfig -> IO Middleware
initStaticMiddleware FrameworkConfig { environment } = do
        libDirectory <- cs <$> findLibDirectory

        -- We have different caching rules for the app `static/` directory and the IHP `static/` directory
        appStaticCache <- initCaching (CustomCaching getAppCacheHeader)
        ihpStaticCache <- initCaching (CustomCaching getIHPCacheHeader)
        let appCachingOptions = defaultOptions { cacheContainer = appStaticCache }
        let ihpCachingOptions = defaultOptions { cacheContainer = ihpStaticCache }

        let middleware =
                      staticPolicyWithOptions appCachingOptions (addBase "static/")
                    . staticPolicyWithOptions ihpCachingOptions (addBase (libDirectory <> "static/"))
        pure middleware
    where
        -- In dev mode we disable the browser cache to make sure that always the latest CSS and JS is used
        -- In production mode we cache static assets up to one day
        getAppCacheHeader fileMeta =
            case environment of
                Env.Development -> [("Cache-Control", "no-cache,no-store,must-revalidate")]
                Env.Production ->
                    let
                        -- Cache file in `static/vendor` for one month. Code that is stored in `vendor` should
                        -- have the version number in it's file name. So `static/vendor/jquery.js` should become
                        -- `static/vendor/jquery-3.6.1.js`. That way when updating jquery you will have no issues
                        -- with the cache.
                        isVendorFile = "static/vendor/" `List.isPrefixOf` (fm_fileName fileMeta)
                        vendorCacheControl = ("Cache-Control", "no-transform,public,max-age=2592000,s-maxage=2592000")
                        -- All other app files are cached for a day
                        appCacheControl = ("Cache-Control", "no-transform,public,max-age=86400,s-maxage=86400")
                    in
                        [ if isVendorFile then vendorCacheControl else appCacheControl
                        , ("Last-Modified", fm_lastModified fileMeta)
                        , ("ETag", fm_etag fileMeta)
                        , ("Vary", "Accept-Encoding")
                        ]

        -- Files in IHP's own static directory are cached for one month
        getIHPCacheHeader fileMeta =
                    [ ("Cache-Control", "no-transform,public,max-age=2592000,s-maxage=2592000")
                    , ("Last-Modified", fm_lastModified fileMeta)
                    , ("ETag", fm_etag fileMeta)
                    , ("Vary", "Accept-Encoding")
                    ]

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

initModelContext :: FrameworkConfig -> IO ModelContext
initModelContext FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl, logger } = do
    let isDevelopment = environment == Env.Development
    modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger
    pure modelContext

application :: (FrontController RootApplication, ?applicationContext :: ApplicationContext) => Application
application request respond = do
        requestContext <- ControllerSupport.createRequestContext ?applicationContext request respond
        let ?context = requestContext
        let builtinControllers = let ?application = () in
                [ webSocketApp @AutoRefresh.AutoRefreshWSApp
                , webSocketAppWithCustomPath @AutoRefresh.AutoRefreshWSApp "" -- For b.c. with older versions of ihp-auto-refresh.js
                ]
        frontControllerToWAIApp RootApplication builtinControllers ErrorController.handleNotFound

runServer :: (?applicationContext :: ApplicationContext) => FrameworkConfig -> Application -> IO ()
runServer config@FrameworkConfig { environment = Env.Development, appPort } = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setBeforeMainLoop (ByteString.putStrLn "Server started")
                    |> Warp.setPort appPort
runServer FrameworkConfig { environment = Env.Production, appPort, exceptionTracker } = Warp.runSettings $
                Warp.defaultSettings
                    |> Warp.setPort appPort
                    |> Warp.setOnException (get #onException exceptionTracker)

instance ControllerSupport.InitControllerContext () where
    initContext = pure ()
