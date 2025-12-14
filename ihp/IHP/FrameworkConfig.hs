{-# LANGUAGE ConstraintKinds #-}
module IHP.FrameworkConfig where

import IHP.Prelude
import System.Directory (getCurrentDirectory)
import IHP.Environment
import qualified Data.Text as Text
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Web.Cookie as Cookie
import IHP.Mail.Types
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified Data.Typeable as Typeable
import IHP.View.Types
import IHP.View.CSSFramework
import IHP.Log.Types
import IHP.Log (makeRequestLogger, defaultRequestLogger)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Parse as WaiParse
import qualified Control.Exception as Exception
import IHP.ModelSupport
import IHP.EnvVar

import qualified Prelude
import qualified GHC.Stack as Stack

import qualified Control.Concurrent as Concurrent

newtype AppHostname = AppHostname Text
newtype AppPort = AppPort Int
newtype BaseUrl = BaseUrl Text

-- | Provides IHP with a middleware to log requests and responses.
--
-- By default this uses the RequestLogger middleware from wai-extra. Take a look at the wai-extra
-- documentation when you want to customize the request logging.
--
-- See https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html
--
--
-- Set @requestLoggerMiddleware = \application -> application@ to disable request logging.
newtype RequestLoggerMiddleware = RequestLoggerMiddleware Middleware

-- | Provides the default settings for the session cookie.
--
-- - Max Age: 30 days
-- - Same Site Policy: Lax
-- - HttpOnly (no access through JS)
-- - secure, when baseUrl is a https url
--
-- Override this to set e.g. a custom max age or change the default same site policy.
--
-- __Example: Set max age to 90 days__
-- > sessionCookie = defaultIHPSessionCookie { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 90)) }
newtype SessionCookie = SessionCookie Cookie.SetCookie

-- | How long db connection are kept alive inside the connecton pool when they're idle
newtype DBPoolIdleTime = DBPoolIdleTime NominalDiffTime

-- | Max number of db connections the connection pool can open to the database
newtype DBPoolMaxConnections = DBPoolMaxConnections Int

newtype DatabaseUrl = DatabaseUrl ByteString

type ConfigBuilder = State.StateT TMap.TMap IO ()

-- | Interface for exception tracking services such as sentry
newtype ExceptionTracker = ExceptionTracker { onException :: Maybe Request -> SomeException -> IO () }

-- | Typically "http://localhost:8001", Url where the IDE is running
newtype IdeBaseUrl = IdeBaseUrl Text

-- | Postgres role to be used for making queries with Row level security enabled
newtype RLSAuthenticatedRole = RLSAuthenticatedRole Text

newtype AssetVersion = AssetVersion Text

newtype CustomMiddleware = CustomMiddleware Middleware

newtype DataSyncMaxSubscriptionsPerConnection = DataSyncMaxSubscriptionsPerConnection Int
newtype DataSyncMaxTransactionsPerConnection = DataSyncMaxTransactionsPerConnection Int

newtype Initializer = Initializer { onStartup :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => IO () }

-- | Puts an option into the current configuration
--
-- In case an option already exists with the same type, it will not be overriden:
--
-- > option Production
-- > option Development
-- > findOption @Environment
--
-- This code will return 'Production' as the second call to 'option' is ignored to not override the existing option.
option :: forall option. Typeable option => option -> State.StateT TMap.TMap IO ()
option !value = State.modify (\map -> if TMap.member @option map then map else TMap.insert value map)
{-# INLINABLE option #-}

-- | Adds a callback to be run during startup of the app server
--
-- The follwoing example will print a hello world message on startup:
--
-- > config = do
-- >     addInitializer (putStrLn "Hello World!")
--
addInitializer :: ((?context :: FrameworkConfig, ?modelContext :: ModelContext) => IO ()) -> State.StateT TMap.TMap IO ()
addInitializer onStartup = do
    initializers <- fromMaybe [] <$> findOptionOrNothing @[Initializer]
    let newInitializers = initializers <> [Initializer { onStartup }]
    State.modify (\map -> map
            |> TMap.delete @[Initializer]
            |> TMap.insert newInitializers
        )

ihpDefaultConfig :: ConfigBuilder
ihpDefaultConfig = do
    ihpEnv <- envOrDefault "IHP_ENV" Development
    option ihpEnv

    option $ AppHostname "localhost"

    port :: AppPort <- envOrDefault "PORT" (AppPort defaultPort)
    option port

    option $ ExceptionTracker Warp.defaultOnException

    environment <- findOption @Environment

    defaultLogger <- configIO (defaultLoggerForEnv environment)
    option defaultLogger
    logger <- findOption @Logger

    requestLoggerIpAddrSource <- envOrDefault "IHP_REQUEST_LOGGER_IP_ADDR_SOURCE" RequestLogger.FromSocket

    reqLoggerMiddleware <- configIO $
            case environment of
                Development -> do
                                    reqLogger <- (logger |> defaultRequestLogger)
                                    pure (RequestLoggerMiddleware reqLogger)
                Production  ->  do
                                    reqLogger <- (logger |> makeRequestLogger def { RequestLogger.outputFormat = RequestLogger.Apache requestLoggerIpAddrSource })
                                    pure (RequestLoggerMiddleware reqLogger)


    option $ reqLoggerMiddleware

    option $ defaultCorsResourcePolicy

    option $ Sendmail

    databaseUrl <- configIO defaultDatabaseUrl

    option $ DatabaseUrl databaseUrl
    option $ DBPoolIdleTime $
            case environment of
                Development -> 2
                Production -> 60

    -- poolMaxResources must not be smaller than numStripes
    -- https://github.com/digitallyinduced/ihp/issues/1959
    numCapabilities <- configIO Concurrent.getNumCapabilities
    option $ DBPoolMaxConnections (max numCapabilities 20)

    (AppPort port) <- findOption @AppPort

    -- The IHP_BASEURL env var can override the hardcoded base url in Config.hs
    ihpBaseUrlEnvVar <- envOrNothing "IHP_BASEURL"
    case ihpBaseUrlEnvVar of
        Just baseUrl -> option (BaseUrl baseUrl)
        Nothing -> do
            (AppHostname appHostname) <- findOption @AppHostname
            option $ BaseUrl ("http://" <> appHostname <> (if port /= 80 then ":" <> tshow port else ""))

    (BaseUrl currentBaseUrl) <- findOption @BaseUrl
    option $ SessionCookie (defaultIHPSessionCookie currentBaseUrl)

    option WaiParse.defaultParseRequestBodyOptions

    option bootstrap

    when (environment == Development) do
        ihpIdeBaseUrl <- envOrDefault "IHP_IDE_BASEURL" ("http://localhost:" <> tshow (port + 1))
        option (IdeBaseUrl ihpIdeBaseUrl)

    rlsAuthenticatedRole <- envOrDefault "IHP_RLS_AUTHENTICATED_ROLE" "ihp_authenticated"
    option $ RLSAuthenticatedRole rlsAuthenticatedRole

    dataSyncMaxSubscriptionsPerConnection <- envOrDefault "IHP_DATASYNC_MAX_SUBSCRIPTIONS_PER_CONNECTION" 128
    dataSyncMaxTransactionsPerConnection <- envOrDefault "IHP_DATASYNC_MAX_TRANSACTIONS_PER_CONNECTION" 10
    option $ DataSyncMaxSubscriptionsPerConnection dataSyncMaxSubscriptionsPerConnection
    option $ DataSyncMaxTransactionsPerConnection dataSyncMaxTransactionsPerConnection

    option $ CustomMiddleware id

{-# INLINABLE ihpDefaultConfig #-}

instance EnvVarReader AppPort where
    envStringToValue string = AppPort <$> envStringToValue string

instance EnvVarReader RequestLogger.IPAddrSource where
    envStringToValue "FromHeader" = Right RequestLogger.FromHeader
    envStringToValue "FromSocket" = Right RequestLogger.FromSocket
    envStringToValue otherwise    = Left "Expected 'FromHeader' or 'FromSocket'"

findOption :: forall option. Typeable option => State.StateT TMap.TMap IO option
findOption = fromMaybe (error optionNotFoundErrorMessage) <$> findOptionOrNothing @option
    where
        optionNotFoundErrorMessage = "findOption: Could not find " <> show (Typeable.typeOf (undefined :: option))
{-# INLINABLE findOption #-}

findOptionOrNothing :: forall option. Typeable option => State.StateT TMap.TMap IO (Maybe option)
findOptionOrNothing = do
    options <- State.get
    options
        |> TMap.lookup @option
        |> pure
{-# INLINABLE findOptionOrNothing #-}

buildFrameworkConfig :: ConfigBuilder -> IO FrameworkConfig
buildFrameworkConfig appConfig = do
    let resolve = do
            (AppHostname appHostname) <- findOption @AppHostname
            environment <- findOption @Environment
            (AppPort appPort) <- findOption @AppPort
            (BaseUrl baseUrl) <- findOption @BaseUrl
            (RequestLoggerMiddleware requestLoggerMiddleware) <- findOption @RequestLoggerMiddleware
            (SessionCookie sessionCookie) <- findOption @SessionCookie
            mailServer <- findOption @MailServer
            (DBPoolIdleTime dbPoolIdleTime) <- findOption @DBPoolIdleTime
            (DBPoolMaxConnections dbPoolMaxConnections) <- findOption @DBPoolMaxConnections
            (DatabaseUrl databaseUrl) <- findOption @DatabaseUrl
            cssFramework <- findOption @CSSFramework
            logger <- findOption @Logger
            exceptionTracker <- findOption @ExceptionTracker
            corsResourcePolicy <- findOptionOrNothing @Cors.CorsResourcePolicy
            parseRequestBodyOptions <- findOption @WaiParse.ParseRequestBodyOptions
            (IdeBaseUrl ideBaseUrl) <- findOption @IdeBaseUrl
            (RLSAuthenticatedRole rlsAuthenticatedRole) <- findOption @RLSAuthenticatedRole
            customMiddleware <- findOption @CustomMiddleware
            initializers <- fromMaybe [] <$> findOptionOrNothing @[Initializer]

            appConfig <- State.get


            pure FrameworkConfig { .. }

    (frameworkConfig, _) <- State.runStateT (appConfig >> ihpDefaultConfig >> resolve) TMap.empty

    pure frameworkConfig
{-# INLINABLE buildFrameworkConfig #-}

data FrameworkConfig = FrameworkConfig
    { appHostname :: !Text
    , environment :: !Environment
    , appPort :: !Int
    , baseUrl :: !Text

    -- | Provides IHP with a middleware to log requests and responses.
    --
    -- By default this uses the RequestLogger middleware from wai-extra. Take a look at the wai-extra
    -- documentation when you want to customize the request logging.
    --
    -- See https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html
    --
    --
    -- Set @requestLoggerMiddleware = \application -> application@ to disable request logging.
    , requestLoggerMiddleware :: !Middleware

    -- | Provides the default settings for the session cookie.
    --
    -- - Max Age: 30 days
    -- - Same Site Policy: Lax
    -- - HttpOnly (no access through JS)
    -- - secure, when baseUrl is a https url
    --
    -- Override this to set e.g. a custom max age or change the default same site policy.
    --
    -- __Example: Set max age to 90 days__
    -- > sessionCookie = defaultIHPSessionCookie { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 90)) }
    , sessionCookie :: !Cookie.SetCookie

    , mailServer :: !MailServer

    , databaseUrl :: !ByteString
    -- | How long db connection are kept alive inside the connecton pool when they're idle
    , dbPoolIdleTime :: !NominalDiffTime

    -- | Max number of db connections the connection pool can open to the database
    , dbPoolMaxConnections :: !Int

    -- | Bootstrap 4 by default
    --
    -- Override this if you use a CSS framework that is not bootstrap
    , cssFramework :: !CSSFramework
    , logger :: !Logger
    , exceptionTracker :: !ExceptionTracker

    -- | Custom 'option's from @Config.hs@ are stored here
    --
    -- To access a custom option here, first set it up inside @Config.hs@. This example
    -- reads a string from a env variable on app startup and makes it available to the app
    -- by saving it into the application context:
    --
    -- > -- Config.hs:
    -- >
    -- > newtype RedisUrl = RedisUrl String
    -- >
    -- > config :: ConfigBuilder
    -- > config = do
    -- >     option Development
    -- >     option (AppHostname "localhost")
    -- >
    -- >     redisUrl <- env "REDIS_URL"
    -- >     option (RedisUrl redisUrl)
    --
    -- This redis url can be access from all IHP entrypoints using the request.frameworkConfig.appConfig that is in scope:
    --
    -- > import qualified Data.TMap as TMap
    -- > import Config -- For accessing the RedisUrl data type
    -- >
    -- > action MyAction = do
    -- >     let appConfig = ?context.frameworkConfig.appConfig
    -- >     let (RedisUrl redisUrl) = appConfig
    -- >                |> TMap.lookup @RedisUrl
    -- >                |> fromMaybe (error "Could not find RedisUrl in config")
    -- >
    , appConfig :: !TMap.TMap

    -- | Configures CORS headers for the application. By default this is set to 'Nothing', and the server will not respond with any CORS headers
    --
    -- You can provide a custom CORS policy in @Config.hs@:
    --
    -- > -- Config.hs
    -- > import qualified Network.Wai.Middleware.Cors as Cors
    -- >
    -- > config :: ConfigBuilder
    -- > config = do
    -- >     option Development
    -- >     option (AppHostname "localhost")
    -- >
    -- >     option Cors.simpleCorsResourcePolicy
    -- >
    --
    -- Take a look at the documentation of wai-cors https://hackage.haskell.org/package/wai-cors-0.2.7/docs/Network-Wai-Middleware-Cors.html for understanding what @simpleCorsResourcePolicy@ is doing
    --
    -- You can specify CORS origins like this:
    --
    -- > -- Config.hs
    -- > import qualified Network.Wai.Middleware.Cors as Cors
    -- >
    -- > config :: ConfigBuilder
    -- > config = do
    -- >     option Development
    -- >     option (AppHostname "localhost")
    -- >
    -- >     -- The boolean True specifies if credentials are allowed for the request. You still need to set withCredentials on your XmlHttpRequest
    -- >     option Cors.simpleCorsResourcePolicy { Cors.corsOrigins = Just (["localhost"], True) }
    -- >
    , corsResourcePolicy :: !(Maybe Cors.CorsResourcePolicy)

    -- | Configures the limits for request parameters, uploaded files, maximum number of headers etc.
    --
    -- IHP is using 'Network.Wai.Parse.parseRequestBodyEx' for parsing the HTTP request. By default it applies certain limits
    -- to avoid a single request overloading the server.
    --
    -- You can find the default limits here: https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-Parse.html#v:defaultParseRequestBodyOptions
    --
    -- You can override the default limits like this:
    --
    -- > -- Config.hs
    -- > import qualified Network.Wai.Parse as WaiParse
    -- >
    -- > config :: ConfigBuilder
    -- > config = do
    -- >     option Development
    -- >     option (AppHostname "localhost")
    -- >
    -- >     -- We extend the default options here
    -- >     option $ WaiParse.defaultParseRequestBodyOptions
    -- >             |> WaiParse.setMaxRequestNumFiles 20 -- Increase count of allowed files per request
    -- >
    , parseRequestBodyOptions :: !WaiParse.ParseRequestBodyOptions

    -- | Used by the dev server. This field cannot be strict.
    , ideBaseUrl :: Text

    -- | See IHP.DataSync.Role
    , rlsAuthenticatedRole :: !Text

    -- | User provided WAI middleware that is run after IHP's middleware stack.
    , customMiddleware :: !CustomMiddleware
    , initializers :: ![Initializer]
}

instance HasField "frameworkConfig" FrameworkConfig FrameworkConfig where
    getField frameworkConfig = frameworkConfig

type ConfigProvider context = HasField "frameworkConfig" context FrameworkConfig

-- | Returns the default IHP session cookie configuration. Useful when you want to override the default settings in 'sessionCookie'
defaultIHPSessionCookie :: Text -> Cookie.SetCookie
defaultIHPSessionCookie baseUrl = def
    { Cookie.setCookiePath = Just "/"
    , Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 30))
    , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSecure = "https://" `Text.isPrefixOf` baseUrl
    }

data RootApplication = RootApplication deriving (Eq, Show)

defaultPort :: Int
defaultPort = 8000

defaultDatabaseUrl :: HasCallStack => IO ByteString
defaultDatabaseUrl = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    envOrDefault "DATABASE_URL" defaultDatabaseUrl

defaultLoggerForEnv :: HasCallStack => Environment -> IO Logger
defaultLoggerForEnv = \case
    Development -> defaultLogger
    Production -> newLogger def { level = Info }


-- Returns 'True' when the application is running in a given environment
isEnvironment :: (?context :: context, ConfigProvider context) => Environment -> Bool
isEnvironment environment = ?context.frameworkConfig.environment == environment
{-# INLINABLE isEnvironment #-}

-- | Returns 'True'  when the application is running in Development mode
--
-- Development mode means that the Development option is configured in Config/Config.hs
--
-- See 'Environment' for documentation on the default differences.
isDevelopment :: (?context :: context, ConfigProvider context) => Bool
isDevelopment = isEnvironment Development
{-# INLINABLE isDevelopment #-}

-- | Returns 'True' when the application is running in Production mode
--
-- Production mode means that the Production option is configured in Config/Config.hs
--
-- See 'Environment' for documentation on the default differences.
isProduction :: (?context :: context, ConfigProvider context) => Bool
isProduction = isEnvironment Production
{-# INLINABLE isProduction #-}

defaultCorsResourcePolicy :: Maybe Cors.CorsResourcePolicy
defaultCorsResourcePolicy = Nothing

-- | Builds a config and calls the provided callback.
--
-- Once the callback has returned the resources allocated by the config are closed. Specifcally
-- this will close open log file handles.
--
-- __Example:__
--
-- > import Config (config)
-- >
-- > withFrameworkConfig config \frameworkConfig -> do
-- >     -- Do something with the FrameworkConfig here
--
withFrameworkConfig :: ConfigBuilder -> (FrameworkConfig -> IO result) -> IO result
withFrameworkConfig configBuilder = Exception.bracket (buildFrameworkConfig configBuilder) (\frameworkConfig -> frameworkConfig.logger.cleanup)

initModelContext :: FrameworkConfig -> IO ModelContext
initModelContext FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl, logger } = do
    let isDevelopment = environment == Development
    modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger
    pure modelContext

withModelContext :: FrameworkConfig -> (ModelContext -> IO result) -> IO result
withModelContext frameworkConfig action =
    Exception.bracket (initModelContext frameworkConfig) releaseModelContext action

-- | Wraps an Exception thrown during the config process, but adds a CallStack
--
-- Inspired by https://maksbotan.github.io/posts/2021-01-20-callstacks.html
--
data ExceptionWithCallStack = ExceptionWithCallStack CallStack SomeException

instance Prelude.Show ExceptionWithCallStack where
    show (ExceptionWithCallStack callStack inner) = Prelude.show inner <> "\n" <> Stack.prettyCallStack callStack

instance Exception ExceptionWithCallStack

-- | Runs IO inside the config process
--
-- It works like 'liftIO', but attaches a CallStack on error. Without this it would be hard to see where
-- an error during the config setup comes from.
--
-- All call-sites of this function should also have a @HasCallStack@ constraint to provide helpful information in the call stack.
--
-- See https://github.com/digitallyinduced/ihp/issues/1503
configIO :: (MonadIO monad, HasCallStack) => IO result -> monad result
configIO action = liftIO (action `catch` wrapWithCallStack)
    where
        wrapWithCallStack exception = throwIO (ExceptionWithCallStack Stack.callStack exception)
