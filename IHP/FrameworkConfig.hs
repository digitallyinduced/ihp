module IHP.FrameworkConfig where

import IHP.Prelude
import ClassyPrelude (readMay)
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
import qualified System.Posix.Env.ByteString as Posix
import Data.String.Interpolate.IsString (i)

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

ihpDefaultConfig :: ConfigBuilder
ihpDefaultConfig = do
    option <$> envOrDefault "IHP_ENV" Development
    option $ AppHostname "localhost"

    option <$> envOrDefault "PORT" defaultPort

    option $ ExceptionTracker Warp.defaultOnException

    environment <- findOption @Environment

    defaultLogger <- liftIO (defaultLoggerForEnv environment)
    option defaultLogger
    logger <- findOption @Logger

    requestLoggerIpAddrSource <- envOrDefault "IHP_REQUEST_LOGGER_IP_ADDR_SOURCE" RequestLogger.FromSocket

    reqLoggerMiddleware <- liftIO $
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

    databaseUrl <- liftIO defaultDatabaseUrl

    option $ DatabaseUrl databaseUrl
    option $ DBPoolIdleTime $
            case environment of
                Development -> 2
                Production -> 60
    option $ DBPoolMaxConnections 20

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
        ihpIdeBaseUrl <- envOrDefault "IHP_IDE_BASEURL" "http://localhost:8001"
        option (IdeBaseUrl ihpIdeBaseUrl)

    rlsAuthenticatedRole <- envOrDefault "IHP_RLS_AUTHENTICATED_ROLE" "ihp_authenticated"
    option $ RLSAuthenticatedRole rlsAuthenticatedRole

    initAssetVersion

{-# INLINABLE ihpDefaultConfig #-}


-- | Returns a env variable. The raw string
-- value is parsed before returning it. So the return value type depends on what
-- you expect (e.g. can be Text, Int some custom type).
--
-- When the parameter is missing or cannot be parsed, an error is raised and
-- the app startup is aborted. Use 'envOrDefault' when you want to get a
-- default value instead of an error, or 'paramOrNothing' to get @Nothing@
-- when the env variable is missing.
--
-- You can define a custom env variable parser by defining a 'EnvVarReader' instance.
--
-- __Example:__ Accessing a env var PORT.
--
-- Let's say an env var PORT is set to 1337
--
-- > export PORT=1337
--
-- We can read @PORT@ like this:
--
-- > port <- env @Int "PORT"
--
-- __Example:__ Missing env vars
--
-- Let's say the @PORT@ env var is not defined. In that case we'll get an
-- error when trying to access it:
--
-- >>> port <- env @Int "PORT"
-- "Env var 'PORT' not set, but it's required for the app to run"
--
env :: forall result monad. (MonadIO monad) => EnvVarReader result => ByteString -> monad result
env name = envOrDefault name (error [i|Env var '#{name}' not set, but it's required for the app to run|])

envOrDefault :: (MonadIO monad) => EnvVarReader result => ByteString -> result -> monad result
envOrDefault name defaultValue = fromMaybe defaultValue <$> envOrNothing name

envOrNothing :: (MonadIO monad) => EnvVarReader result => ByteString -> monad (Maybe result)
envOrNothing name = liftIO $ fmap parseString <$> Posix.getEnv name
    where
        parseString string = case envStringToValue string of
            Left errorMessage -> error [i|Env var '#{name}' is invalid: #{errorMessage}|]
            Right value -> value

class EnvVarReader valueType where
    envStringToValue :: ByteString -> Either Text valueType

instance EnvVarReader Environment where
    envStringToValue "Production"  = Right Production
    envStringToValue "Development" = Right Development
    envStringToValue otherwise     = Left "Should be set to 'Development' or 'Production'"

instance EnvVarReader Int where
    envStringToValue string = case textToInt (cs string) of
        Just integer -> Right integer
        Nothing -> Left [i|Expected integer, got #{string}|]

instance EnvVarReader Text where
    envStringToValue string = Right (cs string)

instance EnvVarReader ByteString where
    envStringToValue string = Right string

instance EnvVarReader AppPort where
    envStringToValue string = AppPort <$> envStringToValue string

instance EnvVarReader RequestLogger.IPAddrSource where
    envStringToValue "FromHeader" = Right RequestLogger.FromHeader
    envStringToValue "FromSocket" = Right RequestLogger.FromSocket
    envStringToValue otherwise    = Left "Expected 'FromHeader' or 'FromSocket'"


initAssetVersion :: ConfigBuilder
initAssetVersion = do
    ihpCloudContainerId <- envOrNothing "IHP_CLOUD_CONTAINER_ID"
    ihpAssetVersion <- envOrNothing "IHP_ASSET_VERSION"
    let assetVersion = [ ihpCloudContainerId, ihpAssetVersion]
            |> catMaybes
            |> head
            |> fromMaybe "dev"
    option (AssetVersion assetVersion)

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
            (AssetVersion assetVersion) <- findOption @AssetVersion

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
    -- >     redisUrl <- liftIO $ System.Environment.getEnv "REDIS_URL"
    -- >     option (RedisUrl redisUrl)
    --
    -- This redis url can be access from all IHP entrypoints using the ?applicationContext that is in scope:
    -- 
    -- > import qualified Data.TMap as TMap
    -- > import Config -- For accessing the RedisUrl data type
    -- > 
    -- > action MyAction = do
    -- >     let appConfig = ?context |> getFrameworkConfig |> get #appConfig
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
    , corsResourcePolicy :: Maybe Cors.CorsResourcePolicy

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
    , parseRequestBodyOptions :: WaiParse.ParseRequestBodyOptions
    , ideBaseUrl :: Text

    -- | See IHP.DataSync.Role
    , rlsAuthenticatedRole :: Text

    -- | The asset version is used for cache busting
    --
    -- On IHP Cloud IHP automatically uses the @IHP_CLOUD_CONTAINER_ID@ env variable
    -- as the asset version. So when running there, you don't need to do anything.
    --
    -- If you deploy IHP on your own, you should provide the IHP_ASSET_VERSION
    -- env variable with e.g. the git commit hash of the production build.
    --
    -- If IHP cannot figure out an asset version, it will fallback to the static
    -- string @"dev"@.
    --
    , assetVersion :: !Text
}

class ConfigProvider a where
    getFrameworkConfig :: a -> FrameworkConfig

instance ConfigProvider FrameworkConfig where
    getFrameworkConfig = id

instance LoggingProvider FrameworkConfig where
    getLogger = get #logger


-- | Proxies FrameworkConfig fields contained in some context that can provider a FrameworkConfig
fromConfig :: (?context :: context, ConfigProvider context) => (FrameworkConfig -> a) -> a
fromConfig selector = (selector . getFrameworkConfig) ?context
{-# INLINE fromConfig #-}

-- | Get the current frameworkConfig
getConfig :: (?context :: context, ConfigProvider context) => FrameworkConfig
getConfig = fromConfig id
{-# INLINE getConfig #-}

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

defaultDatabaseUrl :: IO ByteString
defaultDatabaseUrl = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    envOrDefault "DATABASE_URL" defaultDatabaseUrl

defaultLoggerForEnv :: Environment -> IO Logger
defaultLoggerForEnv = \case
    Development -> defaultLogger
    Production -> newLogger def { level = Info }


-- Returns 'True' when the application is running in a given environment
isEnvironment :: (?context :: context, ConfigProvider context) => Environment -> Bool
isEnvironment environment = (getFrameworkConfig ?context |> get #environment) == environment
{-# INLINABLE isEnvironment #-}

-- | Returns 'True'  when the application is running in Development mode
--
-- Development mode means that the Development option is configured in Config/Config.hs
isDevelopment :: (?context :: context, ConfigProvider context) => Bool
isDevelopment = isEnvironment Development
{-# INLINABLE isDevelopment #-}

-- | Returns 'True' when the application is running in Production mode
--
-- Production mode means that the Production option is configured in Config/Config.hs
isProduction :: (?context :: context, ConfigProvider context) => Bool
isProduction = isEnvironment Production
{-# INLINABLE isProduction #-}

defaultCorsResourcePolicy :: Maybe Cors.CorsResourcePolicy
defaultCorsResourcePolicy = Nothing