{-# LANGUAGE ConstraintKinds #-}
module IHP.FrameworkConfig
( -- * Re-exports from IHP.FrameworkConfig.Types
  module IHP.FrameworkConfig.Types
  -- * Configuration helpers
, option
, addInitializer
, findOption
, findOptionOrNothing
, ihpDefaultConfig
, buildFrameworkConfig
, defaultIHPSessionCookie
, RootApplication (..)
, defaultPort
, defaultDatabaseUrl
, defaultLoggerForEnv
, isEnvironment
, isDevelopment
, isProduction
, defaultCorsResourcePolicy
, withFrameworkConfig
, configIO
, ExceptionWithCallStack (..)
) where

import IHP.Prelude
import IHP.FrameworkConfig.Types
import qualified System.Directory.OsPath as Directory
import IHP.Environment
import System.OsPath (decodeUtf)
import qualified Data.Text as Text
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Web.Cookie as Cookie
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified Data.Typeable as Typeable
import IHP.View.Types
import IHP.View.CSSFramework.Bootstrap (bootstrap)
import IHP.Log.Types
import IHP.Log (makeRequestLogger, defaultRequestLogger)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Parse as WaiParse
import qualified Control.Exception as Exception
import IHP.EnvVar

import qualified Prelude
import qualified GHC.Stack as Stack


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

    databaseUrl <- configIO defaultDatabaseUrl

    option $ DatabaseUrl databaseUrl
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
    currentDirectoryOsPath <- Directory.getCurrentDirectory
    currentDirectory <- decodeUtf currentDirectoryOsPath
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
