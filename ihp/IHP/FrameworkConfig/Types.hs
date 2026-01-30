{-# LANGUAGE ConstraintKinds #-}
{-|
Module: IHP.FrameworkConfig.Types
Description: Core types for framework configuration
Copyright: (c) digitally induced GmbH, 2020

This module contains the core types for IHP's framework configuration.
It's designed to be lightweight and avoid Template Haskell dependencies,
allowing modules that only need the types to compile faster.

For the full configuration API including defaults, use 'IHP.FrameworkConfig'.
-}
module IHP.FrameworkConfig.Types
( AppHostname (..)
, AppPort (..)
, BaseUrl (..)
, RequestLoggerMiddleware (..)
, SessionCookie (..)
, DBPoolIdleTime (..)
, DBPoolMaxConnections (..)
, DatabaseUrl (..)
, ConfigBuilder
, ExceptionTracker (..)
, IdeBaseUrl (..)
, RLSAuthenticatedRole (..)
, AssetVersion (..)
, CustomMiddleware (..)
, AuthMiddleware (..)
, DataSyncMaxSubscriptionsPerConnection (..)
, DataSyncMaxTransactionsPerConnection (..)
, Initializer (..)
, FrameworkConfig (..)
, ConfigProvider
) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Control.Exception (SomeException)
import GHC.Records (HasField(..))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified Web.Cookie as Cookie
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Parse as WaiParse
import Network.Wai (Middleware, Request)
import IHP.Environment (Environment)
import IHP.View.Types (CSSFramework)
import IHP.Log.Types (Logger)
import IHP.ModelSupport.Types (ModelContext)

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

-- | Middleware for authentication.
--
-- This middleware runs after the session and model context middlewares,
-- and populates the WAI request vault with the authenticated user/admin.
--
-- __Example:__
--
-- > -- Config.hs
-- > import IHP.LoginSupport.Middleware
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option $ AuthMiddleware authMiddleware
--
newtype AuthMiddleware = AuthMiddleware Middleware

newtype DataSyncMaxSubscriptionsPerConnection = DataSyncMaxSubscriptionsPerConnection Int
newtype DataSyncMaxTransactionsPerConnection = DataSyncMaxTransactionsPerConnection Int

newtype Initializer = Initializer { onStartup :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => IO () }

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
    , appConfig :: !TMap.TMap

    -- | Configures CORS headers for the application
    , corsResourcePolicy :: !(Maybe Cors.CorsResourcePolicy)

    -- | Configures the limits for request parameters, uploaded files, maximum number of headers etc.
    , parseRequestBodyOptions :: !WaiParse.ParseRequestBodyOptions

    -- | Used by the dev server. This field cannot be strict.
    , ideBaseUrl :: Text

    -- | See IHP.DataSync.Role
    , rlsAuthenticatedRole :: !Text

    -- | User provided WAI middleware that is run after IHP's middleware stack.
    , customMiddleware :: !CustomMiddleware

    -- | Authentication middleware that populates the request vault with the
    -- current user/admin. Runs after session and model context middlewares.
    , authMiddleware :: !AuthMiddleware
    , initializers :: ![Initializer]
    }

instance HasField "frameworkConfig" FrameworkConfig FrameworkConfig where
    getField frameworkConfig = frameworkConfig

type ConfigProvider context = HasField "frameworkConfig" context FrameworkConfig
