module IHP.FrameworkConfig where

import ClassyPrelude
import qualified System.Environment as Environment
import System.Directory (getCurrentDirectory)
import IHP.Environment
import Data.String.Conversions (cs)
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified System.Process as Process
import Network.Wai (Middleware)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Web.Cookie as Cookie
import Data.Default (def)
import Data.Time.Clock (NominalDiffTime)
import IHP.Mail.Types

defaultPort :: Int
defaultPort = 8000

portRef :: IO (IORef Int)
portRef = (newIORef defaultPort)

defaultFrameworkConfig :: Text -> Environment -> IO FrameworkConfig
defaultFrameworkConfig appHostname environment = do
    appPort <- portRef >>= readIORef
    let
        baseUrl = let port = appPort in "http://" <> appHostname <> (if port /= 80 then ":" <> tshow port else "")
        requestLoggerMiddleware = RequestLogger.logStdoutDev
        sessionCookie = defaultIHPSessionCookie baseUrl
        mailServer = Sendmail

    pure FrameworkConfig {..}


data FrameworkConfig = FrameworkConfig 
    { appHostname :: Text
    , environment :: Environment
    , appPort :: Int
    , baseUrl :: Text

    -- | Provides IHP with a middleware to log requests and responses.
    --
    -- By default this uses the RequestLogger middleware from wai-extra. Take a look at the wai-extra
    -- documentation when you want to customize the request logging.
    --
    -- See https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html
    -- 
    --
    -- Set @requestLoggerMiddleware = \application -> application@ to disable request logging.
    , requestLoggerMiddleware :: Middleware

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
    , sessionCookie :: Cookie.SetCookie

    , mailServer :: MailServer
    }

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    -- | How long db connection are kept alive inside the connecton pool when they're idle
    dbPoolIdleTime :: NominalDiffTime
    dbPoolIdleTime = 60

    -- | Max number of db connections the connection pool can open to the database
    dbPoolMaxConnections :: Int
    dbPoolMaxConnections = 20
=======
class FrameworkConfigProxy a where
    extractConfig :: a -> FrameworkConfig

    appHostname :: a -> Text
    appHostname = appHostname_ . extractConfig

    environment :: a -> Environment
    environment = environment_ . extractConfig

    appPort :: a -> Int
    appPort = appPort_ . extractConfig

    baseUrl :: a -> Text
    baseUrl = baseUrl_ . extractConfig

    requestLoggerMiddleware :: a -> Middleware
    requestLoggerMiddleware = requestLoggerMiddleware_ . extractConfig

    sessionCookie :: a -> Cookie.SetCookie
    sessionCookie = sessionCookie_ . extractConfig

    mailServer :: a -> MailServer
    mailServer = mailServer_ . extractConfig

instance FrameworkConfigProxy FrameworkConfig where
    extractConfig = id

    
>>>>>>> Add FrameworkConfigProxy typeclass for easier access
=======
>>>>>>> Remove FrameworkConfigProxy in favour of implictly bound functions

=======
>>>>>>> Fix RequestContext issue with scripts
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

initAppPort :: IO Int
initAppPort = do
    portStr <- Environment.lookupEnv "PORT"
    case portStr of
        Just portStr -> do
            let port = fromMaybe (error "PORT: Invalid value") (readMay portStr)
            portRef >>= (flip writeIORef) port
            pure port
        Nothing -> pure defaultPort

appDatabaseUrl :: IO ByteString
appDatabaseUrl = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    (Environment.lookupEnv "DATABASE_URL") >>= (pure . maybe defaultDatabaseUrl cs )

-- | Finds the lib
--
-- The location depends on whether the framework is installed through nix
-- or checked out from git inside the current project directory.
--
-- When it's installed with nix, the lib dir is located at @lib/ihp@
-- while the dev server binary is located at @bin/RunDevServer@.
findLibDirectory :: IO Text
findLibDirectory = do
    frameworkMountedLocally <- Directory.doesDirectoryExist "IHP"
    ihpLibSymlinkAvailable <- Directory.doesDirectoryExist "build/ihp-lib"
    if frameworkMountedLocally
        then pure "IHP/lib/IHP/"
        else if ihpLibSymlinkAvailable
            then do
                pure "build/ihp-lib/"
            else do
                binDir <- cs <$> Process.readCreateProcess (Process.shell "dirname $(which RunDevServer)") ""
                pure (Text.strip binDir <> "/../lib/IHP/")
