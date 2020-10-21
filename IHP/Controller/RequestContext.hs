module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
, configAppHostname
, configEnvironment
, configAppPort
, configBaseUrl
, configRequestLoggerMiddleware
, configSessionCookie
, configMailServer
) where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LBS
import           Network.Wai                   (Request, Response, ResponseReceived)
import           Network.Wai.Parse (File, Param)
import qualified Data.Vault.Lazy               as Vault
import           Network.Wai.Session           (Session)
import           Network.Wai                   (Middleware)
import qualified Web.Cookie as Cookie
import           IHP.FrameworkConfig           (FrameworkConfig)
import qualified IHP.FrameworkConfig as FrameworkConfig
import           IHP.Environment
import           IHP.Mail.Types                (MailServer)



type Respond = Response -> IO ResponseReceived

data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , params :: [Param]
    , files :: [File LBS.ByteString]
    , vault :: (Vault.Key (Session IO String String))
    , frameworkConfig :: FrameworkConfig
    }


-- Proxies FrameworkConfig fields contained in the RequestContext

configAppHostname :: (?requestContext :: RequestContext) => Text
configAppHostname = (FrameworkConfig.appHostname . frameworkConfig) ?requestContext

configEnvironment :: (?requestContext :: RequestContext) => Environment
configEnvironment = (FrameworkConfig.environment . frameworkConfig) ?requestContext

configAppPort :: (?requestContext :: RequestContext) => Int
configAppPort = (FrameworkConfig.appPort . frameworkConfig) ?requestContext

configBaseUrl :: (?requestContext :: RequestContext) => Text
configBaseUrl = (FrameworkConfig.baseUrl . frameworkConfig) ?requestContext

configRequestLoggerMiddleware :: (?requestContext :: RequestContext) => Middleware
configRequestLoggerMiddleware = (FrameworkConfig.requestLoggerMiddleware . frameworkConfig) ?requestContext

configSessionCookie :: (?requestContext :: RequestContext) => Cookie.SetCookie
configSessionCookie = (FrameworkConfig.sessionCookie . frameworkConfig) ?requestContext

configMailServer :: (?requestContext :: RequestContext) => MailServer
configMailServer = (FrameworkConfig.mailServer . frameworkConfig) ?requestContext

