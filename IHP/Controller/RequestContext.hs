module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
, getConfig
) where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LBS
import           Network.Wai                   (Request, Response, ResponseReceived)
import           Network.Wai.Parse (File, Param)
import qualified Data.Vault.Lazy               as Vault
import           Network.Wai.Session           (Session)
import           IHP.FrameworkConfig



type Respond = Response -> IO ResponseReceived

data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , params :: [Param]
    , files :: [File LBS.ByteString]
    , vault :: (Vault.Key (Session IO String String))
    , frameworkConfig :: FrameworkConfig
    }

instance ConfigProvider RequestContext where
    getFrameworkConfig = frameworkConfig
