module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
, getConfig
, RequestBody (..)
) where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LBS
import           Network.Wai                   (Request, Response, ResponseReceived)
import           Network.Wai.Parse (File, Param)
import qualified Data.Vault.Lazy               as Vault
import           Network.Wai.Session           (Session)
import           IHP.FrameworkConfig
import qualified Data.Aeson as Aeson


type Respond = Response -> IO ResponseReceived

data RequestBody = FormBody { params :: [Param], files :: [File LBS.ByteString] } | JSONBody (Maybe Aeson.Value)

data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , requestBody :: RequestBody
    , vault :: (Vault.Key (Session IO String String))
    , frameworkConfig :: FrameworkConfig
    }

instance ConfigProvider RequestContext where
    getFrameworkConfig = frameworkConfig
