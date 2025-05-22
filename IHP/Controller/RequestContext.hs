module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
, RequestBody (..)
) where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LBS
import           Network.Wai                   (Request, Response, ResponseReceived)
import           Network.Wai.Parse (File, Param)
import           IHP.FrameworkConfig
import qualified Data.Aeson as Aeson


type Respond = Response -> IO ResponseReceived

data RequestBody
    = FormBody { params :: [Param], files :: [File LBS.ByteString] }
    | JSONBody { jsonPayload :: Maybe Aeson.Value, rawPayload :: LByteString } -- ^ The jsonPayload is the decoded json request. We keep a copy of the original json request in rawPayload, so that you can e.g. get a HMAC signature from the request

data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , requestBody :: RequestBody
    , frameworkConfig :: FrameworkConfig
    }
