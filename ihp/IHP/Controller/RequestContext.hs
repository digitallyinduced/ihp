{-|
Module: IHP.Controller.RequestContext
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
, RequestBody (..)
) where

import Prelude
import qualified Data.ByteString.Lazy as LBS
import Network.Wai (Request, Response, ResponseReceived)
import Network.Wai.Parse (File, Param)
import qualified Data.Aeson as Aeson
import GHC.Records (HasField(..))
import IHP.FrameworkConfig.Types (FrameworkConfig)
import IHP.RequestVault (requestFrameworkConfig)

-- | Type alias for WAI respond function
type Respond = Response -> IO ResponseReceived

-- | Represents the parsed HTTP request body
data RequestBody
    -- | A form body with URL-encoded or multipart params and files
    = FormBody { params :: [Param], files :: [File LBS.ByteString] }
    -- | A JSON body
    | JSONBody { jsonPayload :: Maybe Aeson.Value, rawPayload :: LBS.ByteString }

-- | Request context containing WAI request data
data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , requestBody :: RequestBody
    }

-- | Access frameworkConfig from the request vault
--
-- This allows @requestContext.frameworkConfig@ to work by looking up
-- the FrameworkConfig from the WAI request vault.
instance HasField "frameworkConfig" RequestContext FrameworkConfig where
    getField rc = requestFrameworkConfig rc.request
    {-# INLINE getField #-}
