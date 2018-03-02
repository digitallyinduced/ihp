module Foundation.Controller.RequestContext where

import           ClassyPrelude
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Either
import           Data.Maybe                    (fromJust)
import           Data.String.Conversions       (cs)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Read
import           Foundation.ApplicationContext
import           Foundation.HaskellSupport
import           Foundation.ModelSupport
import           Network.HTTP.Types            (status200, status302)
import qualified Network.URI
import           Network.Wai                   (Request, Response, ResponseReceived, queryString, requestBody, responseLBS)
import qualified Network.Wai
import           Network.Wai.Parse             as WaiParse
import qualified Network.Wai.Util

import qualified Config

import           Text.Blaze.Html               (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Data.Vault.Lazy               as Vault
import           Network.Wai.Session           (Session)

type Respond = Response -> IO ResponseReceived

data RequestContext = RequestContext Request Respond [WaiParse.Param] [WaiParse.File Data.ByteString.Lazy.ByteString] (Vault.Key (Session IO String String))


