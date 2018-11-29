{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ControllerSupport (withContext, Action, Action', cs, (|>), redirectTo, getRequestBody, getRequestUrl, getHeader, RequestContext (..), ActionHelper, getRequest, requestHeaders, getFiles) where
import ClassyPrelude
import Foundation.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString, requestHeaders)
import qualified Network.Wai
import Network.HTTP.Types (status200, status302)
import Foundation.ModelSupport
import Foundation.ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Network.Wai.Util
import qualified Data.ByteString.Lazy
import qualified Network.URI
import Data.Maybe (fromJust)
import qualified Data.Text.Read
import qualified Data.Either
import qualified Data.Text.Encoding
import qualified Data.Text
import qualified Data.Aeson
import Foundation.Controller.RequestContext
import qualified Data.CaseInsensitive

import qualified Config

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)

import Database.PostgreSQL.Simple as PG

import Control.Monad.Reader

import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy         as Vault
import Apps.Web.Controller.Context (ControllerContext, createControllerContext)

type Action = ((?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: ControllerContext) => IO ResponseReceived)
type Action' = IO ResponseReceived

type ActionHelper return = ((?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: ControllerContext) => return)

--request :: StateT RequestContext IO ResponseReceived -> Request
--request = do
--    RequestContext request <- get
--    return request

--(|>) :: a -> f -> f a


{-# INLINE redirectTo #-}
redirectTo :: (?requestContext :: RequestContext) => Text -> IO ResponseReceived
redirectTo url = do
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ Network.URI.parseURI (cs $ Config.baseUrl <> url))

{-# INLINE getRequestBody #-}
getRequestBody :: (?requestContext :: RequestContext) => IO ByteString
getRequestBody =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.requestBody request

{-# INLINE getRequestUrl #-}
getRequestUrl :: (?requestContext :: RequestContext) => ByteString
getRequestUrl =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.rawPathInfo request

{-# INLINE getHeader #-}
getHeader :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
getHeader name =
    let (RequestContext request _ _ _ _) = ?requestContext
    in lookup (Data.CaseInsensitive.mk name) (Network.Wai.requestHeaders request)

{-# INLINE getRequest #-}
getRequest :: (?requestContext :: RequestContext) => Network.Wai.Request
getRequest =
    let (RequestContext request _ _ _ _) = ?requestContext
    in request

{-# INLINE getFiles #-}
getFiles :: (?requestContext :: RequestContext) => [File Data.ByteString.Lazy.ByteString]
getFiles =
    let (RequestContext _ _ _ files _) = ?requestContext
    in files

{-# INLINE withContext #-}
withContext :: ((?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: ControllerContext) => a) -> ApplicationContext -> Request -> Respond -> IO a
withContext theAction (ApplicationContext modelContext session) request respond = do
    (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
    let ?requestContext = RequestContext request respond params files session
    let ?modelContext = modelContext
    controllerContext <- createControllerContext
    let ?controllerContext = controllerContext
    return $ theAction
