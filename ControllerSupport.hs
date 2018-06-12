{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ControllerSupport (withContext, Action, Action', cs, (|>), redirectTo, getRequestBody, getRequestUrl, RequestContext (..)) where
import ClassyPrelude
import Foundation.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString)
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

import qualified Config

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)

import Database.PostgreSQL.Simple as PG

import Control.Monad.Reader

import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy         as Vault
import qualified Controller.Context

type Action = ((?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext) => IO ResponseReceived)
type Action' = IO ResponseReceived

--request :: StateT RequestContext IO ResponseReceived -> Request
--request = do
--    RequestContext request <- get
--    return request

--(|>) :: a -> f -> f a


redirectTo :: (?requestContext :: RequestContext) => Text -> IO ResponseReceived
redirectTo url = do
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ Network.URI.parseURI (cs $ Config.baseUrl <> url))

getRequestBody :: (?requestContext :: RequestContext) => IO ByteString
getRequestBody =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.requestBody request

getRequestUrl :: (?requestContext :: RequestContext) => ByteString
getRequestUrl =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.rawPathInfo request

withContext :: ((?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext) => a) -> ApplicationContext -> Request -> Respond -> IO a
withContext theAction (ApplicationContext modelContext session) request respond = do
    (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
    let ?requestContext = RequestContext request respond params files session
    let ?modelContext = modelContext
    controllerContext <- Controller.Context.createControllerContext
    let ?controllerContext = controllerContext
    return $ theAction
