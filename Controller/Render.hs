module Foundation.Controller.Render where
import ClassyPrelude
import Foundation.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString, responseBuilder, responseFile)
import qualified Network.Wai
import Network.HTTP.Types (status200, status302, status406)
import Network.HTTP.Types.Header
import Foundation.ModelSupport
import Foundation.ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Network.Wai.Util
import qualified Data.ByteString.Lazy
import qualified Network.URI
import Data.Maybe (fromJust)
import Foundation.ViewSupport
import qualified Data.Text.Read
import qualified Data.Either
import qualified Data.Text.Encoding
import qualified Data.Text
import qualified Data.Aeson
import Foundation.ControllerSupport (RequestContext (..))
import qualified Apps.Web.Controller.Context as Controller.Context
import qualified Network.HTTP.Media as Accept
import qualified Data.List as List

import qualified Config

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)

import Database.PostgreSQL.Simple as PG

import Control.Monad.Reader
import Foundation.HaskellSupport
import Control.Lens hiding ((|>))
import Data.Generics.Product

renderPlain :: (?requestContext :: RequestContext) => ByteString -> IO ResponseReceived
renderPlain text = do
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseLBS status200 [] (cs text)

renderHtml :: (?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext, CreateViewContext viewContext, HasField' "layout" viewContext Layout, Generic viewContext) => HtmlWithContext viewContext -> IO ResponseReceived
renderHtml html = do
    let (RequestContext request respond _ _ _) = ?requestContext
    viewContext <- createViewContext
    let layout = get #layout viewContext
    let boundHtml = let ?viewContext = viewContext in (layout html)
    respond $ responseBuilder status200 [(hContentType, "text/html"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder boundHtml)

renderFile :: (?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext) => String -> ByteString -> IO ResponseReceived
renderFile filePath contentType = do
    let (RequestContext request respond _ _ _) = ?requestContext
    respond $ responseFile status200 [(hContentType, contentType)] filePath Nothing

renderJson :: (?requestContext :: RequestContext) => Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson json = do
    let (RequestContext request respond _ _ _) = ?requestContext
    respond $ responseLBS status200 [(hContentType, "application/json")] (Data.Aeson.encode json)

renderJson' :: (?requestContext :: RequestContext) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson' additionalHeaders json = do
    let (RequestContext request respond _ _ _) = ?requestContext
    respond $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)

renderNotFound :: (?requestContext :: RequestContext) => IO ResponseReceived
renderNotFound = renderPlain "Not Found"

data PolymorphicRender htmlType jsonType = PolymorphicRender { html :: htmlType, json :: jsonType }
class MaybeRender a where maybeRenderToMaybe :: a -> Maybe (IO ResponseReceived)
instance MaybeRender () where maybeRenderToMaybe _ = Nothing
instance MaybeRender (IO ResponseReceived) where maybeRenderToMaybe response = Just response

-- Can be used to render different responses for html, json, etc. requests based on `Accept` header
-- Example:
--
-- show :: Action
-- show = do
--     renderPolymorphic polymorphicRender {
--         html = renderHtml [hsx|<div>Hello World</div>|]
--         json = renderJson True
--     }
--
-- This will render `Hello World` for normal browser requests and `true` when requested via an ajax request
renderPolymorphic :: (?requestContext :: RequestContext) => (MaybeRender htmlType, MaybeRender jsonType) => PolymorphicRender htmlType jsonType -> IO ResponseReceived
renderPolymorphic PolymorphicRender { html, json } = do
    let RequestContext request respond _ _ _ = ?requestContext
    let headers = Network.Wai.requestHeaders request
    let acceptHeader = snd (fromMaybe (hAccept, "text/html") (List.find (\(headerName, _) -> headerName == hAccept) headers)) :: ByteString
    let send406Error = respond $ responseLBS status406 [] "Could not find any acceptable response format"
    let formats = concat [
                case maybeRenderToMaybe html of
                    Just handler -> [("text/html", handler)]
                    Nothing -> mempty
                 ,
                case maybeRenderToMaybe json of
                    Just handler -> [("application/json", handler)]
                    Nothing -> mempty
            ]
    fromMaybe send406Error (Accept.mapAcceptMedia formats acceptHeader)

polymorphicRender = PolymorphicRender () ()


class View action where
    renderView :: action -> (?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext) => IO ResponseReceived
