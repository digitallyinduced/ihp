{-# LANGUAGE BangPatterns #-}
module TurboHaskell.Controller.Render where
import ClassyPrelude
import TurboHaskell.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, responseLBS, requestBody, queryString, responseBuilder, responseFile)
import qualified Network.Wai
import Network.HTTP.Types (status200, status302, status406)
import Network.HTTP.Types.Header
import TurboHaskell.ModelSupport
import TurboHaskell.ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Network.Wai.Util
import qualified Data.ByteString.Lazy
import qualified Network.URI
import Data.Maybe (fromJust)
import qualified TurboHaskell.ViewSupport as ViewSupport
import qualified Data.Text.Read
import qualified Data.Either
import qualified Data.Text.Encoding
import qualified Data.Text
import qualified Data.Aeson
import TurboHaskell.ControllerSupport
import qualified Network.HTTP.Media as Accept
import qualified Data.List as List

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)

import Database.PostgreSQL.Simple as PG

import Control.Monad.Reader
import GHC.Records

{-# INLINE renderPlain #-}
renderPlain :: (?requestContext :: RequestContext) => ByteString -> IO ()
renderPlain text = respondAndExit $ responseLBS status200 [] (cs text)

{-# INLINE respondHtml #-}
respondHtml :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => Html -> IO ()
respondHtml html = respondAndExit $ responseBuilder status200 [(hContentType, "text/html"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)

{-# INLINE respondSvg #-}
respondSvg :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => Html -> IO ()
respondSvg html = respondAndExit $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)

{-# INLINE renderHtml #-}
renderHtml :: forall viewContext view controller. (ViewSupport.View view viewContext, ?theAction :: controller, ?requestContext :: RequestContext, ?modelContext :: ModelContext, ViewSupport.CreateViewContext viewContext, HasField "layout" viewContext ViewSupport.Layout, ?controllerContext :: ControllerContext) => view -> IO Html
renderHtml !view = do
    context' <- ViewSupport.createViewContext @viewContext
    let ?viewContext = context'
    let (context, view') = ViewSupport.beforeRender (context', view)
    let layout = getField @"layout" context
    let ?view = view'
    let boundHtml = let ?viewContext = context in layout (ViewSupport.html view')
    return boundHtml

renderFile :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => String -> ByteString -> IO ()
renderFile filePath contentType = respondAndExit $ responseFile status200 [(hContentType, contentType)] filePath Nothing

renderJson :: (?requestContext :: RequestContext) => Data.Aeson.ToJSON json => json -> IO ()
renderJson json = respondAndExit $ responseLBS status200 [(hContentType, "application/json")] (Data.Aeson.encode json)

renderJson' :: (?requestContext :: RequestContext) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ()
renderJson' additionalHeaders json = respondAndExit $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)

renderNotFound :: (?requestContext :: RequestContext) => IO ()
renderNotFound = renderPlain "Not Found"

data PolymorphicRender htmlType jsonType = PolymorphicRender { html :: htmlType, json :: jsonType }
class MaybeRender a where maybeRenderToMaybe :: a -> Maybe (IO ())
instance MaybeRender () where
    {-# INLINE maybeRenderToMaybe #-}
    maybeRenderToMaybe _ = Nothing
instance MaybeRender (IO ()) where
    {-# INLINE maybeRenderToMaybe #-}
    maybeRenderToMaybe response = Just response

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
{-# INLINE renderPolymorphic #-}
renderPolymorphic :: forall viewContext jsonType htmlType. (?requestContext :: RequestContext) => (MaybeRender htmlType, MaybeRender jsonType) => PolymorphicRender htmlType jsonType -> IO ()
renderPolymorphic PolymorphicRender { html, json } = do
    let RequestContext request _ _ _ _ = ?requestContext
    let headers = Network.Wai.requestHeaders request
    let acceptHeader = snd (fromMaybe (hAccept, "text/html") (List.find (\(headerName, _) -> headerName == hAccept) headers)) :: ByteString
    let send406Error = respondAndExit $ responseLBS status406 [] "Could not find any acceptable response format"
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




{-# INLINE render #-}
render :: forall view viewContext controller. (ViewSupport.View view viewContext, ?theAction :: controller, ?requestContext :: RequestContext, ?modelContext :: ModelContext, ViewSupport.CreateViewContext viewContext, HasField "layout" viewContext ViewSupport.Layout, ?controllerContext :: ControllerContext) => view -> IO ()
render !view = do
    renderPolymorphic PolymorphicRender
            { html = (renderHtml @viewContext view) >>= respondHtml
            , json = renderJson (ViewSupport.json view)
            }
    
