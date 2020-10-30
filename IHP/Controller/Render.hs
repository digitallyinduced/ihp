{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where
import ClassyPrelude
import IHP.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, responseLBS, requestBody, queryString, responseBuilder, responseFile)
import qualified Network.Wai
import Network.HTTP.Types (status200, status302, status406)
import Network.HTTP.Types.Header
import IHP.ModelSupport
import qualified Network.Wai.Util
import qualified Data.ByteString.Lazy
import qualified IHP.ViewSupport as ViewSupport
import qualified Data.Aeson
import IHP.ControllerSupport
import qualified Network.HTTP.Media as Accept
import qualified Data.List as List

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)
import GHC.Records

{-# INLINE renderPlain #-}
renderPlain :: (?context :: RequestContext) => ByteString -> IO ()
renderPlain text = respondAndExit $ responseLBS status200 [(hContentType, "text/plain")] (cs text)

{-# INLINE respondHtml #-}
respondHtml :: (?context :: RequestContext) => Html -> IO ()
respondHtml html = respondAndExit $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)

{-# INLINE respondSvg #-}
respondSvg :: (?context :: RequestContext) => Html -> IO ()
respondSvg html = respondAndExit $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)

{-# INLINE renderHtml #-}
renderHtml :: forall viewContext view controller. (ViewSupport.View view viewContext, ?theAction :: controller, ?context :: RequestContext, ?modelContext :: ModelContext, ViewSupport.CreateViewContext viewContext, HasField "layout" viewContext ViewSupport.Layout, ?controllerContext :: ControllerContext) => view -> IO Html
renderHtml !view = do
    context' <- ViewSupport.createViewContext @viewContext
    let ?context = context'
    let (context, view') = ViewSupport.beforeRender (context', view)
    let layout = getField @"layout" context
    let ?view = view'
    let boundHtml = let ?context = context in layout (ViewSupport.html view')
    pure boundHtml

renderFile :: (?context :: RequestContext, ?modelContext :: ModelContext) => String -> ByteString -> IO ()
renderFile filePath contentType = respondAndExit $ responseFile status200 [(hContentType, contentType)] filePath Nothing

renderJson :: (?context :: RequestContext) => Data.Aeson.ToJSON json => json -> IO ()
renderJson json = respondAndExit $ responseLBS status200 [(hContentType, "application/json")] (Data.Aeson.encode json)

renderJson' :: (?context :: RequestContext) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ()
renderJson' additionalHeaders json = respondAndExit $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)

renderNotFound :: (?context :: RequestContext) => IO ()
renderNotFound = renderPlain "Not Found"

data PolymorphicRender htmlType jsonType = PolymorphicRender { html :: htmlType, json :: jsonType }
class MaybeRender a where maybeRenderToMaybe :: a -> Maybe (IO ())
instance MaybeRender () where
    {-# INLINE maybeRenderToMaybe #-}
    maybeRenderToMaybe _ = Nothing
instance MaybeRender (IO ()) where
    {-# INLINE maybeRenderToMaybe #-}
    maybeRenderToMaybe response = Just response

-- | Can be used to render different responses for html, json, etc. requests based on `Accept` header
-- Example:
-- `
-- show :: Action
-- show = do
--     renderPolymorphic polymorphicRender {
--         html = renderHtml [hsx|<div>Hello World</div>|]
--         json = renderJson True
--     }
-- `
-- This will render `Hello World` for normal browser requests and `true` when requested via an ajax request
{-# INLINE renderPolymorphic #-}
renderPolymorphic :: forall viewContext jsonType htmlType. (?context :: RequestContext) => (MaybeRender htmlType, MaybeRender jsonType) => PolymorphicRender htmlType jsonType -> IO ()
renderPolymorphic PolymorphicRender { html, json } = do
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
render :: forall view viewContext controller. (ViewSupport.View view viewContext, ?theAction :: controller, ?context :: RequestContext, ?modelContext :: ModelContext, ViewSupport.CreateViewContext viewContext, HasField "layout" viewContext ViewSupport.Layout, ?controllerContext :: ControllerContext) => view -> IO ()
render !view = do
    renderPolymorphic PolymorphicRender
            { html = (renderHtml @viewContext view) >>= respondHtml
            , json = renderJson (ViewSupport.json view)
            }
    
