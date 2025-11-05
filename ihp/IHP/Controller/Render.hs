{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where
import ClassyPrelude
import Network.Wai (responseLBS, responseBuilder, responseFile)
import qualified Network.Wai
import Network.HTTP.Types (Status, status200, status406)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy
import qualified IHP.ViewSupport as ViewSupport
import qualified Data.Aeson
import IHP.ControllerSupport
import qualified Network.HTTP.Media as Accept
import qualified Data.List as List

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Layout
import qualified Data.ByteString.Builder as ByteString
import IHP.FlashMessages (consumeFlashMessagesMiddleware)
import qualified IHP.Controller.RequestContext

renderPlain :: (?context :: ControllerContext) => LByteString -> IO ()
renderPlain text = respondAndExit $ responseLBS status200 [(hContentType, "text/plain")] text
{-# INLINABLE renderPlain #-}

respondHtml :: (?context :: ControllerContext) => Html -> IO ()
respondHtml html =
        -- The seq is required to force evaluation of `evaluatedBuilder` before returning the IO action. See below for details
        evaluatedBuilder `seq` (respondAndExit $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] evaluatedBuilder)
    where
        builder = Blaze.renderHtmlBuilder html
        builderAsByteString = ByteString.toLazyByteString builder

        -- We force the full evaluation of the blaze html expressions to catch
        -- any runtime errors with the IHP error middleware. Without this full evaluation
        -- certain thunks might only cause an error when warp is building the response string.
        -- But then it's already too late to catch the exception and the user will only get
        -- the default warp error message instead of our nice IHP error message design.
        evaluatedBuilder = Data.ByteString.Lazy.length builderAsByteString `seq` ByteString.lazyByteString builderAsByteString
{-# INLINABLE respondHtml #-}

respondSvg :: (?context :: ControllerContext) => Html -> IO ()
respondSvg html = respondAndExit $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)
{-# INLINABLE respondSvg #-}

renderHtml :: forall view. (ViewSupport.View view, ?context :: ControllerContext) => view -> IO Html
renderHtml !view = do
    let ?view = view
    ViewSupport.beforeRender view
    frozenContext <- Context.freeze ?context

    let ?context = frozenContext
    let layout = case Context.maybeFromFrozenContext @ViewLayout of
            Just (ViewLayout layout) -> layout
            Nothing -> id

    let boundHtml = let ?context = frozenContext in layout (ViewSupport.html ?view)
    pure boundHtml
{-# INLINABLE renderHtml #-}

renderFile :: (?context :: ControllerContext) => String -> ByteString -> IO ()
renderFile filePath contentType = respondAndExit $ responseFile status200 [(hContentType, contentType)] filePath Nothing
{-# INLINABLE renderFile #-}

renderJson :: (?context :: ControllerContext) => Data.Aeson.ToJSON json => json -> IO ()
renderJson json = renderJsonWithStatusCode status200 json
{-# INLINABLE renderJson #-}

renderJsonWithStatusCode :: (?context :: ControllerContext) => Data.Aeson.ToJSON json => Status -> json -> IO ()
renderJsonWithStatusCode statusCode json = respondAndExit $ responseLBS statusCode [(hContentType, "application/json")] (Data.Aeson.encode json)
{-# INLINABLE renderJsonWithStatusCode #-}

renderXml :: (?context :: ControllerContext) => LByteString -> IO ()
renderXml xml = respondAndExit $ responseLBS status200 [(hContentType, "application/xml")] xml
{-# INLINABLE renderXml #-}

-- | Use 'setHeader' intead
renderJson' :: (?context :: ControllerContext) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ()
renderJson' additionalHeaders json = respondAndExit $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)
{-# INLINABLE renderJson' #-}

data PolymorphicRender
    = PolymorphicRender
        { html :: Maybe (IO ())
        , json :: Maybe (IO ())
        }

-- | Can be used to render different responses for html, json, etc. requests based on `Accept` header
-- Example:
--
-- > show :: Action
-- > show = do
-- >     renderPolymorphic polymorphicRender {
-- >         html = renderHtml [hsx|<div>Hello World</div>|]
-- >         json = renderJson True
-- >     }
--
-- This will render @Hello World@ for normal browser requests and @true@ when requested via an ajax request
{-# INLINABLE renderPolymorphic #-}
renderPolymorphic :: (?context :: ControllerContext) => PolymorphicRender -> IO ()
renderPolymorphic PolymorphicRender { html, json } = do
    let headers = Network.Wai.requestHeaders request
    let acceptHeader = snd (fromMaybe (hAccept, "text/html") (List.find (\(headerName, _) -> headerName == hAccept) headers)) :: ByteString
    let send406Error = respondAndExit $ responseLBS status406 [] "Could not find any acceptable response format"
    let formats = concat [
                case html of
                    Just handler -> [("text/html", handler)]
                    Nothing -> mempty
                 ,
                case json of
                    Just handler -> [("application/json", handler)]
                    Nothing -> mempty
            ]
    fromMaybe send406Error (Accept.mapAcceptMedia formats acceptHeader)

polymorphicRender :: PolymorphicRender
polymorphicRender = PolymorphicRender Nothing Nothing


{-# INLINABLE render #-}
render :: forall view. (ViewSupport.View view, ?context :: ControllerContext) => view -> IO ()
render !view = do
    renderPolymorphic PolymorphicRender
            { html = Just do
                    let next request respond = do
                            let requestContext' = ?context.requestContext { IHP.Controller.RequestContext.request, IHP.Controller.RequestContext.respond }
                            let context' = ?context { Context.requestContext = requestContext' }
                            let ?context = context' in (renderHtml view) >>= respondHtml
                            error "unreachable"
                    _ <- consumeFlashMessagesMiddleware next request ?context.requestContext.respond
                    pure ()
            , json = Just $ renderJson (ViewSupport.json view)
            }

