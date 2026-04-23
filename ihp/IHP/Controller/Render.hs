{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where
import ClassyPrelude
import Network.Wai (responseLBS, responseBuilder, responseFile, requestMethod)
import Network.HTTP.Types (Status, status200, status406, status422)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (methodGet, methodHead)
import qualified IHP.ViewSupport as ViewSupport
import qualified Data.Aeson
import IHP.ControllerSupport
import qualified Network.HTTP.Media as Accept


import IHP.HSX.Markup (Markup, MarkupM(..))
import qualified IHP.Controller.Context as Context
import IHP.Controller.Layout
import IHP.FlashMessages (consumeFlashMessagesMiddleware)

renderPlain :: (?request :: Request, ?respond :: Respond) => LByteString -> IO ResponseReceived
renderPlain text = respondWith $ responseLBS status200 [(hContentType, "text/plain")] text
{-# INLINE renderPlain #-}

respondHtml :: (?request :: Request, ?respond :: Respond) => Markup -> IO ResponseReceived
respondHtml markup = respondHtmlWithStatus status200 markup
{-# INLINE respondHtml #-}

-- | Like 'respondHtml' but uses the given HTTP status code instead of 200.
--
-- Used by 'render' to default non-GET responses to 422 so Hotwire Turbo can
-- re-render the form with validation errors instead of rejecting a 200 OK
-- form response.
respondHtmlWithStatus :: (?request :: Request, ?respond :: Respond) => Status -> Markup -> IO ResponseReceived
respondHtmlWithStatus status (Markup builder) =
        -- Pass the Builder directly to WAI, avoiding the intermediate lazy
        -- ByteString allocation that responseLBS would require.
        respondWith $ responseBuilder status [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] builder
{-# INLINE respondHtmlWithStatus #-}

respondSvg :: (?request :: Request, ?respond :: Respond) => Markup -> IO ResponseReceived
respondSvg (Markup builder) =
        respondWith $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] builder
{-# INLINABLE respondSvg #-}

renderHtml :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request) => view -> IO Markup
renderHtml !view = do
    let ?view = view
    ViewSupport.beforeRender view
    frozenContext <- Context.freeze ?context

    let ?context = frozenContext
    (ViewLayout layout) <- getLayout

    let boundHtml = let ?context = frozenContext; in layout (ViewSupport.html ?view)
    pure boundHtml
{-# INLINE renderHtml #-}

renderFile :: (?request :: Request, ?respond :: Respond) => String -> ByteString -> IO ResponseReceived
renderFile filePath contentType = respondWith $ responseFile status200 [(hContentType, contentType)] filePath Nothing
{-# INLINE renderFile #-}

renderJson :: (?request :: Request, ?respond :: Respond) => Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson json = renderJsonWithStatusCode status200 json
{-# INLINE renderJson #-}

renderJsonWithStatusCode :: (?request :: Request, ?respond :: Respond) => Data.Aeson.ToJSON json => Status -> json -> IO ResponseReceived
renderJsonWithStatusCode statusCode json = respondWith $ responseLBS statusCode [(hContentType, "application/json")] (Data.Aeson.encode json)
{-# INLINE renderJsonWithStatusCode #-}

renderXml :: (?request :: Request, ?respond :: Respond) => LByteString -> IO ResponseReceived
renderXml xml = respondWith $ responseLBS status200 [(hContentType, "application/xml")] xml
{-# INLINE renderXml #-}

-- | Use 'setHeader' instead
renderJson' :: (?request :: Request, ?respond :: Respond) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ResponseReceived
renderJson' additionalHeaders json = respondWith $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)
{-# INLINE renderJson' #-}

{-# INLINE render #-}
render :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => view -> IO ResponseReceived
render !view = do
    let !currentRequest = ?request
    renderHtmlView currentRequest view

-- | Renders HTML or JSON based on the request's Accept header.
-- Requires both 'View' and 'JsonView' instances for the view type.
{-# INLINE renderHtmlOrJson #-}
renderHtmlOrJson :: forall view. (ViewSupport.View view, ViewSupport.JsonView view, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => view -> IO ResponseReceived
renderHtmlOrJson !view = do
    let !currentRequest = ?request
    let acceptHeader = lookup hAccept (?request.requestHeaders)
    case acceptHeader of
        Nothing -> renderHtmlView currentRequest view
        Just h | "text/html" `isPrefixOf` h -> renderHtmlView currentRequest view
        _ -> do
            let accept = fromMaybe "text/html" acceptHeader
            let send406Error = respondWith $ responseLBS status406 [] "Could not find any acceptable response format"
            let formats =
                    [ ("text/html", renderHtmlView currentRequest view)
                    , ("application/json", renderJson (ViewSupport.json view))
                    ]
            fromMaybe send406Error (Accept.mapAcceptMedia formats accept)

renderHtmlView :: (ViewSupport.View view, ?context :: ControllerContext, ?respond :: Respond) => Request -> view -> IO ResponseReceived
renderHtmlView currentRequest view = do
    let next request respond = do
            let ?request = request
            let ?respond = respond
            (renderHtml view) >>= respondHtmlWithStatus (renderStatus request)
    consumeFlashMessagesMiddleware next currentRequest ?respond

-- | Default HTTP status for 'render' responses.
--
-- GET and HEAD requests render with 200. For other methods (POST/PUT/PATCH/DELETE)
-- we default to 422 Unprocessable Content so that Hotwire Turbo treats the
-- response as "re-render the form with validation errors" instead of rejecting
-- a 200 OK form response. The idiomatic IHP pattern is 'redirectTo' on success
-- and 'render' on validation failure, so a non-GET 'render' is almost always
-- a validation failure.
renderStatus :: Request -> Status
renderStatus request
    | method == methodGet = status200
    | method == methodHead = status200
    | otherwise = status422
  where
    method = requestMethod request
