{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where
import ClassyPrelude
import Network.Wai (responseLBS, responseBuilder, responseFile)
import Network.HTTP.Types (Status, status200, status406)
import Network.HTTP.Types.Header
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
respondHtml (Markup builder) = do
        -- Pass the Builder directly to WAI, avoiding the intermediate lazy
        -- ByteString allocation that responseLBS would require.
        respondWith $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] builder
{-# INLINE respondHtml #-}

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
            (renderHtml view) >>= respondHtml
    consumeFlashMessagesMiddleware next currentRequest ?respond
