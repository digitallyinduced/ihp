{-# LANGUAGE BangPatterns #-}
module IHP.Controller.Render where
import ClassyPrelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Network.Wai (responseLBS, responseBuilder, responseFile)
import Network.HTTP.Types (Status, status200, status406)
import Network.HTTP.Types.Header
import qualified IHP.ViewSupport as ViewSupport
import qualified Data.Aeson
import IHP.ControllerSupport
import qualified Network.HTTP.Media as Accept


import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html (Html)
import IHP.AutoRefresh.View (autoRefreshMeta)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Layout
import IHP.FlashMessages (consumeFlashMessagesMiddleware)

renderPlain :: (?request :: Request) => LByteString -> IO ()
renderPlain text = respondAndExitWithHeaders $ responseLBS status200 [(hContentType, "text/plain")] text
{-# INLINE renderPlain #-}

respondHtml :: (?context :: ControllerContext, ?request :: Request) => Html -> IO ()
respondHtml html = do
        let !bs = Blaze.renderHtml html
        frozenContext <- Context.freeze ?context
        let meta = let ?context = frozenContext in Blaze.renderHtml autoRefreshMeta
        let bs' =
                if LazyByteString.null meta || ByteString.isInfixOf "ihp-auto-refresh-id" (LazyByteString.toStrict bs)
                    then bs
                    else meta <> bs
        -- We force the full evaluation of the blaze html to catch any runtime errors
        -- with the IHP error middleware. Without this, certain thunks might only cause
        -- an error when warp is building the response string. But then it's already too
        -- late to catch the exception and the user will only get the default warp error
        -- message instead of our nice IHP error message design.
        _ <- evaluate (LazyByteString.length bs')
        respondAndExitWithHeaders $ responseLBS status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] bs'
{-# INLINE respondHtml #-}

respondSvg :: (?request :: Request) => Html -> IO ()
respondSvg html = respondAndExitWithHeaders $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)
{-# INLINABLE respondSvg #-}

renderHtml :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request) => view -> IO Html
renderHtml !view = do
    let ?view = view
    ViewSupport.beforeRender view
    frozenContext <- Context.freeze ?context

    let ?context = frozenContext
    (ViewLayout layout) <- getLayout

    let boundHtml = let ?context = frozenContext; in layout (ViewSupport.html ?view)
    pure boundHtml
{-# INLINE renderHtml #-}

renderFile :: (?request :: Request) => String -> ByteString -> IO ()
renderFile filePath contentType = respondAndExitWithHeaders $ responseFile status200 [(hContentType, contentType)] filePath Nothing
{-# INLINE renderFile #-}

renderJson :: (?request :: Request) => Data.Aeson.ToJSON json => json -> IO ()
renderJson json = renderJsonWithStatusCode status200 json
{-# INLINE renderJson #-}

renderJsonWithStatusCode :: (?request :: Request) => Data.Aeson.ToJSON json => Status -> json -> IO ()
renderJsonWithStatusCode statusCode json = respondAndExitWithHeaders $ responseLBS statusCode [(hContentType, "application/json")] (Data.Aeson.encode json)
{-# INLINE renderJsonWithStatusCode #-}

renderXml :: (?request :: Request) => LByteString -> IO ()
renderXml xml = respondAndExitWithHeaders $ responseLBS status200 [(hContentType, "application/xml")] xml
{-# INLINE renderXml #-}

-- | Use 'setHeader' instead
renderJson' :: (?request :: Request) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ()
renderJson' additionalHeaders json = respondAndExitWithHeaders $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)
{-# INLINE renderJson' #-}

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
{-# INLINE renderPolymorphic #-}
renderPolymorphic :: (?context :: ControllerContext, ?request :: Request) => PolymorphicRender -> IO ()
renderPolymorphic PolymorphicRender { html, json } = do
    let acceptHeader = lookup hAccept (request.requestHeaders)
    case acceptHeader of
        -- Fast path: no Accept header or starts with text/html — dispatch directly
        Nothing | Just handler <- html -> handler
        Just h | "text/html" `isPrefixOf` h, Just handler <- html -> handler
        _ -> do
            let accept = fromMaybe "text/html" acceptHeader
            let send406Error = respondAndExitWithHeaders $ responseLBS status406 [] "Could not find any acceptable response format"
            let formats = concat [
                        case html of
                            Just handler -> [("text/html", handler)]
                            Nothing -> mempty
                         ,
                        case json of
                            Just handler -> [("application/json", handler)]
                            Nothing -> mempty
                    ]
            fromMaybe send406Error (Accept.mapAcceptMedia formats accept)

polymorphicRender :: PolymorphicRender
polymorphicRender = PolymorphicRender Nothing Nothing


{-# INLINE render #-}
render :: forall view. (ViewSupport.View view, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => view -> IO ()
render !view = do
    let !currentRequest = ?request
    renderPolymorphic PolymorphicRender
            { html = Just do
                    let next request respond = do
                            let ?request = request in ((renderHtml view) >>= respondHtml)
                            error "unreachable"
                    _ <- consumeFlashMessagesMiddleware next currentRequest ?respond
                    pure ()
            , json = Just $ renderJson (ViewSupport.json view)
            }
