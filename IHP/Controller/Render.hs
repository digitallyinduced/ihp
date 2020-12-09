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
import qualified IHP.Controller.Context as Context
import IHP.Controller.Layout

renderPlain :: (?context :: ControllerContext) => LByteString -> IO ()
renderPlain text = respondAndExit $ responseLBS status200 [(hContentType, "text/plain")] text
{-# INLINABLE renderPlain #-}

respondHtml :: (?context :: ControllerContext) => Html -> IO ()
respondHtml html = respondAndExit $ responseBuilder status200 [(hContentType, "text/html; charset=utf-8"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)
{-# INLINABLE respondHtml #-}

respondSvg :: (?context :: ControllerContext) => Html -> IO ()
respondSvg html = respondAndExit $ responseBuilder status200 [(hContentType, "image/svg+xml"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder html)
{-# INLINABLE respondSvg #-}

renderHtml :: forall viewContext view controller. (ViewSupport.View view, ?theAction :: controller, ?context :: ControllerContext, ?modelContext :: ModelContext) => view -> IO Html
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

renderFile :: (?context :: ControllerContext, ?modelContext :: ModelContext) => String -> ByteString -> IO ()
renderFile filePath contentType = respondAndExit $ responseFile status200 [(hContentType, contentType)] filePath Nothing
{-# INLINABLE renderFile #-}

renderJson :: (?context :: ControllerContext) => Data.Aeson.ToJSON json => json -> IO ()
renderJson json = respondAndExit $ responseLBS status200 [(hContentType, "application/json")] (Data.Aeson.encode json)
{-# INLINABLE renderJson #-}

renderJson' :: (?context :: ControllerContext) => ResponseHeaders -> Data.Aeson.ToJSON json => json -> IO ()
renderJson' additionalHeaders json = respondAndExit $ responseLBS status200 ([(hContentType, "application/json")] <> additionalHeaders) (Data.Aeson.encode json)
{-# INLINABLE renderJson' #-}

renderNotFound :: (?context :: ControllerContext) => IO ()
renderNotFound = renderPlain "Not Found"
{-# INLINABLE renderNotFound #-}

data PolymorphicRender
    = PolymorphicRender
        { html :: Maybe (IO ())
        , json :: Maybe (IO ())
        }

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
{-# INLINABLE renderPolymorphic #-}
renderPolymorphic :: forall viewContext jsonType htmlType. (?context :: ControllerContext) => PolymorphicRender -> IO ()
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
render :: forall view controller. (ViewSupport.View view, ?theAction :: controller, ?context :: ControllerContext, ?modelContext :: ModelContext) => view -> IO ()
render !view = do
    renderPolymorphic PolymorphicRender
            { html = Just $ (renderHtml view) >>= respondHtml
            , json = Just $ renderJson (ViewSupport.json view)
            }
    
