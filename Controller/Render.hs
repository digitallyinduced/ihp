module Foundation.Controller.Render where
    import ClassyPrelude
    import Foundation.HaskellSupport
    import Data.String.Conversions (cs)
    import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString, responseBuilder)
    import qualified Network.Wai
    import Network.HTTP.Types (status200, status302)
    import Network.HTTP.Types.Header
    import Foundation.ModelSupport
    import Foundation.ApplicationContext
    import Network.Wai.Parse as WaiParse
    import qualified Network.Wai.Util
    import qualified Data.ByteString.Lazy
    import qualified Network.URI
    import Data.Maybe (fromJust)
    import qualified Foundation.ViewSupport
    import qualified Data.Text.Read
    import qualified Data.Either
    import qualified Data.Text.Encoding
    import qualified Data.Text
    import qualified Data.Aeson
    import qualified View.Context
    import Foundation.ControllerSupport (RequestContext (..))
    import qualified Controller.Context

    import qualified Config

    import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
    import Text.Blaze.Html (Html)

    import Database.PostgreSQL.Simple as PG

    import Control.Monad.Reader

    renderPlain :: (?requestContext :: RequestContext) => ByteString -> IO ResponseReceived
    renderPlain text = do
        let (RequestContext _ respond _ _ _) = ?requestContext
        respond $ responseLBS status200 [] (cs text)

    renderHtml :: (?requestContext :: RequestContext, ?modelContext :: ModelContext, ?controllerContext :: Controller.Context.ControllerContext) => Foundation.ViewSupport.Html -> IO ResponseReceived
    renderHtml html = do
        let (RequestContext request respond _ _ _) = ?requestContext
        viewContext <- View.Context.createViewContext request
        let boundHtml = let ?viewContext = viewContext in html
        respond $ responseBuilder status200 [(hContentType, "text/html"), (hConnection, "keep-alive")] (Blaze.renderHtmlBuilder boundHtml)

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