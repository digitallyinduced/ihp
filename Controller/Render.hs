module Foundation.Controller.Render where
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
    import qualified Foundation.ViewSupport
    import qualified Data.Text.Read
    import qualified Data.Either
    import qualified Data.Text.Encoding
    import qualified Data.Text
    import qualified Data.Aeson
    import qualified View.Context
    import Foundation.ControllerSupport (ControllerContext (..))

    import qualified Config

    import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
    import Text.Blaze.Html (Html)

    import Database.PostgreSQL.Simple as PG

    import Control.Monad.Reader

    renderPlain :: (?controllerContext :: ControllerContext) => ByteString -> IO ResponseReceived
    renderPlain text = do
        let (ControllerContext _ respond _ _ _) = ?controllerContext
        respond $ responseLBS status200 [] (cs text)

    renderHtml :: (?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => Foundation.ViewSupport.Html -> IO ResponseReceived
    renderHtml html = do
        let (ControllerContext request respond _ _ _) = ?controllerContext
        viewContext <- View.Context.createViewContext request
        let boundHtml = let ?viewContext = viewContext in html
        respond $ responseLBS status200 [("Content-Type", "text/html")] (Blaze.renderHtml boundHtml)

    renderJson :: (?controllerContext :: ControllerContext) => Data.Aeson.ToJSON json => json -> IO ResponseReceived
    renderJson json = do
        let (ControllerContext request respond _ _ _) = ?controllerContext
        respond $ responseLBS status200 [("Content-Type", "application/json")] (Data.Aeson.encode json)

    renderNotFound :: (?controllerContext :: ControllerContext) => IO ResponseReceived
    renderNotFound = renderPlain "Not Found"