module Foundation.ControllerSupport (withContext, Action, renderHtml, renderPlain, param, cs, (|>), redirectTo) where
    import ClassyPrelude
    import Foundation.HaskellSupport
    import Data.String.Conversions (cs)
    import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString)
    import Network.HTTP.Types (status200, status302)
    import Foundation.ModelSupport
    import Foundation.ApplicationContext
    import Network.Wai.Parse as WaiParse
    import qualified Network.Wai.Util
    import qualified Data.ByteString.Lazy
    import qualified Network.URI
    import Data.Maybe (fromJust)

    import qualified Config

    import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
    import Text.Blaze.Html (Html)

    import Database.PostgreSQL.Simple as PG

    import Control.Monad.Reader

    data ControllerContext = ControllerContext Request Respond [WaiParse.Param] [WaiParse.File Data.ByteString.Lazy.ByteString]

    type Respond = Response -> IO ResponseReceived
    type WithControllerContext returnType = ReaderT ControllerContext IO returnType

    type Action = ((?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => IO ResponseReceived)

    withContext :: Action -> ApplicationContext -> Request -> Respond -> IO ResponseReceived
    withContext theAction (ApplicationContext modelContext) request respond = do
        (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
        let
            ?controllerContext = ControllerContext request respond params files
            ?modelContext = modelContext
            in theAction

    --request :: StateT ControllerContext IO ResponseReceived -> Request
    --request = do
    --    ControllerContext request <- get
    --    return request

    --(|>) :: a -> f -> f a


    renderPlain :: (?controllerContext :: ControllerContext) => ByteString -> IO ResponseReceived
    renderPlain text = do
        let (ControllerContext _ respond _ _) = ?controllerContext
        respond $ responseLBS status200 [] (cs text)

    renderHtml :: (?controllerContext :: ControllerContext) => Html -> IO ResponseReceived
    renderHtml html = do
        let (ControllerContext _ respond _ _) = ?controllerContext
        respond $ responseLBS status200 [("Content-Type", "text/html")] (Blaze.renderHtml html)

    redirectTo :: (?controllerContext :: ControllerContext) => Text -> IO ResponseReceived
    redirectTo url = do
        let (ControllerContext _ respond _ _) = ?controllerContext
        respond $ fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ Network.URI.parseURI (cs $ Config.baseUrl <> url))

    --params ::
    --params attributes = map readAttribute attributes

    param :: (?controllerContext :: ControllerContext) => ByteString -> ByteString
    param name = do
        let (ControllerContext request _ bodyParams _) = ?controllerContext
        let
            allParams :: [(ByteString, Maybe ByteString)]
            allParams = concat [(map (\(a, b) -> (a, Just b)) bodyParams), (queryString request)]
        fromMaybe (error $ "Required parameter " <> cs name <> " is missing") (join (lookup name allParams))