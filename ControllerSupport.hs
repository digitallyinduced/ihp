{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ControllerSupport (withContext, Action, param, paramInt, paramText, cs, (|>), redirectTo, params, ParamName (paramName), getRequestBody, ControllerContext (..), setSession, getSession, getSessionInt) where
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
    import qualified Data.Text.Read
    import qualified Data.Either
    import qualified Data.Text.Encoding
    import qualified Data.Text
    import qualified Data.Aeson

    import qualified Config

    import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
    import Text.Blaze.Html (Html)

    import Database.PostgreSQL.Simple as PG

    import Control.Monad.Reader

    import Network.Wai.Session (Session)
    import qualified Data.Vault.Lazy         as Vault

    data ControllerContext = ControllerContext Request Respond [WaiParse.Param] [WaiParse.File Data.ByteString.Lazy.ByteString] (Vault.Key (Session IO String String))

    type Respond = Response -> IO ResponseReceived
    type WithControllerContext returnType = ReaderT ControllerContext IO returnType

    type Action = ((?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => IO ResponseReceived)

    withContext :: Action -> ApplicationContext -> Request -> Respond -> IO ResponseReceived
    withContext theAction (ApplicationContext modelContext session) request respond = do
        (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
        let
            ?controllerContext = ControllerContext request respond params files session
            ?modelContext = modelContext
            in theAction

    --request :: StateT ControllerContext IO ResponseReceived -> Request
    --request = do
    --    ControllerContext request <- get
    --    return request

    --(|>) :: a -> f -> f a


    redirectTo :: (?controllerContext :: ControllerContext) => Text -> IO ResponseReceived
    redirectTo url = do
        let (ControllerContext _ respond _ _ _) = ?controllerContext
        respond $ fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ Network.URI.parseURI (cs $ Config.baseUrl <> url))

    --params ::
    --params attributes = map readAttribute attributes

    param :: (?controllerContext :: ControllerContext) => ByteString -> ByteString
    param name = do
        let (ControllerContext request _ bodyParams _ _) = ?controllerContext
        let
            allParams :: [(ByteString, Maybe ByteString)]
            allParams = concat [(map (\(a, b) -> (a, Just b)) bodyParams), (queryString request)]
        fromMaybe (error $ "Required parameter " <> cs name <> " is missing") (join (lookup name allParams))

    paramInt :: (?controllerContext :: ControllerContext) => ByteString -> Int
    paramInt name = fst $ Data.Either.fromRight (error $ "Invalid parameter " <> cs name) (Data.Text.Read.decimal $ cs $ param name)

    paramText :: (?controllerContext :: ControllerContext) => ByteString -> Text
    paramText name = cs $ param name

    getRequestBody :: (?controllerContext :: ControllerContext) => IO ByteString
    getRequestBody =
        let (ControllerContext request _ _ _ _) = ?controllerContext
        in Network.Wai.requestBody request

    setSession :: (?controllerContext :: ControllerContext) => String -> String -> IO ()
    setSession name value = sessionInsert name value
        where
            (ControllerContext request _ _ _ session) = ?controllerContext
            Just (_, sessionInsert) = Vault.lookup session (Network.Wai.vault request)


    getSession :: (?controllerContext :: ControllerContext) => String -> IO (Maybe String)
    getSession = sessionLookup
        where
            (ControllerContext request _ _ _ session) = ?controllerContext
            Just (sessionLookup, _) = Vault.lookup session (Network.Wai.vault request)

    getSessionInt :: (?controllerContext :: ControllerContext) => String -> IO (Maybe Int)
    getSessionInt name = do
        value <- getSession name
        return $ case fmap (Data.Text.Read.decimal . cs) value of
                Just (Right value) -> Just $ fst value
                _ -> Nothing

    class ParamName a where
        paramName :: a -> ByteString

    instance ParamName ByteString where
        paramName = ClassyPrelude.id

    params :: (?controllerContext :: ControllerContext) => ParamName a => [a] -> [(a, ByteString)]
    params = map (\name -> (name, param $ paramName name))