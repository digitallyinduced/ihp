module Foundation.Controller.Session where
import ClassyPrelude
import Foundation.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString)
import qualified Network.Wai
import Network.HTTP.Types (status200, status302)
import qualified Data.Text.Read
import qualified Data.Either
import qualified Data.Text
import Foundation.Controller.RequestContext

import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy         as Vault


setSession :: (?requestContext :: RequestContext) => Text -> Text -> IO ()
setSession name value = sessionInsert (cs name) (cs value)
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (_, sessionInsert) = Vault.lookup session (Network.Wai.vault request)


getSession :: (?requestContext :: RequestContext) => Text -> IO (Maybe Text)
getSession name = do
        value <- (sessionLookup (cs name))
        let textValue = fmap cs value
        return $ if textValue == Just "" then Nothing else textValue
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (sessionLookup, _) = Vault.lookup session (Network.Wai.vault request)

getSessionInt :: (?requestContext :: RequestContext) => Text -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    return $ case fmap (Data.Text.Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _ -> Nothing

-- Due to a compiler bug we have to place these functions inside the Session module
setSuccessMessage :: (?requestContext :: RequestContext) => Text -> IO ()
setSuccessMessage message = setSession "flashMessage" message