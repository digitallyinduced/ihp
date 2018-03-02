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


setSession :: (?requestContext :: RequestContext) => String -> String -> IO ()
setSession name value = sessionInsert name value
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (_, sessionInsert) = Vault.lookup session (Network.Wai.vault request)


getSession :: (?requestContext :: RequestContext) => String -> IO (Maybe String)
getSession = sessionLookup
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (sessionLookup, _) = Vault.lookup session (Network.Wai.vault request)

getSessionInt :: (?requestContext :: RequestContext) => String -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    return $ case fmap (Data.Text.Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _ -> Nothing
