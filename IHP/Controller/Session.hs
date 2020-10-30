module IHP.Controller.Session where
import           ClassyPrelude
import qualified Data.Either
import           Data.String.Conversions              (cs)
import qualified Data.Text
import qualified Data.Text.Read
import qualified Data.UUID
import           IHP.Controller.RequestContext
import           IHP.HaskellSupport
import           IHP.ControllerSupport
import           Network.HTTP.Types                   (status200, status302, status401)
import           Network.Wai                          (Request, Response, ResponseReceived, queryString, requestBody, responseLBS)
import qualified Network.Wai

import qualified Data.Vault.Lazy                      as Vault
import           Network.Wai.Session                  (Session)
import           Network.Wai.Middleware.HttpAuth      (extractBasicAuth)
import           Network.HTTP.Types.Header            (hWWWAuthenticate)
import qualified Data.Maybe as Maybe


setSession :: (?requestContext :: RequestContext) => Text -> Text -> IO ()
setSession name value = sessionInsert (cs name) (cs value)
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (_, sessionInsert) = Vault.lookup session (Network.Wai.vault request)


getSession :: (?requestContext :: RequestContext) => Text -> IO (Maybe Text)
getSession name = do
        value <- (sessionLookup (cs name))
        let textValue = fmap cs value
        pure $! if textValue == Just "" then Nothing else textValue
    where
        (RequestContext request _ _ _ session) = ?requestContext
        Just (sessionLookup, _) = Vault.lookup session (Network.Wai.vault request)


getSessionAndClear :: (?requestContext :: RequestContext) => Text -> IO (Maybe Text)
getSessionAndClear name = do
    value <- getSession name
    when (isJust value) (setSession name "")
    pure value

getSessionInt :: (?requestContext :: RequestContext) => Text -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    pure $! case fmap (Data.Text.Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _                  -> Nothing

getSessionUUID :: (?requestContext :: RequestContext) => Text -> IO (Maybe Data.UUID.UUID)
getSessionUUID name = do
    value <- getSession name
    pure $! case fmap Data.UUID.fromText value of
            Just (Just value) -> Just value
            _                 -> Nothing

-- | Adds basic http authentication
--
-- Mainly for protecting a site during external review.
-- Meant for use in the controller:
-- 
-- > beforeAction = basicAuth ... 
-- 
basicAuth :: (?requestContext::RequestContext) => Text -> Text -> Text -> IO ()
basicAuth uid pw realm = do
    let mein = Just (encodeUtf8 uid, encodeUtf8 pw)
    let cred = join $ fmap extractBasicAuth (getHeader "Authorization")
    when (cred /= mein) $ respondAndExit $ responseLBS status401 [(hWWWAuthenticate,encodeUtf8 ("Basic " ++ (if null realm then "" else "realm=\"" ++ realm ++ "\", ") ++ "charset=\"UTF-8\""))] ""

successMessageKey :: Text
successMessageKey = "flashSuccessMessage"

errorMessageKey :: Text
errorMessageKey = "flashErrorMessage"

data FlashMessage = SuccessFlashMessage Text | ErrorFlashMessage Text

-- Due to a compiler bug we have to place these functions inside the Session module
setSuccessMessage :: (?requestContext :: RequestContext) => Text -> IO ()
setSuccessMessage = setSession successMessageKey

getSuccessMessage :: (?requestContext :: RequestContext) => IO (Maybe Text)
getSuccessMessage = getSession successMessageKey

clearSuccessMessage :: (?requestContext :: RequestContext) => IO ()
clearSuccessMessage = setSession successMessageKey ""

setErrorMessage :: (?requestContext :: RequestContext) => Text -> IO ()
setErrorMessage = setSession errorMessageKey

getAndClearFlashMessages :: (?requestContext :: RequestContext) => IO [FlashMessage]
getAndClearFlashMessages = do
    successMessage <- getSuccessMessage
    errorMessage <- getSession errorMessageKey
    case successMessage of
        Just value | value /= "" -> setSuccessMessage ""
        _ -> pure ()
    case errorMessage of
        Just value | value /= "" -> setErrorMessage ""
        _ -> pure ()
    pure $ Maybe.catMaybes ((fmap SuccessFlashMessage successMessage):(fmap ErrorFlashMessage errorMessage):[])
