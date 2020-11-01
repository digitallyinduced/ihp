{-|
Module: IHP.Controller.BasicAuth
Description: Very Simple Basic Auth
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.BasicAuth (basicAuth) where

import IHP.Prelude
import IHP.Controller.RequestContext
import IHP.ControllerSupport
import Network.HTTP.Types (status401)
import Network.Wai (responseLBS)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.HTTP.Types.Header (hWWWAuthenticate)

-- | Adds basic http authentication
--
-- Mainly for protecting a site during external review.
-- Meant for use in the controller:
-- 
-- > beforeAction = basicAuth ... 
-- 
basicAuth :: (?context :: RequestContext) => Text -> Text -> Text -> IO ()
basicAuth uid pw realm = do
    let mein = Just (cs uid, cs pw)
    let cred = join $ fmap extractBasicAuth (getHeader "Authorization")
    when (cred /= mein) $ respondAndExit $ responseLBS status401 [(hWWWAuthenticate,cs ("Basic " ++ (if null realm then "" else "realm=\"" ++ realm ++ "\", ") ++ "charset=\"UTF-8\""))] ""