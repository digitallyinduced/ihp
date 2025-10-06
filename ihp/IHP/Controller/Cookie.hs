{-|
Module: IHP.Controller.Cookie
Description: Set Cookies
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.Controller.Cookie (setCookie, getCookie, allCookies) where

import IHP.Prelude
import IHP.ControllerSupport
import Web.Cookie
import qualified Data.Binary.Builder as Binary
import qualified Data.ByteString.Lazy as LBS

-- | Sets a @Set-Cookie@ header
--
-- > import Web.Cookie
-- >
-- > action MyAction = do
-- >     setCookie defaultSetCookie
-- >             { setCookieName = "exampleCookie"
-- >             , setCookieValue = "exampleValue"
-- >             }
-- 
setCookie :: (?context :: ControllerContext) => SetCookie -> IO ()
setCookie cookie = setHeader ("Set-Cookie", cookieString)
    where
        cookieString = cookie
                |> renderSetCookie
                |> Binary.toLazyByteString
                |> LBS.toStrict

-- | Returns a cookie by it's name
--
-- > getCookie "fbc"
-- Just "1234"
--
getCookie :: (?context :: ControllerContext) => Text -> Maybe Text
getCookie name =
    lookup name allCookies

-- | Returns all cookies sent with the current request
allCookies :: (?context :: ControllerContext) => [(Text, Text)]
allCookies =
    maybe [] parseCookiesText $ lookup "Cookie" request.requestHeaders