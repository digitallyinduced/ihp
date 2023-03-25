{-|
Module: IHP.Controller.Cookie
Description: Set Cookies
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.Controller.Cookie (setCookie) where

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