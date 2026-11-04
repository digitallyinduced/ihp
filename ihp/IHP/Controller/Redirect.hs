{-|
Module: IHP.Controller.Redirect
Description: redirect helpers
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Redirect
( redirectTo
, redirectToPath
, redirectToUrl
, redirectToSeeOther
, redirectToPathSeeOther
, redirectToUrlSeeOther
, redirectBack
, redirectBackWithFallback
, isLocalPath
) where

import Prelude
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs, convertString)
import qualified Network.Wai.Util
import Network.URI (URI (..), parseURI, parseURIReference, uriToString)
import IHP.Router.UrlGenerator (HasPath (pathTo))
import Network.HTTP.Types.Status
import qualified Network.Wai.Middleware.Approot as Approot
import IHP.ControllerSupport

-- | Redirects to an action
--
-- __Example:__
--
-- > redirectTo ShowProjectAction { projectId = project.id }
--
-- Use 'redirectToPath' if you want to redirect to a non-action url.
redirectTo :: (?request :: Request, ?respond :: Respond, HasPath action) => action -> IO ResponseReceived
redirectTo action = redirectToPath (pathTo action)
{-# INLINABLE redirectTo #-}

-- | Redirects to an action using HTTP 303 See Other
--
-- Forces the follow-up request to be a GET (useful after POST/DELETE).
redirectToSeeOther :: (?request :: Request, ?respond :: Respond, HasPath action) => action -> IO ResponseReceived
redirectToSeeOther action = redirectToPathSeeOther (pathTo action)
{-# INLINABLE redirectToSeeOther #-}

-- TODO: redirectTo user

-- | Redirects to a path (given as a string) on this site
--
-- __Example:__
--
-- > redirectToPath "/blog/wp-login.php"
--
-- The path has to start with a @/@ and is appended to the app's base url. A text that
-- a browser could read as another host, like @\@evil.example@ or @//evil.example@, is
-- refused with an error, so a path taken from user input (e.g. a @?next=@ parameter)
-- can never redirect off-site. Check such input with 'isLocalPath' when you want a
-- fallback instead of an error.
--
-- Use 'redirectTo' if you want to redirect to a controller action. Use 'redirectToUrl'
-- if you want to redirect to another host.
redirectToPath :: (?request :: Request, ?respond :: Respond) => Text -> IO ResponseReceived
redirectToPath path = redirectToUrl (localPathToUrl "redirectToPath" path)
{-# INLINABLE redirectToPath #-}

-- | Redirects to a path on this site using HTTP 303 See Other
--
-- Forces the follow-up request to be a GET (useful after POST/DELETE).
--
-- Like 'redirectToPath', this refuses a text that is not a path on this site.
redirectToPathSeeOther :: (?request :: Request, ?respond :: Respond) => Text -> IO ResponseReceived
redirectToPathSeeOther path = redirectToUrlSeeOther (localPathToUrl "redirectToPathSeeOther" path)
{-# INLINABLE redirectToPathSeeOther #-}

-- | Turns a path on this site into a full url by prefixing the app's base url
--
-- Fails with an error naming the calling function when the text is not a path on this
-- site, see 'isLocalPath'.
localPathToUrl :: (?request :: Request) => String -> Text -> Text
localPathToUrl caller path
    | isLocalPath path = convertString (Approot.getApproot ?request) <> path
    | otherwise = error (caller <> ": " <> show path <> " is not a path on this site, refusing to redirect there. Check the path with isLocalPath first, or use redirectToUrl to redirect to another host.")
{-# INLINABLE localPathToUrl #-}

-- | Redirects to a url (given as a string)
--
-- __Example:__
--
-- > redirectToUrl "https://example.com/hello-world.html"
--
-- Use 'redirectToPath' if you want to redirect to a relative path like @/hello-world.html@
redirectToUrl :: (?request :: Request, ?respond :: Respond) => Text -> IO ResponseReceived
redirectToUrl url = do
    let !parsedUrl = fromMaybe
            (error ("redirectToPath: Unable to parse url: " <> show url))
            (parseURI (cs url))
    let !redirectResponse = fromMaybe
            (error "redirectToPath: Unable to construct redirect response")
            (Network.Wai.Util.redirect status302 [] parsedUrl)
    respondWith redirectResponse
{-# INLINABLE redirectToUrl #-}

-- | Redirects to a url using HTTP 303 See Other
--
-- Forces the follow-up request to be a GET (useful after POST/DELETE).
redirectToUrlSeeOther :: (?request :: Request, ?respond :: Respond) => Text -> IO ResponseReceived
redirectToUrlSeeOther url = do
    let !parsedUrl = fromMaybe
            (error ("redirectToUrlSeeOther: Unable to parse url: " <> show url))
            (parseURI (cs url))
    let !redirectResponse = fromMaybe
            (error "redirectToUrlSeeOther: Unable to construct redirect response")
            (Network.Wai.Util.redirect status303 [] parsedUrl)
    respondWith redirectResponse
{-# INLINABLE redirectToUrlSeeOther #-}

-- | Redirects back to the last page
--
-- Uses the Referer header to do a redirect to page that got you here.
--
-- In case the Referer header is not set this function will redirect to @/@. Use 'redirectBackWithFallback' when you want
-- to specify a custom fallback path.
--
-- __Example:__
--
-- > action LikeAction { postId } = do
-- >     post <- fetch postId
-- >     post
-- >         |> incrementField #likesCount
-- >         |> updateRecord
-- >
-- >     redirectBack
--
redirectBack :: (?request :: Request, ?respond :: Respond) => IO ResponseReceived
redirectBack = redirectBackWithFallback "/"
{-# INLINABLE redirectBack #-}

-- | Redirects back to the last page or the given fallback path in case the Referer header is missing
--
-- If you don't care about the missing-Referer-header case, use 'redirectBack'.
--
-- __Example:__
--
-- > action LikeAction { postId } = do
-- >     post <- fetch postId
-- >     post
-- >         |> incrementField #likesCount
-- >         |> updateRecord
-- >
-- >     redirectBackWithFallback (pathTo ShowPostAction { postId = post.id })
--
redirectBackWithFallback :: (?request :: Request, ?respond :: Respond) => Text -> IO ResponseReceived
redirectBackWithFallback fallbackPathOrUrl = do
    case getHeader "Referer" of
        Just referer -> case parseURI (cs referer) of
                Just uri -> redirectToUrl (cs (uriToString id uri ""))           -- Referer Is URL "https://google.com/..."
                Nothing -> redirectToPath (cs referer)          -- Referer Is Path "/../"
        Nothing -> case parseURI (cs fallbackPathOrUrl) of
                Just uri -> redirectToUrl (cs (uriToString id uri ""))           -- Fallback Is URL "https://google.com/..."
                Nothing -> redirectToPath fallbackPathOrUrl     -- Fallback Is Path "/../"
{-# INLINABLE redirectBackWithFallback #-}

-- | Returns 'True' when the given text is a path on this site, like @/posts/1?details=on@.
--
-- Use this before redirecting to a path that came from the request, e.g. a @?next=@
-- query parameter after a login. Without the check, a link such as
-- @/login?next=\@evil.example@ would let a real login send the user to another host
-- (an "open redirect").
--
-- Everything a browser could read as another host or another scheme is refused: absolute
-- urls (@https://evil.example@), network-path references (@//evil.example@, @/\\evil.example@),
-- relative paths (@evil.example@, @\@evil.example@, @.evil.example@) and anything that is not a
-- valid URI, such as a path containing whitespace or control characters.
--
-- __Example:__
--
-- > action CreateSessionAction = do
-- >     let next = paramOrDefault @Text "/" "next"
-- >     -- ... check the credentials ...
-- >     redirectToPath (if isLocalPath next then next else "/")
--
-- 'redirectToPath' and 'redirectToPathSeeOther' call this themselves and fail on a path
-- that is not local. Use it directly when you need a fallback instead of a failure, or when
-- the path is rendered into a view (e.g. a hidden @next@ form field).
isLocalPath :: Text -> Bool
isLocalPath path = case parseURIReference (cs path) of
    Just URI { uriScheme = "", uriAuthority = Nothing, uriPath = '/':_ } -> True
    _ -> False
