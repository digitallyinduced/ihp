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
) where

import IHP.Prelude
import qualified Network.Wai.Util
import Network.URI (parseURI)
import IHP.Controller.RequestContext
import IHP.Router.UrlGenerator (HasPath (pathTo))
import Network.HTTP.Types.Status
import qualified Network.Wai.Middleware.Approot as Approot

import IHP.Controller.Context
import IHP.ControllerSupport
import IHP.Controller.Response (respondWith)
import Network.Wai (ResponseReceived)

-- | Redirects to an action
--
-- __Example:__
--
-- > redirectTo ShowProjectAction { projectId = project.id }
--
-- Use 'redirectToPath' if you want to redirect to a non-action url.
redirectTo :: (?context :: ControllerContext, HasPath action) => action -> IO ResponseReceived
redirectTo action = redirectToPath (pathTo action)
{-# INLINABLE redirectTo #-}

-- | Redirects to an action using HTTP 303 See Other
--
-- Forces the follow-up request to be a GET (useful after POST/DELETE).
redirectToSeeOther :: (?context :: ControllerContext, HasPath action) => action -> IO ResponseReceived
redirectToSeeOther action = redirectToPathSeeOther (pathTo action)
{-# INLINABLE redirectToSeeOther #-}

-- TODO: redirectTo user

-- | Redirects to a path (given as a string)
--
-- __Example:__
--
-- > redirectToPath "/blog/wp-login.php"
--
-- Use 'redirectTo' if you want to redirect to a controller action.
redirectToPath :: (?context :: ControllerContext) => Text -> IO ResponseReceived
redirectToPath path = redirectToUrl (convertString baseUrl <> path)
    where
        baseUrl = Approot.getApproot ?context.requestContext.request
{-# INLINABLE redirectToPath #-}

-- | Redirects to a path using HTTP 303 See Other
--
-- Forces the follow-up request to be a GET (useful after POST/DELETE).
redirectToPathSeeOther :: (?context :: ControllerContext) => Text -> IO ResponseReceived
redirectToPathSeeOther path = redirectToUrlSeeOther (convertString baseUrl <> path)
    where
        baseUrl = Approot.getApproot ?context.requestContext.request
{-# INLINABLE redirectToPathSeeOther #-}

-- | Redirects to a url (given as a string)
--
-- __Example:__
--
-- > redirectToUrl "https://example.com/hello-world.html"
--
-- Use 'redirectToPath' if you want to redirect to a relative path like @/hello-world.html@
redirectToUrl :: (?context :: ControllerContext) => Text -> IO ResponseReceived
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
redirectToUrlSeeOther :: (?context :: ControllerContext) => Text -> IO ResponseReceived
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
redirectBack :: (?context :: ControllerContext) => IO ResponseReceived
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
redirectBackWithFallback :: (?context :: ControllerContext) => Text -> IO ResponseReceived
redirectBackWithFallback fallbackPathOrUrl = do
    case getHeader "Referer" of
        Just referer -> case parseURI (cs referer) of
                Just uri -> redirectToUrl (tshow uri)           -- Referer Is URL "https://google.com/..."
                Nothing -> redirectToPath (cs referer)          -- Referer Is Path "/../"
        Nothing -> case parseURI (cs fallbackPathOrUrl) of
                Just uri -> redirectToUrl (tshow uri)           -- Fallback Is URL "https://google.com/..."
                Nothing -> redirectToPath fallbackPathOrUrl     -- Fallback Is Path "/../"
{-# INLINABLE redirectBackWithFallback #-}
