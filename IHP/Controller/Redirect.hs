{-|
Module: IHP.Controller.Redirect
Description: redirect helpers
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Redirect
( redirectTo
, redirectToPath
, redirectToUrl
, redirectBack
, redirectBackWithFallback
) where

import IHP.Prelude
import qualified Network.Wai.Util
import Network.URI (parseURI)
import IHP.Controller.RequestContext
import IHP.RouterSupport (HasPath (pathTo))
import IHP.FrameworkConfig
import Network.HTTP.Types.Status

import IHP.Controller.Context
import IHP.ControllerSupport

-- | Redirects to an action
--
-- __Example:__
--
-- > redirectTo ShowProjectAction { projectId = get #id project }
--
-- Use 'redirectToPath' if you want to redirect to a non-action url.
redirectTo :: (?context :: ControllerContext, HasPath action) => action -> IO ()
redirectTo action = redirectToPath (pathTo action)
{-# INLINABLE redirectTo #-}

-- TODO: redirectTo user

-- | Redirects to a path (given as a string)
--
-- __Example:__
--
-- > redirectToPath "/blog/wp-login.php"
--
-- Use 'redirectTo' if you want to redirect to a controller action.
redirectToPath :: (?context :: ControllerContext) => Text -> IO ()
redirectToPath path = redirectToUrl (fromConfig baseUrl <> path)
{-# INLINABLE redirectToPath #-}

-- | Redirects to a url (given as a string)
--
-- __Example:__
--
-- > redirectToUrl "https://example.com/hello-world.html"
--
-- Use 'redirectToPath' if you want to redirect to a relative path like @/hello-world.html@
redirectToUrl :: (?context :: ControllerContext) => Text -> IO ()
redirectToUrl url = do
    let RequestContext { respond } = ?context |> get #requestContext
    let !parsedUrl = fromMaybe
            (error ("redirectToPath: Unable to parse url: " <> show url))
            (parseURI (cs url))
    let !redirectResponse = fromMaybe
            (error "redirectToPath: Unable to construct redirect response")
            (Network.Wai.Util.redirect status302 [] parsedUrl)
    respondAndExit redirectResponse
{-# INLINABLE redirectToUrl #-}


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
redirectBack :: (?context :: ControllerContext) => IO ()
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
-- >     redirectBackWithFallback (pathTo ShowPostAction { postId = get #id post })
--
redirectBackWithFallback :: (?context :: ControllerContext) => Text -> IO ()
redirectBackWithFallback fallbackPathOrUrl = do
    case getHeader "Referer" of
        Just referer -> case parseURI (cs referer) of
                Just uri -> redirectToUrl (tshow uri)           -- Referer Is URL "https://google.com/..."
                Nothing -> redirectToPath (cs referer)          -- Referer Is Path "/../"
        Nothing -> case parseURI (cs fallbackPathOrUrl) of
                Just uri -> redirectToUrl (tshow uri)           -- Fallback Is URL "https://google.com/..."
                Nothing -> redirectToPath fallbackPathOrUrl     -- Fallback Is Path "/../"
{-# INLINABLE redirectBackWithFallback #-}
