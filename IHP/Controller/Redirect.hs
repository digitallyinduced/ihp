{-|
Module: IHP.Controller.Redirect
Description: redirect helpers
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Redirect (redirectTo, redirectToPath, redirectToUrl) where
import ClassyPrelude
import qualified Network.Wai.Util
import Network.URI (parseURI)
import IHP.Controller.RequestContext
import IHP.RouterSupport (HasPath (pathTo))
import qualified Network.Wai as Wai
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200, status302)
import GHC.Records

import IHP.ControllerSupport

-- | Redirects to an action
-- 
-- __Example:__
-- 
-- > redirectTo ShowProjectAction { projectId = get #id project }
--
-- Use 'redirectToPath' if you want to redirect to a non-action url.
{-# INLINE redirectTo #-}
redirectTo :: (?requestContext :: RequestContext, HasPath action) => action -> IO ()
redirectTo action = redirectToPath (pathTo action)

-- TODO: redirectTo user

-- | Redirects to a path (given as a string)
--
-- __Example:__
-- 
-- > redirectToPath "/blog/wp-login.php"
--
-- Use 'redirectTo' if you want to redirect to a controller action.
{-# INLINE redirectToPath #-}
redirectToPath :: (?requestContext :: RequestContext) => Text -> IO ()
redirectToPath path = redirectToUrl (configBaseUrl <> path)

-- | Redirects to a url (given as a string)
-- 
-- __Example:__
--
-- > redirectToUrl "https://example.com/hello-world.html"
--
-- Use 'redirectToPath' if you want to redirect to a relative path like "/hello-world.html"
{-# INLINE redirectToUrl #-}
redirectToUrl :: (?requestContext :: RequestContext) => Text -> IO ()
redirectToUrl url = do
    let (RequestContext _ respond _ _ _ _) = ?requestContext
    let !parsedUrl = fromMaybe 
            (error ("redirectToPath: Unable to parse url: " <> show url))
            (parseURI (cs url))
    let !redirectResponse = fromMaybe
            (error "redirectToPath: Unable to construct redirect response")
            (Network.Wai.Util.redirect status302 [] parsedUrl)
    respondAndExit redirectResponse
