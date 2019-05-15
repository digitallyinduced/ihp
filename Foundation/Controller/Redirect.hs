module Foundation.Controller.Redirect (redirectTo, redirectToPath) where
import ClassyPrelude
import qualified Network.Wai.Util 
import Network.URI (parseURI)
import Foundation.Controller.RequestContext
import Foundation.RouterSupport (HasPath (pathTo))
import qualified Network.Wai as Wai
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200, status302)
import GHC.Records

import Foundation.FrameworkConfig (FrameworkConfig)
import qualified Foundation.FrameworkConfig as FrameworkConfig

-- Redirects to an action
-- Example:
-- ```
-- redirectTo ShowProjectAction { projectId = get #id project }
-- ```
--
-- Use `redirectToPath` if you want to redirect to a non-action url
redirectTo :: (?requestContext :: RequestContext, FrameworkConfig, HasPath action) => action -> IO Wai.ResponseReceived
redirectTo action = redirectToPath (pathTo action)

-- Redirects to a path (given as a string)
-- Example:
-- ```
-- redirectToPath "/blog/wp-login.php"
-- ```
--
-- Use `redirectTo` if you want to redirect to a controller action
redirectToPath :: (?requestContext :: RequestContext, FrameworkConfig) => Text -> IO Wai.ResponseReceived
redirectToPath url = do
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $! fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ parseURI (cs $ FrameworkConfig.baseUrl <> url))