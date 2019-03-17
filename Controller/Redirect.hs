module Foundation.Controller.Redirect (redirectTo, redirectToPath) where
import ClassyPrelude
import qualified Network.Wai.Util 
import qualified Config
import Network.URI (parseURI)
import Foundation.Controller.RequestContext
import Foundation.RouterSupport (HasPath (pathTo))
import qualified Network.Wai as Wai
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200, status302)

redirectTo :: (?requestContext :: RequestContext, HasPath action) => action -> IO Wai.ResponseReceived
redirectTo action = redirectToPath (pathTo action)

redirectToPath :: (?requestContext :: RequestContext) => Text -> IO Wai.ResponseReceived
redirectToPath url = do
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ fromJust $ Network.Wai.Util.redirect status302 [] (fromJust $ parseURI (cs $ Config.baseUrl <> url))