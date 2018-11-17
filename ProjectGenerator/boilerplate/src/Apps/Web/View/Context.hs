module View.Context where

import           ClassyPrelude
import           Controller.Context
import qualified Foundation.Controller.Session
import           Foundation.ControllerSupport  (RequestContext (RequestContext))
import qualified Foundation.ControllerSupport
import           Foundation.HaskellSupport
import           Foundation.ModelSupport
import           Helper.Controller
import           Model.Generated.Types
import qualified Network.Wai

data ViewContext = ViewContext {
        request      :: Network.Wai.Request,
        -- user         :: Maybe User,
        flashMessages :: [Foundation.Controller.Session.FlashMessage]
    }

createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext) => (?modelContext :: ModelContext) => Network.Wai.Request -> IO ViewContext
createViewContext request = do
    flashMessages <- Foundation.Controller.Session.getAndClearFlashMessages
    return $ ViewContext {
            request = request,
            -- user = currentUserOrNothing,
            flashMessages
        }
