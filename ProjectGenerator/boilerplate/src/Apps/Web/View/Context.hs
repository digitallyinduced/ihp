module Apps.Web.View.Context where

import           ClassyPrelude
import           Apps.Web.Controller.Context as Controller.Context
import qualified Foundation.Controller.Session
import           Foundation.ControllerSupport  (RequestContext (RequestContext))
import qualified Foundation.ControllerSupport
import           Foundation.HaskellSupport
import           Foundation.ModelSupport
import           Helper.Controller
import           Model.Generated.Types
import qualified Network.Wai
import Data.Dynamic

data ViewContext = ViewContext {
        request      :: Network.Wai.Request,
        -- user        :: Maybe User,
        flashMessages :: [Foundation.Controller.Session.FlashMessage],
        validations :: [Dynamic]
    }

createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext) => (?modelContext :: ModelContext) => Network.Wai.Request -> IO ViewContext
createViewContext request = do
    flashMessages <- Foundation.Controller.Session.getAndClearFlashMessages
    validations <- readIORef (get #validations ?controllerContext)
    return $ ViewContext {
            request = request,
            -- user = currentUserOrNothing,
            flashMessages,
            validations
        }
