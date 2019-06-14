module Apps.Web.View.Context where

import ClassyPrelude
import Apps.Web.Controller.Context
import qualified TurboHaskell.Controller.Session
import TurboHaskell.ControllerSupport  (RequestContext (RequestContext))
import qualified TurboHaskell.ControllerSupport
import TurboHaskell.HaskellSupport
import TurboHaskell.ModelSupport
import Helper.Controller
import Model.Generated.Types
import qualified Network.Wai
import Data.Dynamic
import qualified TurboHaskell.ViewSupport as ViewSupport
import Apps.Web.View.Layout
import Apps.Web.Types

instance ViewSupport.CreateViewContext ViewContext where
    type ControllerContext ViewContext = ControllerContext
    createViewContext = do
        flashMessages <- TurboHaskell.Controller.Session.getAndClearFlashMessages
        validations <- readIORef (get #validations ?controllerContext)
        let viewContext = ViewContext {
                requestContext = ?requestContext,
                -- user = currentUserOrNothing,
                flashMessages,
                validations,
                layout = let ?viewContext = viewContext in defaultLayout
            }
        return viewContext