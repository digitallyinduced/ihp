module Apps.Web.View.Context where

import ClassyPrelude
import Apps.Web.Controller.Context
import qualified Foundation.Controller.Session
import Foundation.ControllerSupport  (RequestContext (RequestContext))
import qualified Foundation.ControllerSupport
import Foundation.HaskellSupport
import Foundation.ModelSupport
import Helper.Controller
import Model.Generated.Types
import qualified Network.Wai
import Data.Dynamic
import qualified Foundation.ViewSupport as ViewSupport
import Apps.Web.View.Layout
import Apps.Web.Types

instance ViewSupport.CreateViewContext ViewContext where
    type ControllerContext ViewContext = ControllerContext
    createViewContext = do
        flashMessages <- Foundation.Controller.Session.getAndClearFlashMessages
        validations <- readIORef (get #validations ?controllerContext)
        let viewContext = ViewContext {
                requestContext = ?requestContext,
                -- user = currentUserOrNothing,
                flashMessages,
                validations,
                layout = let ?viewContext = viewContext in defaultLayout
            }
        return viewContext