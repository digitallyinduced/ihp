module IHP.IDE.ToolServer.ViewContext where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import IHP.ControllerSupport  (RequestContext (RequestContext))
import IHP.ControllerSupport
import IHP.HaskellSupport
import IHP.ModelSupport
import qualified IHP.ViewSupport as ViewSupport
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.Types
import IHP.IDE.ToolServer.Helper.Controller

instance ViewSupport.CreateViewContext ViewContext where
    type ViewApp ViewContext = ToolServerApplication
    createViewContext = do
        flashMessages <- Session.getAndClearFlashMessages

        let viewContext = ViewContext {
                requestContext = ?requestContext,
                flashMessages,
                controllerContext = ?controllerContext,
                layout = let ?viewContext = viewContext in toolServerLayout,
                appUrl = "http://localhost:" <> tshow appPort
            }
        pure viewContext



