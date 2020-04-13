module TurboHaskell.IDE.ToolServer.ViewContext where

import TurboHaskell.Prelude
import qualified TurboHaskell.Controller.Session as Session
import TurboHaskell.ControllerSupport  (RequestContext (RequestContext))
import qualified TurboHaskell.ControllerSupport
import TurboHaskell.HaskellSupport
import TurboHaskell.ModelSupport
import Application.Helper.Controller
import Generated.Types
import qualified TurboHaskell.ViewSupport as ViewSupport
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout

instance ViewSupport.CreateViewContext ViewContext where
    type ViewApp ViewContext = ToolServerApplication
    createViewContext = do
        flashMessages <- Session.getAndClearFlashMessages
        let viewContext = ViewContext {
                requestContext = ?requestContext,
                flashMessages,
                controllerContext = ?controllerContext,
                layout = let ?viewContext = viewContext in toolServerLayout
            }
        pure viewContext
