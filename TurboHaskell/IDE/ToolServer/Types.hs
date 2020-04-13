module TurboHaskell.IDE.ToolServer.Types where

import TurboHaskell.Prelude
import qualified TurboHaskell.Controller.Session as Session
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import qualified TurboHaskell.ViewSupport as ViewSupport

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [Session.FlashMessage]
    , layout :: ViewSupport.Layout
    , controllerContext :: ControllerSupport.ControllerContext
    }


data ToolServerApplication = ToolServerApplication deriving (Eq, Show)

data SchemaDesignerController
    = TablesAction
    | ShowTableAction
    deriving (Eq, Show, Data)


