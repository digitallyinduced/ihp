module TurboHaskell.IDE.Data.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.Data.View.ShowDatabase

instance Controller DataController where
    action ShowDatabaseAction = do
        render ShowDatabaseView