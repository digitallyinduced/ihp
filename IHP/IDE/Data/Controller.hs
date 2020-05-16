module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.Data.View.ShowDatabase

instance Controller DataController where
    action ShowDatabaseAction = do
        render ShowDatabaseView