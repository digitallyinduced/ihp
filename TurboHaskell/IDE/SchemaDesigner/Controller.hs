module TurboHaskell.IDE.SchemaDesigner.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.SchemaDesigner.View
import TurboHaskell.IDE.SchemaDesigner.View
import TurboHaskell.IDE.SchemaDesigner.Parser

instance Controller SchemaDesignerController where
    action TablesAction = do
        statements <- readSchema
        render IndexView { .. }

    action ShowTableAction = do
        let name = param "name"
        statements <- readSchema
        render ShowView { .. }


readSchema :: _ => _
readSchema = parseSchemaSql >>= \case
        Left error -> do renderPlain error; pure []
        Right statements -> pure statements