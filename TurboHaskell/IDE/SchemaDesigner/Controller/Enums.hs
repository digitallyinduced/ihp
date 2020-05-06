module TurboHaskell.IDE.SchemaDesigner.Controller.Enums where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext

import TurboHaskell.IDE.SchemaDesigner.View.Enums.New
import TurboHaskell.IDE.SchemaDesigner.View.Enums.Show
import TurboHaskell.IDE.SchemaDesigner.View.Enums.Edit

import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace)
import qualified TurboHaskell.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import TurboHaskell.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import TurboHaskell.IDE.SchemaDesigner.Controller.Schema

instance Controller EnumsController where
    
    action ShowEnumAction { .. } = do
        statements <- readSchema
        let name = enumName
        render ShowEnumView { .. }

    action NewEnumAction = do
        statements <- readSchema
        render NewEnumView { .. }

    action CreateEnumAction = do
        let enumName = param "enumName"
        updateSchema (addEnum enumName)
        redirectTo ShowEnumAction { .. }

    action EditEnumAction { .. } = do
        statements <- readSchema
        let enumId = param "enumId"
        render EditEnumView { .. }

    action UpdateEnumAction = do
        let enumName = param "enumName"
        let enumId = param "enumId"
        updateSchema (updateEnum enumId enumName)
        redirectTo ShowEnumAction { .. }


updateEnum :: Int -> Text -> [Statement] -> [Statement]
updateEnum enumId enumName list = replace enumId CreateEnumType { name = enumName, values = (get #values (list !! enumId))} list

addEnum :: Text -> [Statement] -> [Statement]
addEnum enumName list = list <> [CreateEnumType { name = enumName, values = []}]