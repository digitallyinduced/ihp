module TurboHaskell.IDE.SchemaDesigner.Controller.EnumValues where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext

import TurboHaskell.IDE.SchemaDesigner.View.EnumValues.New
import TurboHaskell.IDE.SchemaDesigner.View.EnumValues.Edit

import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace)
import qualified TurboHaskell.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import TurboHaskell.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import TurboHaskell.IDE.SchemaDesigner.Controller.Schema

instance Controller EnumValuesController where
    
    action NewEnumValueAction { enumName } = do
        statements <- readSchema
        render NewEnumValueView { .. }

    action CreateEnumValueAction = do
        let enumName = param "enumName"
        let enumValueName = param "enumValueName"
        updateSchema (map (addValueToEnum enumName enumValueName))
        redirectTo ShowEnumAction { .. }

    action EditEnumValueAction { .. } = do
        statements <- readSchema
        let valueId = param "valueId"
        let enumName = param "enumName"
        let enum = findEnumByName enumName statements
        let values = maybe [] (get #values) enum
        let value = removeQuotes (cs (values !! valueId))
        render EditEnumValueView { .. }

    action UpdateEnumValueAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let valueId = param "valueId"
        let newValue = param "enumValueName" :: Text
        let enum = findEnumByName enumName statements
        let values = maybe [] (get #values) enum
        let value = values !! valueId
        when (newValue == "") do
            setSuccessMessage ("Column Name can not be empty")
            redirectTo ShowEnumAction { enumName }
        updateSchema (map (updateValueInEnum enumName newValue valueId))
        redirectTo ShowEnumAction { .. }

    action DeleteEnumValueAction { .. } = do
        statements <- readSchema
        let enumName = param "enumName"
        let valueId = param "valueId"
        updateSchema (map (deleteValueInEnum enumName valueId))
        redirectTo ShowEnumAction { .. }

addValueToEnum :: Text -> Text -> Statement -> Statement
addValueToEnum enumName enumValueName (table@CreateEnumType { name, values }) | name == enumName =
    table { values = values <> ["'" <> enumValueName <> "'"] }
addValueToEnum enumName enumValueName statement = statement

updateValueInEnum :: Text -> Text -> Int -> Statement -> Statement
updateValueInEnum enumName value valueId (table@CreateEnumType { name, values }) | name == enumName =
    table { values = (replace valueId ("'" <> value <> "'") values) }
updateValueInEnum enumName value valueId statement = statement

deleteValueInEnum :: Text -> Int -> Statement -> Statement
deleteValueInEnum enumName valueId (table@CreateEnumType { name, values }) | name == enumName =
    table { values = delete (values !! valueId) values}
deleteValueInEnum enumName valueId statement = statement