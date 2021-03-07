module IHP.IDE.SchemaDesigner.Controller.EnumValues where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.EnumValues.New
import IHP.IDE.SchemaDesigner.View.EnumValues.Edit

import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Compiler
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findStatementByName, findStatementByName, removeQuotes, replace)
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import IHP.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import IHP.IDE.SchemaDesigner.Controller.Schema
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.Controller.Helper

instance Controller EnumValuesController where
    beforeAction = setLayout schemaDesignerLayout
    
    action NewEnumValueAction { enumName } = do
        statements <- readSchema
        render NewEnumValueView { .. }

    action CreateEnumValueAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let enumValueName = param "enumValueName"
        let enumValuesInUse = getAllEnumValues statements
        let validationResult = enumValueName |> validateAll [nonEmptyEnumValue, isUniqueEnumValue enumValuesInUse Nothing]
        case validationResult of 
            Failure message -> 
                setErrorMessage message
            Success ->  
                updateSchema (map (addValueToEnum enumName enumValueName))
        redirectTo ShowEnumAction { .. }

    action EditEnumValueAction { .. } = do
        statements <- readSchema
        let valueId = param "valueId"
        let enumName = param "enumName"
        let enum = findStatementByName enumName statements
        let values = maybe [] (get #values) enum
        let value = (cs (values !! valueId))
        render EditEnumValueView { .. }

    action UpdateEnumValueAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let valueId = param "valueId"
        let newValue = param "enumValueName" :: Text
        let enum = findStatementByName enumName statements
        let values = maybe [] (get #values) enum
        let value = values !! valueId
        let enumValuesInUse = getAllEnumValues statements
        let validationResult = newValue |> validateAll [nonEmptyEnumValue, isUniqueEnumValue enumValuesInUse (Just value)]
        case validationResult of
            Failure message ->
                setErrorMessage message
            Success ->
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
    table { values = values <> [enumValueName] }
addValueToEnum enumName enumValueName statement = statement

updateValueInEnum :: Text -> Text -> Int -> Statement -> Statement
updateValueInEnum enumName value valueId (table@CreateEnumType { name, values }) | name == enumName =
    table { values = (replace valueId value values) }
updateValueInEnum enumName value valueId statement = statement

deleteValueInEnum :: Text -> Int -> Statement -> Statement
deleteValueInEnum enumName valueId (table@CreateEnumType { name, values }) | name == enumName =
    table { values = delete (values !! valueId) values}
deleteValueInEnum enumName valueId statement = statement

nonEmptyEnumValue :: Validator Text
nonEmptyEnumValue "" = Failure "Enum Value cannot be empty" 
nonEmptyEnumValue _  = Success

isUniqueEnumValue :: [Text] -> Maybe Text -> Validator Text
isUniqueEnumValue enumValuesInUse oldEnumValue enumValue 
    | enumValue `elem` enumValuesInUse && Just enumValue /= oldEnumValue  = Failure "Enum Value must be globally unique"
    | otherwise                                                           = Success

getAllEnumValues :: [Statement] -> [Text]
getAllEnumValues statements = concat $ mapMaybe extractEnumValues statements 
    where 
        extractEnumValues CreateEnumType { values } = Just values
        extractEnumValues _                         = Nothing 