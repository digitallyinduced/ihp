module IHP.IDE.SchemaDesigner.Controller.EnumValues where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.EnumValues.New
import IHP.IDE.SchemaDesigner.View.EnumValues.Edit

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findStatementByName, replace, schemaDesignerLayout)
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

instance Controller EnumValuesController where
    beforeAction = setLayout schemaDesignerLayout

    action NewEnumValueAction { enumName } = do
        statements <- readSchema
        render NewEnumValueView { .. }

    action CreateEnumValueAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let enumValueName = param "enumValueName"
        let validationResult = enumValueName |> validateEnumValue statements Nothing
        case validationResult of
            Failure message ->
                setErrorMessage message
            Success ->
                updateSchema (map (addValueToEnum enumName enumValueName))

        -- The form to save an enum has two save buttons:
        --
        -- 1. Save
        -- 2. Save & Add another
        --
        case paramOrDefault @Text "Save" "submit" of
            "Save" -> redirectTo ShowEnumAction { .. }
            "Save & Add Another" -> redirectTo NewEnumValueAction { .. }

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
        let validationResult = newValue |> validateEnumValue statements (Just value)
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

validateEnumValue :: [Statement] -> Maybe Text -> Validator Text
validateEnumValue statements = validateNameInSchema "enum value" (getAllObjectNames statements)

getAllEnumValues :: [Statement] -> [Text]
getAllEnumValues statements = concat $ mapMaybe extractEnumValues statements
    where
        extractEnumValues CreateEnumType { values } = Just values
        extractEnumValues _ = Nothing