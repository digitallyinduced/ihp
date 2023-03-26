module IHP.IDE.SchemaDesigner.Controller.Enums where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Enums.New
import IHP.IDE.SchemaDesigner.View.Enums.Show
import IHP.IDE.SchemaDesigner.View.Enums.Edit

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (replace, schemaDesignerLayout)
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

instance Controller EnumsController where
    beforeAction = setLayout schemaDesignerLayout

    action ShowEnumAction { .. } = do
        statements <- readSchema
        let name = enumName
        render ShowEnumView { .. }

    action NewEnumAction = do
        statements <- readSchema
        render NewEnumView { .. }

    action CreateEnumAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let validationResult = enumName |> validateEnum statements Nothing
        case validationResult of
            Failure message -> do
                setErrorMessage message
                redirectTo TablesAction
            Success -> do
                updateSchema $ SchemaOperations.addEnum enumName
                redirectTo ShowEnumAction { .. }

    action EditEnumAction { .. } = do
        statements <- readSchema
        let enumId = param "enumId"
        render EditEnumView { .. }

    action UpdateEnumAction = do
        statements <- readSchema
        let enumName = param "enumName"
        let enumId = param "enumId"
        let oldEnumName = get #name (statements !! enumId)
        let validationResult = enumName |> validateEnum statements (Just oldEnumName)
        case validationResult of
            Failure message -> do
                setErrorMessage message
                redirectTo ShowEnumAction { enumName = oldEnumName }
            Success -> do
                updateSchema (updateEnum enumId enumName)
                redirectTo ShowEnumAction { .. }

    action DeleteEnumAction { .. } = do
        let tableId = param "tableId"
        updateSchema (deleteEnum tableId)
        redirectTo TablesAction

updateEnum :: Int -> Text -> [Statement] -> [Statement]
updateEnum enumId enumName list = replace enumId CreateEnumType { name = enumName, values = (get #values (list !! enumId))} list

deleteEnum :: Int -> [Statement] -> [Statement]
deleteEnum tableId list = delete (list !! tableId) list

validateEnum :: [Statement] -> Maybe Text -> Validator Text
validateEnum statements = validateNameInSchema "enum name" (getAllObjectNames statements)
