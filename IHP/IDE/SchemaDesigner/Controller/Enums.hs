module IHP.IDE.SchemaDesigner.Controller.Enums where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Enums.New
import IHP.IDE.SchemaDesigner.View.Enums.Show
import IHP.IDE.SchemaDesigner.View.Enums.Edit

import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Compiler
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findStatementByName, findStatementByName, removeQuotes, replace, isIllegalKeyword)
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import IHP.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import IHP.IDE.SchemaDesigner.Controller.Schema
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

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
                updateSchema (addEnum enumName)
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

addEnum :: Text -> [Statement] -> [Statement]
addEnum enumName list = list <> [CreateEnumType { name = enumName, values = []}]

deleteEnum :: Int -> [Statement] -> [Statement]
deleteEnum tableId list = delete (list !! tableId) list

validateEnum :: [Statement] -> Maybe Text -> Validator Text
validateEnum statements = validateNameInSchema "enum name" (getAllObjectNames statements)
