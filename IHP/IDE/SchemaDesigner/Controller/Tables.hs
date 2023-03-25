module IHP.IDE.SchemaDesigner.Controller.Tables where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Tables.New
import IHP.IDE.SchemaDesigner.View.Tables.Show
import IHP.IDE.SchemaDesigner.View.Tables.Index
import IHP.IDE.SchemaDesigner.View.Tables.Edit

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findStatementByName, replace, schemaDesignerLayout)
import qualified IHP.SchemaCompiler as SchemaCompiler
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation
import IHP.IDE.SchemaDesigner.Controller.Columns (updateForeignKeyConstraint)
import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

instance Controller TablesController where
    beforeAction = setLayout schemaDesignerLayout

    action TablesAction = do
        statements <- readSchema

        let tableNames = statements |> mapMaybe \case
                StatementCreateTable CreateTable { name } -> Just name
                otherwise -> Nothing

        case headMay tableNames of
            Just tableName -> redirectTo ShowTableAction { tableName }
            otherwise -> render IndexView { .. }

    action ShowTableAction { tableName } = do
        let name = tableName
        statements <- readSchema
        let (Just table) = findStatementByName name statements
        let generatedHaskellCode = SchemaCompiler.compileStatementPreview statements table
        render ShowView { .. }

    action NewTableAction = do
        statements <- readSchema
        render NewTableView { .. }

    action CreateTableAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let validationResult = tableName |> validateTable statements Nothing
        case validationResult of
            Failure message -> do
                setErrorMessage message
                redirectTo TablesAction
            Success -> do
                updateSchema (SchemaOperations.addTable tableName)
                redirectTo ShowTableAction { .. }

    action EditTableAction { .. } = do
        statements <- readSchema
        let tableId = param "tableId"
        render EditTableView { .. }

    action UpdateTableAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let tableId = param "tableId"

        let oldTableName = (get #name . unsafeGetCreateTable) (statements !! tableId)
        let validationResult = tableName |> validateTable statements (Just oldTableName)
        case validationResult of
            Failure message -> do
                setErrorMessage message
                redirectTo ShowTableAction { tableName = oldTableName }
            Success -> do
                updateSchema (SchemaOperations.updateTable tableId tableName)
                redirectTo ShowTableAction { .. }

    action DeleteTableAction { .. } = do
        let tableName = param "tableName"

        updateSchema (SchemaOperations.deleteTable tableName)
        redirectTo TablesAction

validateTable :: [Statement] -> Maybe Text -> Validator Text
validateTable statements = validateNameInSchema "table name" (getAllObjectNames statements)

updateReferenceTableOfForeignKeyConstraint constraint newTableName statements =
    let Just constraintId = elemIndex constraint statements
        tableName = get #tableName constraint
        columnName = get #columnName (get #constraint constraint)
        constraintName = get #constraintName constraint
        referenceTable = newTableName
        Just onDelete = get #onDelete (get #constraint constraint)
    in updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId
