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
import qualified IHP.IDE.SchemaDesigner.MigrationChangeTracker as MigrationChangeTracker

instance Controller TablesController where
    beforeAction = setLayout schemaDesignerLayout

    action TablesAction = do
        statements <- readSchema
        render IndexView { .. }

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
                MigrationChangeTracker.addTable tableName
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
                let updateConstraintsStatement =
                        referencingTableForeignKeyConstraints oldTableName statements
                            |> map (\constraint -> updateReferenceTableOfForeignKeyConstraint constraint tableName statements)

                forEach updateConstraintsStatement updateSchema
                updateSchema (updateTable tableId tableName)
                redirectTo ShowTableAction { .. }

    action DeleteTableAction { .. } = do
        let tableId = param "tableId"
        let tableName = param "tableName"
        updateSchema (deleteTable tableId)
        updateSchema (deleteForeignKeyConstraints tableName)
        redirectTo TablesAction



updateTable :: Int -> Text -> [Statement] -> [Statement]
updateTable tableId tableName list = replace tableId (StatementCreateTable CreateTable { name = tableName, columns = get #columns table, primaryKeyConstraint = get #primaryKeyConstraint table, constraints = get #constraints table }) list
  where table = unsafeGetCreateTable (list !! tableId)

deleteTable :: Int -> [Statement] -> [Statement]
deleteTable tableId list = delete (list !! tableId) list

deleteForeignKeyConstraints :: Text -> [Statement] -> [Statement]
deleteForeignKeyConstraints tableName list = filter (\con -> not (con == AddConstraint { tableName = tableName, constraintName = get #constraintName con, constraint = get #constraint con })) list

validateTable :: [Statement] -> Maybe Text -> Validator Text
validateTable statements = validateNameInSchema "table name" (getAllObjectNames statements)

referencingTableForeignKeyConstraints tableName statements =
    filter (\statement ->
        statement ==
            AddConstraint
                { tableName = (get #tableName statement)
                , constraintName = (get #constraintName statement)
                , constraint =
                    ForeignKeyConstraint
                        { columnName = (get #columnName (get #constraint statement))
                        , referenceTable = tableName
                        , referenceColumn = (get #referenceColumn (get #constraint statement))
                        , onDelete = (get #onDelete (get #constraint statement))
                        }
                }
        ) statements

updateReferenceTableOfForeignKeyConstraint constraint newTableName statements =
    let Just constraintId = elemIndex constraint statements
        tableName = get #tableName constraint
        columnName = get #columnName (get #constraint constraint)
        constraintName = get #constraintName constraint
        referenceTable = newTableName
        Just onDelete = get #onDelete (get #constraint constraint)
    in updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId
