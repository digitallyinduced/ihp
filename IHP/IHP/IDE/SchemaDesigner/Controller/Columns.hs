module IHP.IDE.SchemaDesigner.Controller.Columns where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Columns.New
import IHP.IDE.SchemaDesigner.View.Columns.Edit
import IHP.IDE.SchemaDesigner.View.Columns.NewForeignKey
import IHP.IDE.SchemaDesigner.View.Columns.EditForeignKey

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout, findStatementByName, replace, findForeignKey, findTableIndex)
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

instance Controller ColumnsController where
    beforeAction = setLayout schemaDesignerLayout

    action NewColumnAction { tableName } = do
        statements <- readSchema
        let (Just table) = findStatementByName tableName statements
        let tableNames = nameList (getCreateTable statements)
        let enumNames = nameList (getCreateEnum statements)
        render NewColumnView { .. }

    action CreateColumnAction = do
        let tableName = param "tableName"
        let columnName = param "name"
        let validationResult = columnName |> validateColumn
        case validationResult of
            Failure message ->
                setErrorMessage message
            Success -> do
                let options = SchemaOperations.AddColumnOptions
                        { tableName
                        , columnName
                        , columnType = param "columnType"
                        , defaultValue = param "defaultValue"
                        , isArray = param "isArray"
                        , allowNull = param "allowNull"
                        , isUnique = param "isUnique"
                        , isReference = param "isReference"
                        , referenceTable = paramOrNothing "referenceTable"
                        , primaryKey = param "primaryKey"
                        , withIndex = paramOrDefault False "withIndex"
                        , autoPolicy = paramOrDefault False "autoPolicy"
                        }
                updateSchema $ SchemaOperations.addColumn options

        redirectTo ShowTableAction { .. }

    action EditColumnAction { .. } = do
        let columnId = param "columnId"
        let name = tableName
        statements <- readSchema
        let (Just table) = findStatementByName name statements
        let table = findStatementByName tableName statements
        let columns = maybe [] (get #columns . unsafeGetCreateTable) table
        let column = columns !! columnId
        let enumNames = nameList (getCreateEnum statements)
        render EditColumnView { .. }

    action UpdateColumnAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let columnName = param "name"
        let validationResult = columnName |> validateColumn

        case validationResult of
            Failure message ->
                setErrorMessage message
            Success -> do
                let options = SchemaOperations.UpdateColumnOptions
                        { tableName
                        , columnName
                        , columnId = param "columnId"
                        , defaultValue = param "defaultValue"
                        , isArray = param "isArray"
                        , columnType = param "columnType"
                        , allowNull = param "allowNull"
                        , isUnique = param "isUnique"
                        , primaryKey = param "primaryKey"
                        }

                updateSchema $ SchemaOperations.updateColumn options
        redirectTo ShowTableAction { .. }

    action DeleteColumnAction { .. } = do
        statements <- readSchema
        let tableName = param "tableName"
        let columnId = param "columnId"
        let columnName = param "columnName"

        let options = SchemaOperations.DeleteColumnOptions
                { tableName
                , columnName
                , columnId
                }

        updateSchema $ SchemaOperations.deleteColumn options

        redirectTo ShowTableAction { .. }

    action ToggleColumnUniqueAction { .. } = do
        let tableName = param "tableName"
        let columnId = param "columnId"
        updateSchema (map (toggleUniqueInColumn tableName columnId))
        redirectTo ShowTableAction { .. }

    -- FOREIGN KEYS
    action NewForeignKeyAction { tableName, columnName } = do
        let name = tableName
        statements <- readSchema
        let tableNames = nameList (getCreateTable statements)
        render NewForeignKeyView { .. }

    action CreateForeignKeyAction = do
        let tableName = param "tableName"
        let columnName = param "columnName"
        let constraintName = param "constraintName"
        let referenceTable = param "referenceTable"
        let onDelete = NoAction
        updateSchema (SchemaOperations.addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete)
        redirectTo ShowTableAction { .. }

    action EditForeignKeyAction { tableName, columnName, constraintName, referenceTable } = do
        let name = tableName
        statements <- readSchema
        let tableNames = nameList (getCreateTable statements)
        let (Just statement) = find (\statement -> statement == AddConstraint { tableName = tableName, deferrable = Nothing, deferrableType = Nothing, constraint = ForeignKeyConstraint { name = Just constraintName, columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (get #onDelete (get #constraint statement)) }}) statements
        onDelete <- case (get #onDelete (get #constraint statement)) of
            Just NoAction -> do pure "NoAction"
            Just Restrict -> do pure "Restrict"
            Just SetNull -> do pure "SetNull"
            Just SetDefault -> do pure "SetDefault"
            Just Cascade -> do pure "Cascade"
            Nothing -> do pure "NoAction"
        render EditForeignKeyView { .. }

    action UpdateForeignKeyAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let columnName = param "columnName"
        let constraintName = param "constraintName"
        let referenceTable = param "referenceTable"
        let constraintId = statements
                |> findIndex \case
                    AddConstraint { tableName = fkTable, constraint = ForeignKeyConstraint { columnName = fkColumnName } } -> tableName == fkTable && columnName == fkColumnName
                    otherwise -> False
        let onDeleteParam = param @Text "onDelete"
        let onDelete = case onDeleteParam of
                "Restrict" -> Restrict
                "SetNull" -> SetNull
                "SetDefault" -> SetDefault
                "Cascade" -> Cascade
                _ -> NoAction
        case constraintId of
            Just constraintId -> updateSchema (updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId)
            Nothing -> putStrLn ("Error")
        redirectTo ShowTableAction { .. }

    action DeleteForeignKeyAction { constraintName, tableName } = do
        statements <- readSchema
        updateSchema (deleteForeignKeyConstraint constraintName)
        redirectTo ShowTableAction { .. }

toggleUniqueInColumn :: Text -> Int -> Statement -> Statement
toggleUniqueInColumn tableName columnId (StatementCreateTable table@CreateTable { name, columns })
    | name == tableName = StatementCreateTable $
        table { columns = (replace columnId ((columns !! columnId) { isUnique = (not (get #isUnique (columns !! columnId))) }) columns) }
toggleUniqueInColumn tableName columnId statement = statement

deleteColumnInTable :: Text -> Int -> Statement -> Statement
deleteColumnInTable tableName columnId (StatementCreateTable table@CreateTable { name, columns })
    | name == tableName = StatementCreateTable $
        table { columns = delete (columns !! columnId) columns}
deleteColumnInTable tableName columnId statement = statement


updateForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> Int -> [Statement] -> [Statement]
updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId list = replace constraintId AddConstraint { tableName = tableName, deferrable = Nothing, deferrableType = Nothing, constraint = ForeignKeyConstraint { name = Just constraintName, columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) } } list

deleteForeignKeyConstraint :: Text -> [Statement] -> [Statement]
deleteForeignKeyConstraint constraintName = filter \case
    AddConstraint { constraint } | get #name constraint == Just constraintName -> False
    otherwise -> True


deleteTableIndex :: Text -> [Statement] -> [Statement]
deleteTableIndex indexName list =
    list
    |> filter \case
        CreateIndex { indexName = indexName' } -> indexName' /= indexName
        otherwise -> True

getCreateTable :: [Statement] -> [CreateTable]
getCreateTable statements = foldr step [] statements
  where
    step (StatementCreateTable createTable) createTables = createTable : createTables
    step _ createTables = createTables

getCreateEnum statements = filter isCreateEnumType statements
isCreateEnumType CreateEnumType {} = True
isCreateEnumType _ = False

nameList statements = map (get #name) statements


validateColumn :: Validator Text
validateColumn = validateNameInSchema "column name" [] Nothing

referencingColumnForeignKeyConstraints tableName columnName =
    find \case
        AddConstraint { tableName = constraintTable, constraint = ForeignKeyConstraint { columnName = fkColumnName  }  } -> constraintTable == tableName && fkColumnName == columnName
        otherwise -> False
