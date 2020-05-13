module TurboHaskell.IDE.SchemaDesigner.Controller.Columns where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext

import TurboHaskell.IDE.SchemaDesigner.View.Columns.New
import TurboHaskell.IDE.SchemaDesigner.View.Columns.Edit
import TurboHaskell.IDE.SchemaDesigner.View.Columns.NewForeignKey
import TurboHaskell.IDE.SchemaDesigner.View.Columns.EditForeignKey

import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace, getDefaultValue)
import qualified TurboHaskell.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import TurboHaskell.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import TurboHaskell.IDE.SchemaDesigner.Controller.Schema

instance Controller ColumnsController where
    
    action NewColumnAction { tableName } = do
        statements <- readSchema
        let (Just table) = findTableByName tableName statements
        primaryKeyExists <- hasPrimaryKey table
        render NewColumnView { .. }

    action CreateColumnAction = do
        let tableName = param "tableName"
        let defaultValue = getDefaultValue (param "columnType") (param "defaultValue")
        let column = Column
                { name = param "name"
                , columnType = param "columnType"
                , primaryKey = (param "primaryKey")
                , defaultValue = defaultValue
                , notNull = (not (param "allowNull"))
                , isUnique = param "isUnique"
                }
        when ((get #name column) == "") do
            setSuccessMessage ("Column Name can not be empty")
            redirectTo ShowTableAction { tableName }
        updateSchema (map (addColumnToTable tableName column))
        redirectTo ShowTableAction { .. }

    action EditColumnAction { .. } = do
        let columnId = param "columnId"
        let name = tableName
        statements <- readSchema
        let (Just table) = findTableByName name statements
        primaryKeyExists <- hasPrimaryKey table
        let table = findTableByName tableName statements
        let columns = maybe [] (get #columns) table
        let column = columns !! columnId
        render EditColumnView { .. }

    action UpdateColumnAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let defaultValue = getDefaultValue (param "columnType") (param "defaultValue")
        let table = findTableByName tableName statements
        let columns = maybe [] (get #columns) table
        let columnId = param "columnId"
        let column = Column
                { name = param "name"
                , columnType = param "columnType"
                , primaryKey = (param "primaryKey")
                , defaultValue = defaultValue
                , notNull = (not (param "allowNull"))
                , isUnique = param "isUnique"
                }
        when ((get #name column) == "") do
            setSuccessMessage ("Column Name can not be empty")
            redirectTo ShowTableAction { tableName }
        updateSchema (map (updateColumnInTable tableName column columnId))
        redirectTo ShowTableAction { .. }

    action DeleteColumnAction { .. } = do
        statements <- readSchema
        let tableName = param "tableName"
        let columnId = param "columnId"
        updateSchema (map (deleteColumnInTable tableName columnId))
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
        updateSchema (addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete)
        redirectTo ShowTableAction { .. }

    action EditForeignKeyAction { tableName, columnName, constraintName, referenceTable } = do
        let name = tableName
        statements <- readSchema
        let tableNames = nameList (getCreateTable statements)
        let (Just statement) = find (\statement -> statement == AddConstraint { tableName = tableName, constraintName = constraintName, constraint = ForeignKeyConstraint { columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (get #onDelete (get #constraint statement)) }}) statements
        onDelete <- case (get #onDelete (get #constraint statement)) of
            Just NoAction -> do pure "NoAction"
            Just Restrict -> do pure "Restrict"
            Just SetNull -> do pure "SetNull"
            Just Cascade -> do pure "Cascade"
            Nothing -> do pure "NoAction"
        render EditForeignKeyView { .. }

    action UpdateForeignKeyAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let columnName = param "columnName"
        let constraintName = param "constraintName"
        let referenceTable = param "referenceTable"
        let constraintId = findIndex (\statement -> statement == AddConstraint { tableName = tableName
            , constraintName = (get #constraintName statement)
            , constraint = ForeignKeyConstraint
                { columnName = columnName
                , referenceTable = (get #referenceTable (get #constraint statement))
                , referenceColumn = (get #referenceColumn (get #constraint statement))
                , onDelete=(get #onDelete (get #constraint statement)) } }) statements
        let onDeleteParam = param @Text "onDelete"
        let onDelete = case onDeleteParam of
                "Restrict" -> Restrict
                "SetNull" -> SetNull
                "Cascade" -> Cascade
                _ -> NoAction
        case constraintId of
            Just constraintId -> updateSchema (updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId)
            Nothing -> putStrLn ("Error")
        redirectTo ShowTableAction { .. }

addColumnToTable :: Text -> Column -> Statement -> Statement
addColumnToTable tableName column (table@CreateTable { name, columns }) | name == tableName =
    table { columns = columns <> [column] }
addColumnToTable tableName column statement = statement

updateColumnInTable :: Text -> Column -> Int -> Statement -> Statement
updateColumnInTable tableName column columnId (table@CreateTable { name, columns }) | name == tableName =
    table { columns = (replace columnId column columns) }
updateColumnInTable tableName column columnId statement = statement

toggleUniqueInColumn :: Text -> Int -> Statement -> Statement
toggleUniqueInColumn tableName columnId (table@CreateTable { name, columns }) | name == tableName =
    table { columns = (replace columnId ((columns !! columnId) { isUnique = (not (get #isUnique (columns !! columnId))) }) columns) }
toggleUniqueInColumn tableName columnId statement = statement

deleteColumnInTable :: Text -> Int -> Statement -> Statement
deleteColumnInTable tableName columnId (table@CreateTable { name, columns }) | name == tableName =
    table { columns = delete (columns !! columnId) columns}
deleteColumnInTable tableName columnId statement = statement

addForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> [Statement] -> [Statement]
addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete list = list <> [AddConstraint { tableName = tableName, constraintName = constraintName, constraint = ForeignKeyConstraint { columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) } }]

updateForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> Int -> [Statement] -> [Statement]
updateForeignKeyConstraint tableName columnName constraintName referenceTable onDelete constraintId list = replace constraintId AddConstraint { tableName = tableName, constraintName = constraintName, constraint = ForeignKeyConstraint { columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) } } list

getCreateTable statements = filter (\statement -> statement == CreateTable { name = (get #name statement), columns = (get #columns statement) }) statements

nameList statements = map (get #name) statements

hasPrimaryKey CreateTable { columns } = do
    let primaryKey = find (\col -> col == Column { name = get #name col
        , columnType = get #columnType col
        , primaryKey = True
        , defaultValue = get #defaultValue col
        , notNull = get #notNull col
        , isUnique = get #isUnique col
        }) columns
    case primaryKey of
        Nothing -> pure False
        _ -> pure True