module TurboHaskell.IDE.SchemaDesigner.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.SchemaDesigner.View.Columns.New
import TurboHaskell.IDE.SchemaDesigner.View.Columns.Edit
import TurboHaskell.IDE.SchemaDesigner.View.Tables.New
import TurboHaskell.IDE.SchemaDesigner.View.Tables.Show
import TurboHaskell.IDE.SchemaDesigner.View.Tables.Index
import TurboHaskell.IDE.SchemaDesigner.View.Tables.Edit
import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName)
import qualified System.Process as Process
import qualified Data.List as List

instance Controller SchemaDesignerController where
    -- TABLES
    action TablesAction = do
        statements <- readSchema
        render IndexView { .. }

    action ShowTableAction { tableName } = do
        let name = tableName
        statements <- readSchema
        render ShowView { .. }

    action NewTableAction = do
        statements <- readSchema
        render NewTableView { .. }

    action CreateTableAction = do
        let tableName = param "tableName"
        updateSchema (addTable tableName)
        redirectTo ShowTableAction { .. }
    
    action EditTableAction { .. } = do
        statements <- readSchema
        let tableId = param "tableId"
        render EditTableView { .. }

    action UpdateTableAction = do
        let tableName = param "tableName"
        let tableId = param "tableId"
        updateSchema (updateTable tableId tableName)
        redirectTo ShowTableAction { .. }

    -- COLUMNS
    action NewColumnAction { tableName } = do
        statements <- readSchema
        render NewColumnView { .. }

    action CreateColumnAction = do
        let tableName = param "tableName"
        let defaultValue = getDefaultValue (param "columnType") (param "defaultValue") (param "customDefaultValue")
        let column = Column
                { name = param "name"
                , columnType = param "columnType"
                , primaryKey = (param "primaryKey")
                , defaultValue = defaultValue
                , notNull = (not (param "allowNull"))
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
        let table = findTableByName tableName statements
        let columns = maybe [] (get #columns) table
        let column = columns !! columnId
        render EditColumnView { .. }

    action UpdateColumnAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let defaultValue = getDefaultValue (param "columnType") (param "defaultValue") (param "customDefaultValue")
        let table = findTableByName tableName statements
        let columns = maybe [] (get #columns) table
        let columnId = param "columnId"
        let column = Column
                { name = param "name"
                , columnType = param "columnType"
                , primaryKey = (param "primaryKey")
                , defaultValue = defaultValue
                , notNull = (not (param "allowNull"))
                }
        when ((get #name column) == "") do
            setSuccessMessage ("Column Name can not be empty")
            redirectTo ShowTableAction { tableName }
        updateSchema (map (updateColumnInTable tableName column columnId))
        redirectTo ShowTableAction { .. }

    -- DB
    action PushToDbAction = do
        Process.system "make db"
        redirectTo TablesAction

readSchema :: _ => _
readSchema = parseSchemaSql >>= \case
        Left error -> do renderPlain error; pure []
        Right statements -> pure statements

updateSchema :: _ => _
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    writeSchema statements'

addColumnToTable :: Text -> Column -> Statement -> Statement
addColumnToTable tableName column (table@CreateTable { name, columns }) | name == tableName =
    table { columns = columns <> [column] }
addColumnToTable tableName column statement = statement

addTable tableName list = list <> [CreateTable { name = tableName, columns = [] }]

updateTable :: Int -> Text -> [Statement] -> [Statement]
updateTable tableId tableName list = replace tableId CreateTable { name = tableName, columns = (get #columns (list !! tableId))} list

updateColumnInTable :: Text -> Column -> Int -> Statement -> Statement
updateColumnInTable tableName column columnId (table@CreateTable { name, columns }) | name == tableName =
    table { columns = (replace columnId column columns) }
updateColumnInTable tableName column columnId statement = statement

replace :: Int -> a -> [a] -> [a]
replace i e xs = case List.splitAt i xs of
   (before, _:after) -> before ++ (e: after)

getDefaultValue :: Text -> Text -> Text -> Maybe Text
getDefaultValue columnType value custom = case value of
    "EMPTY" -> Just "''"
    "NULL" -> Just "NULL"
    "CUSTOM" -> case columnType of
        "TEXT" -> Just ("'" <> custom <> "'")
        "INT" -> Just custom
        "UUID" -> Just ("'" <> custom <> "'")
        "BOOLEAN" -> Just custom
        "TIMESTAMP WITH TIME ZONE" -> Just ("'" <> custom <> "'")
        "REAL" -> Just custom
        "DOUBLE PRECISION" -> Just custom
        "POINT" -> Just ("'" <> custom <> "'")
        _ -> Just ("'" <> custom <> "'")
    _ -> Nothing