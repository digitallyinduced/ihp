module IHP.IDE.SchemaDesigner.Controller.Tables where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext

import IHP.IDE.SchemaDesigner.View.Tables.New
import IHP.IDE.SchemaDesigner.View.Tables.Show
import IHP.IDE.SchemaDesigner.View.Tables.Index
import IHP.IDE.SchemaDesigner.View.Tables.Edit

import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Compiler
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace)
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import IHP.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text
import IHP.IDE.SchemaDesigner.Controller.Schema

instance Controller TablesController where
    
    action TablesAction = do
        statements <- readSchema
        render IndexView { .. }

    action ShowTableAction { tableName } = do
        let name = tableName
        statements <- readSchema
        let (Just table) = findTableByName name statements
        let generatedHaskellCode = SchemaCompiler.compileStatementPreview statements table
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

    action DeleteTableAction { .. } = do
        let tableId = param "tableId"
        updateSchema (deleteTable tableId)
        redirectTo TablesAction


addTable :: Text -> [Statement] -> [Statement]
addTable tableName list = list <> [CreateTable { name = tableName, columns = [Column
                { name = "id"
                , columnType = "UUID"
                , primaryKey = True
                , defaultValue = "uuid_generate_v4()"
                , notNull = True
                , isUnique = False
                }] }]

updateTable :: Int -> Text -> [Statement] -> [Statement]
updateTable tableId tableName list = replace tableId CreateTable { name = tableName, columns = (get #columns (list !! tableId))} list

deleteTable :: Int -> [Statement] -> [Statement]
deleteTable tableId list = delete (list !! tableId) list