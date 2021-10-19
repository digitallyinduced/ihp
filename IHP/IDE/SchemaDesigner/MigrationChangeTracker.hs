{-|
Module: IHP.IDE.SchemaDesigner.MigrationChangeTracker
Description: Keep's track of unmigrated schema changes
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.IDE.SchemaDesigner.MigrationChangeTracker where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations
import qualified IHP.IDE.SchemaDesigner.Compiler as Compiler
import qualified IHP.IDE.SchemaDesigner.Parser as Parser

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Directory
import qualified Text.Megaparsec as Megaparsec
import Data.Maybe (fromJust)

path = "Application/Migration/unmigrated-changes.sql"

addUnmigratedChange :: [Statement] -> IO ()
addUnmigratedChange statements = do
    let sql = statements |> Compiler.compileSql
    ensureMigrationDirectory
    
    fileExists :: Bool <- Directory.doesFileExist path
    unless fileExists do
        Text.writeFile path header

    Text.appendFile path sql

withUnmigratedChanges :: ([Statement] -> [Statement]) -> IO ()
withUnmigratedChanges patchFunction = do
    ensureMigrationDirectory

    fileExists <- Directory.doesFileExist path
    sql <- if fileExists
                then Text.readFile path
                else pure header

    let result = Megaparsec.runParser Parser.parseDDL (cs path) sql
    case result of
        Left error -> fail (cs $ Megaparsec.errorBundlePretty error)
        Right schema -> do
            let newSchema = patchFunction schema
            Text.writeFile path (Compiler.compileSql newSchema)

takeUnmigratedChanges :: IO Text
takeUnmigratedChanges = do
    fileExists <- Directory.doesFileExist path
    sql <- if fileExists
                then Text.readFile path
                else pure ""

    when fileExists (Directory.removeFile path)

    pure sql

-- | Creates a new tables with a 'id' columns as the primary key
addTable :: Text -> IO ()
addTable tableName = SchemaOperations.addTable tableName [] |> addUnmigratedChange

addColumn :: SchemaOperations.AddColumnOptions -> IO ()
addColumn options = withUnmigratedChanges \schema -> 
        if doesTableExists (get #tableName options) schema
            then SchemaOperations.addColumn options schema
            else
                let
                    addColumnOp = SchemaOperations.appendStatement AddColumn
                            { tableName = get #tableName options
                            , column = SchemaOperations.newColumn options
                            }
                    foreignKeyConstraint = SchemaOperations.newForeignKeyConstraint (get #tableName options) (get #columnName options) (fromJust (get #referenceTable options))
                    foreignKeyIndex = SchemaOperations.newForeignKeyIndex (get #tableName options) (get #columnName options)
                in if get #isReference options
                        then schema
                            |> addColumnOp
                            |> SchemaOperations.appendStatement foreignKeyConstraint
                            |> SchemaOperations.appendStatement foreignKeyIndex
                        else schema
                            |> addColumnOp

deleteColumn :: Text -> Text -> IO ()
deleteColumn tableName columnName = withUnmigratedChanges $ SchemaOperations.appendStatement DropColumn { tableName, columnName }

deleteTable :: Text -> IO ()
deleteTable tableName = withUnmigratedChanges $ SchemaOperations.appendStatement DropTable { tableName }

doesTableExists :: Text -> [Statement] -> Bool
doesTableExists tableName statements =
    statements
    |> filter (\case
        StatementCreateTable (CreateTable { name })  -> name == tableName
        otherwise -> False
        )
    |> null
    |> not

addEnum :: Text -> IO ()
addEnum enumName = withUnmigratedChanges $ SchemaOperations.addEnum enumName

addValueToEnum :: Text -> Text -> IO ()
addValueToEnum enumName enumValueName = withUnmigratedChanges $ SchemaOperations.addValueToEnum enumName enumValueName

ensureMigrationDirectory :: IO ()
ensureMigrationDirectory = Directory.createDirectoryIfMissing False "Application/Migration"

header :: Text
header = cs [plain|-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
--
-- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
--
-- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html


|]