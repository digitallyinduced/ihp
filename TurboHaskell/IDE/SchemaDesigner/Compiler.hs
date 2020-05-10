{-|
Module: TurboHaskell.IDE.SchemaDesigner.Compiler
Description: Compiles AST of SQL to DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module TurboHaskell.IDE.SchemaDesigner.Compiler (compileSql, writeSchema) where

import TurboHaskell.Prelude
import TurboHaskell.IDE.SchemaDesigner.Types
import Data.Maybe (fromJust)
import qualified Data.Text.IO as Text

writeSchema :: [Statement] -> IO ()
writeSchema !statements = do
    let sortedStatements = sortBy compareStatement statements
    Text.writeFile "Application/Schema.sql" (compileSql sortedStatements)

compileSql :: [Statement] -> Text
compileSql statements = statements
    |> map compileStatement
    |> unlines

compileStatement :: Statement -> Text
compileStatement CreateTable { name, columns } = "CREATE TABLE " <> name <> " (\n" <> intercalate ",\n" (map compileColumn columns) <> "\n);"
compileStatement CreateEnumType { name, values } = "CREATE TYPE " <> name <> " AS ENUM (" <> intercalate ", " values <> ");"
compileStatement CreateExtension { name, ifNotExists } = "CREATE EXTENSION " <> (if ifNotExists then "IF NOT EXISTS " else "") <> "\"" <> name <> "\";"
compileStatement AddConstraint { tableName, constraintName, constraint } = "ALTER TABLE " <> tableName <> " ADD CONSTRAINT " <> constraintName <> " " <> compileConstraint constraint <> ";"
compileStatement Comment { content } = "-- " <> content
compileStatement UnknownStatement { raw } = raw

compileConstraint :: Constraint -> Text
compileConstraint ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete } = "FOREIGN KEY (" <> columnName <> ") REFERENCES " <> referenceTable <> (if isJust referenceColumn then " (" <> fromJust referenceColumn <> ")" else "") <> " " <> compileOnDelete onDelete

compileOnDelete :: Maybe OnDelete -> Text
compileOnDelete Nothing = ""
compileOnDelete (Just NoAction) = "ON DELETE NO ACTION"
compileOnDelete (Just Restrict) = "ON DELETE RESTRICT"
compileOnDelete (Just SetNull) = "ON DELETE SET NULL"
compileOnDelete (Just Cascade) = "ON DELETE CASCADE"

compileColumn :: Column -> Text
compileColumn Column { name, columnType, primaryKey, defaultValue, notNull, isUnique } =
    "    " <> unwords (catMaybes
        [ Just name
        , Just columnType
        , fmap compileDefaultValue defaultValue
        , if primaryKey then Just "PRIMARY KEY" else Nothing
        , if notNull then Just "NOT NULL" else Nothing
        , if isUnique then Just "UNIQUE" else Nothing
        ])

compileDefaultValue :: Text -> Text
compileDefaultValue value = "DEFAULT " <> value

compareStatement (CreateTable {}) _ = LT
compareStatement (AddConstraint {}) _ = GT
compareStatement _ _ = EQ