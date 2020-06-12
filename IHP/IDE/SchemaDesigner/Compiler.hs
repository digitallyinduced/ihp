{-|
Module: IHP.IDE.SchemaDesigner.Compiler
Description: Compiles AST of SQL to DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Compiler (compileSql, writeSchema, compileIdentifier) where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
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
compileStatement CreateTable { name, columns } = "CREATE TABLE " <> compileIdentifier name <> " (\n" <> intercalate ",\n" (map compileColumn columns) <> "\n);"
compileStatement CreateEnumType { name, values } = "CREATE TYPE " <> compileIdentifier name <> " AS ENUM (" <> intercalate ", " values <> ");"
compileStatement CreateExtension { name, ifNotExists } = "CREATE EXTENSION " <> (if ifNotExists then "IF NOT EXISTS " else "") <> "\"" <> compileIdentifier name <> "\";"
compileStatement AddConstraint { tableName, constraintName, constraint } = "ALTER TABLE " <> compileIdentifier tableName <> " ADD CONSTRAINT " <> compileIdentifier constraintName <> " " <> compileConstraint constraint <> ";"
compileStatement Comment { content } = "-- " <> content
compileStatement UnknownStatement { raw } = raw

compileConstraint :: Constraint -> Text
compileConstraint ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete } = "FOREIGN KEY (" <> compileIdentifier columnName <> ") REFERENCES " <> compileIdentifier referenceTable <> (if isJust referenceColumn then " (" <> fromJust referenceColumn <> ")" else "") <> " " <> compileOnDelete onDelete

compileOnDelete :: Maybe OnDelete -> Text
compileOnDelete Nothing = ""
compileOnDelete (Just NoAction) = "ON DELETE NO ACTION"
compileOnDelete (Just Restrict) = "ON DELETE RESTRICT"
compileOnDelete (Just SetNull) = "ON DELETE SET NULL"
compileOnDelete (Just Cascade) = "ON DELETE CASCADE"

compileColumn :: Column -> Text
compileColumn Column { name, columnType, primaryKey, defaultValue, notNull, isUnique } =
    "    " <> unwords (catMaybes
        [ Just (compileIdentifier name)
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

compileIdentifier :: _ -> Text
compileIdentifier keyword = case (IHP.Prelude.toUpper keyword) of
    "ABORT" -> tshow keyword
    "ABSOLUTE" -> tshow keyword
    "ACCESS" -> tshow keyword
    "ACTION" -> tshow keyword
    "ADD" -> tshow keyword
    "ADMIN" -> tshow keyword
    "AFTER" -> tshow keyword
    "AGGREGATE" -> tshow keyword
    "ALSO" -> tshow keyword
    "ALTER" -> tshow keyword
    "ASSERTION" -> tshow keyword
    "ASSIGNMENT" -> tshow keyword
    "AT" -> tshow keyword
    "BACKWARD" -> tshow keyword
    "BEFORE" -> tshow keyword
    "BEGIN" -> tshow keyword
    "BY" -> tshow keyword
    "CACHE" -> tshow keyword
    "CALLED" -> tshow keyword
    "CASCADE" -> tshow keyword
    "CHAIN" -> tshow keyword
    "CHARACTERISTICS" -> tshow keyword
    "CHECKPOINT" -> tshow keyword
    "CLASS" -> tshow keyword
    "CLOSE" -> tshow keyword
    "CLUSTER" -> tshow keyword
    "COMMENT" -> tshow keyword
    "COMMIT" -> tshow keyword
    "COMMITTED" -> tshow keyword
    "CONNECTION" -> tshow keyword
    "CONSTRAINTS" -> tshow keyword
    "CONVERSION" -> tshow keyword
    "COPY" -> tshow keyword
    "CREATEDB" -> tshow keyword
    "CREATEROLE" -> tshow keyword
    "CREATEUSER" -> tshow keyword
    "CSV" -> tshow keyword
    "CURSOR" -> tshow keyword
    "CYCLE" -> tshow keyword
    "DATABASE" -> tshow keyword
    "DAY" -> tshow keyword
    "DEALLOCATE" -> tshow keyword
    "DECLARE" -> tshow keyword
    "DEFAULTS" -> tshow keyword
    "DEFERRED" -> tshow keyword
    "DEFINER" -> tshow keyword
    "DELETE" -> tshow keyword
    "DELIMITER" -> tshow keyword
    "DELIMITERS" -> tshow keyword
    "DISABLE" -> tshow keyword
    "DOMAIN" -> tshow keyword
    "DOUBLE" -> tshow keyword
    "DROP" -> tshow keyword
    "EACH" -> tshow keyword
    "ENABLE" -> tshow keyword
    "ENCODING" -> tshow keyword
    "ENCRYPTED" -> tshow keyword
    "ESCAPE" -> tshow keyword
    "EXCLUDING" -> tshow keyword
    "EXCLUSIVE" -> tshow keyword
    "EXECUTE" -> tshow keyword
    "EXPLAIN" -> tshow keyword
    "EXTERNAL" -> tshow keyword
    "FETCH" -> tshow keyword
    "FIRST" -> tshow keyword
    "FORCE" -> tshow keyword
    "FORWARD" -> tshow keyword
    "FUNCTION" -> tshow keyword
    "GLOBAL" -> tshow keyword
    "GRANTED" -> tshow keyword
    "HANDLER" -> tshow keyword
    "HEADER" -> tshow keyword
    "HOLD" -> tshow keyword
    "HOUR" -> tshow keyword
    "IMMEDIATE" -> tshow keyword
    "IMMUTABLE" -> tshow keyword
    "IMPLICIT" -> tshow keyword
    "INCLUDING" -> tshow keyword
    "INCREMENT" -> tshow keyword
    "INDEX" -> tshow keyword
    "INHERIT" -> tshow keyword
    "INHERITS" -> tshow keyword
    "INPUT" -> tshow keyword
    "INSENSITIVE" -> tshow keyword
    "INSERT" -> tshow keyword
    "INSTEAD" -> tshow keyword
    "INVOKER" -> tshow keyword
    "ISOLATION" -> tshow keyword
    "KEY" -> tshow keyword
    "LANCOMPILER" -> tshow keyword
    "LANGUAGE" -> tshow keyword
    "LARGE" -> tshow keyword
    "LAST" -> tshow keyword
    "LEVEL" -> tshow keyword
    "LISTEN" -> tshow keyword
    "LOAD" -> tshow keyword
    "LOCAL" -> tshow keyword
    "LOCATION" -> tshow keyword
    "LOCK" -> tshow keyword
    "LOGIN" -> tshow keyword
    "MATCH" -> tshow keyword
    "MAXVALUE" -> tshow keyword
    "MINUTE" -> tshow keyword
    "MINVALUE" -> tshow keyword
    "MODE" -> tshow keyword
    "MONTH" -> tshow keyword
    "MOVE" -> tshow keyword
    "NAMES" -> tshow keyword
    "NEXT" -> tshow keyword
    "NO" -> tshow keyword
    "NOCREATEDB" -> tshow keyword
    "NOCREATEROLE" -> tshow keyword
    "NOCREATEUSER" -> tshow keyword
    "NOINHERIT" -> tshow keyword
    "NOLOGIN" -> tshow keyword
    "NOSUPERUSER" -> tshow keyword
    "NOTHING" -> tshow keyword
    "NOTIFY" -> tshow keyword
    "NOWAIT" -> tshow keyword
    "OBJECT" -> tshow keyword
    "OF" -> tshow keyword
    "OIDS" -> tshow keyword
    "OPERATOR" -> tshow keyword
    "OPTION" -> tshow keyword
    "OWNER" -> tshow keyword
    "PARTIAL" -> tshow keyword
    "PASSWORD" -> tshow keyword
    "PREPARE" -> tshow keyword
    "PREPARED" -> tshow keyword
    "PRESERVE" -> tshow keyword
    "PRIOR" -> tshow keyword
    "PRIVILEGES" -> tshow keyword
    "PROCEDURAL" -> tshow keyword
    "PROCEDURE" -> tshow keyword
    "QUOTE" -> tshow keyword
    "READ" -> tshow keyword
    "RECHECK" -> tshow keyword
    "REINDEX" -> tshow keyword
    "RELATIVE" -> tshow keyword
    "RELEASE" -> tshow keyword
    "RENAME" -> tshow keyword
    "REPEATABLE" -> tshow keyword
    "REPLACE" -> tshow keyword
    "RESET" -> tshow keyword
    "RESTART" -> tshow keyword
    "RESTRICT" -> tshow keyword
    "RETURNS" -> tshow keyword
    "REVOKE" -> tshow keyword
    "ROLE" -> tshow keyword
    "ROLLBACK" -> tshow keyword
    "ROWS" -> tshow keyword
    "RULE" -> tshow keyword
    "SAVEPOINT" -> tshow keyword
    "SCHEMA" -> tshow keyword
    "SCROLL" -> tshow keyword
    "SECOND" -> tshow keyword
    "SECURITY" -> tshow keyword
    "SEQUENCE" -> tshow keyword
    "SERIALIZABLE" -> tshow keyword
    "SESSION" -> tshow keyword
    "SET" -> tshow keyword
    "SHARE" -> tshow keyword
    "tshow" -> tshow keyword
    "SIMPLE" -> tshow keyword
    "STABLE" -> tshow keyword
    "START" -> tshow keyword
    "STATEMENT" -> tshow keyword
    "STATISTICS" -> tshow keyword
    "STDIN" -> tshow keyword
    "STDOUT" -> tshow keyword
    "STORAGE" -> tshow keyword
    "STRICT" -> tshow keyword
    "SUPERUSER" -> tshow keyword
    "SYSID" -> tshow keyword
    "SYSTEM" -> tshow keyword
    "TABLESPACE" -> tshow keyword
    "TEMP" -> tshow keyword
    "TEMPLATE" -> tshow keyword
    "TEMPORARY" -> tshow keyword
    "TOAST" -> tshow keyword
    "TRANSACTION" -> tshow keyword
    "TRIGGER" -> tshow keyword
    "TRUNCATE" -> tshow keyword
    "TRUSTED" -> tshow keyword
    "TYPE" -> tshow keyword
    "UNCOMMITTED" -> tshow keyword
    "UNENCRYPTED" -> tshow keyword
    "UNKNOWN" -> tshow keyword
    "UNLISTEN" -> tshow keyword
    "UNTIL" -> tshow keyword
    "UPDATE" -> tshow keyword
    "VACUUM" -> tshow keyword
    "VALID" -> tshow keyword
    "VALIDATOR" -> tshow keyword
    "VALUES" -> tshow keyword
    "VARYING" -> tshow keyword
    "VIEW" -> tshow keyword
    "VOLATILE" -> tshow keyword
    "WITH" -> tshow keyword
    "WITHOUT" -> tshow keyword
    "WORK" -> tshow keyword
    "WRITE" -> tshow keyword
    "YEAR" -> tshow keyword
    "ZONE" -> tshow keyword
    "BIGINT" -> tshow keyword
    "BIT" -> tshow keyword
    "BOOLEAN" -> tshow keyword
    "CHAR" -> tshow keyword
    "CHARACTER" -> tshow keyword
    "COALESCE" -> tshow keyword
    "CONVERT" -> tshow keyword
    "DEC" -> tshow keyword
    "DECIMAL" -> tshow keyword
    "EXISTS" -> tshow keyword
    "EXTRACT" -> tshow keyword
    "FLOAT" -> tshow keyword
    "GREATEST" -> tshow keyword
    "INOUT" -> tshow keyword
    "INT" -> tshow keyword
    "INTEGER" -> tshow keyword
    "INTERVAL" -> tshow keyword
    "LEAST" -> tshow keyword
    "NATIONAL" -> tshow keyword
    "NCHAR" -> tshow keyword
    "NONE" -> tshow keyword
    "NULLIF" -> tshow keyword
    "NUMERIC" -> tshow keyword
    "OUT" -> tshow keyword
    "OVERLAY" -> tshow keyword
    "POSITION" -> tshow keyword
    "PRECISION" -> tshow keyword
    "REAL" -> tshow keyword
    "ROW" -> tshow keyword
    "SETOF" -> tshow keyword
    "SMALLINT" -> tshow keyword
    "SUBSTRING" -> tshow keyword
    "TIME" -> tshow keyword
    "TIMESTAMP" -> tshow keyword
    "TREAT" -> tshow keyword
    "TRIM" -> tshow keyword
    "VARCHAR" -> tshow keyword
    _ -> cs keyword
    