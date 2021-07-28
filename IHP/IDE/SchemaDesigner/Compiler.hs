{-|
Module: IHP.IDE.SchemaDesigner.Compiler
Description: Compiles AST of SQL to DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Compiler (compileSql, writeSchema, compileIdentifier, compileExpression, compilePostgresType) where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import Data.Maybe (fromJust)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

writeSchema :: [Statement] -> IO ()
writeSchema !statements = do
    let sortedStatements = sortBy compareStatement statements
    Text.writeFile "Application/Schema.sql" (compileSql sortedStatements)

compileSql :: [Statement] -> Text
compileSql statements = statements
    |> map compileStatement
    |> unlines

compileStatement :: Statement -> Text
compileStatement (StatementCreateTable CreateTable { name, columns, primaryKeyConstraint, constraints }) = "CREATE TABLE " <> compileIdentifier name <> " (\n" <> intercalate ",\n" (map (compileColumn primaryKeyConstraint) columns <> maybe [] ((:[]) . indent) (compilePrimaryKeyConstraint primaryKeyConstraint) <> map (indent . compileConstraint) constraints) <> "\n);"
compileStatement CreateEnumType { name, values } = "CREATE TYPE " <> compileIdentifier name <> " AS ENUM (" <> intercalate ", " (values |> map TextExpression |> map compileExpression) <> ");"
compileStatement CreateExtension { name, ifNotExists } = "CREATE EXTENSION " <> (if ifNotExists then "IF NOT EXISTS " else "") <> "\"" <> compileIdentifier name <> "\";"
compileStatement AddConstraint { tableName, constraintName, constraint } = "ALTER TABLE " <> compileIdentifier tableName <> " ADD CONSTRAINT " <> compileIdentifier constraintName <> " " <> compileConstraint constraint <> ";"
compileStatement Comment { content } = "-- " <> content
compileStatement CreateIndex { indexName, unique, tableName, expressions } = "CREATE" <> (if unique then " UNIQUE " else " ") <> "INDEX " <> indexName <> " ON " <> tableName <> " (" <> (intercalate ", " (map compileExpression expressions)) <> ");"
compileStatement CreateFunction { functionName, functionBody, orReplace } = "CREATE " <> (if orReplace then "OR REPLACE " else "") <> "FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$" <> functionBody <> "$$ language plpgsql;"
compileStatement UnknownStatement { raw } = raw <> ";"

-- | Emit a PRIMARY KEY constraint when there are multiple primary key columns
compilePrimaryKeyConstraint :: PrimaryKeyConstraint -> Maybe Text
compilePrimaryKeyConstraint PrimaryKeyConstraint { primaryKeyColumnNames } =
    case primaryKeyColumnNames of
        [] -> Nothing
        [_] -> Nothing
        names -> Just $ "PRIMARY KEY(" <> intercalate ", " names <> ")"

compileConstraint :: Constraint -> Text
compileConstraint ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete } = "FOREIGN KEY (" <> compileIdentifier columnName <> ") REFERENCES " <> compileIdentifier referenceTable <> (if isJust referenceColumn then " (" <> fromJust referenceColumn <> ")" else "") <> " " <> compileOnDelete onDelete
compileConstraint UniqueConstraint { columnNames } = "UNIQUE(" <> intercalate ", " columnNames <> ")"
compileConstraint CheckConstraint { checkExpression } = "CHECK (" <> compileExpression checkExpression <> ")"

compileOnDelete :: Maybe OnDelete -> Text
compileOnDelete Nothing = ""
compileOnDelete (Just NoAction) = "ON DELETE NO ACTION"
compileOnDelete (Just Restrict) = "ON DELETE RESTRICT"
compileOnDelete (Just SetNull) = "ON DELETE SET NULL"
compileOnDelete (Just SetDefault) = "ON DELETE SET DEFAULT"
compileOnDelete (Just Cascade) = "ON DELETE CASCADE"

compileColumn :: PrimaryKeyConstraint -> Column -> Text
compileColumn primaryKeyConstraint Column { name, columnType, defaultValue, notNull, isUnique } =
    "    " <> unwords (catMaybes
        [ Just (compileIdentifier name)
        , Just (compilePostgresType columnType)
        , fmap compileDefaultValue defaultValue
        , primaryKeyColumnConstraint
        , if notNull then Just "NOT NULL" else Nothing
        , if isUnique then Just "UNIQUE" else Nothing
        ])
    where
        -- Emit a PRIMARY KEY column constraint if this is the only primary key column
        primaryKeyColumnConstraint = case primaryKeyConstraint of
            PrimaryKeyConstraint [primaryKeyColumn]
                | name == primaryKeyColumn -> Just "PRIMARY KEY"
                | otherwise -> Nothing
            PrimaryKeyConstraint _ -> Nothing

compileDefaultValue :: Expression -> Text
compileDefaultValue value = "DEFAULT " <> compileExpression value

compileExpression :: Expression -> Text
compileExpression (TextExpression value) = "'" <> value <> "'"
compileExpression (VarExpression name) = name
compileExpression (CallExpression func args) = func <> "(" <> intercalate ", " (map compileExpression args) <> ")"
compileExpression (NotEqExpression a b) = compileExpression a <> " <> " <> compileExpression b
compileExpression (EqExpression a b) = compileExpression a <> " = " <> compileExpression b
compileExpression (IsExpression a b) = compileExpression a <> " IS " <> compileExpression b
compileExpression (NotExpression a) = "NOT " <> compileExpression a
compileExpression (AndExpression a b) = compileExpression a <> " AND " <> compileExpression b
compileExpression (OrExpression a b) = "(" <> compileExpression a <> ") OR (" <> compileExpression b <> ")"
compileExpression (LessThanExpression a b) = compileExpression a <> " < " <> compileExpression b
compileExpression (LessThanOrEqualToExpression a b) = compileExpression a <> " <= " <> compileExpression b
compileExpression (GreaterThanExpression a b) = compileExpression a <> " > " <> compileExpression b
compileExpression (GreaterThanOrEqualToExpression a b) = compileExpression a <> " >= " <> compileExpression b
compileExpression (DoubleExpression double) = tshow double
compileExpression (TypeCastExpression value type_) = compileExpression value <> "::" <> compilePostgresType type_

compareStatement (CreateEnumType {}) _ = LT
compareStatement (StatementCreateTable CreateTable {}) (AddConstraint {}) = LT
compareStatement (a@AddConstraint {}) (b@AddConstraint {}) = compare (get #constraintName a) (get #constraintName b)
compareStatement (AddConstraint {}) _ = GT
compareStatement _ _ = EQ

compilePostgresType :: PostgresType -> Text
compilePostgresType PUUID = "UUID"
compilePostgresType PText = "TEXT"
compilePostgresType PInt = "INT"
compilePostgresType PSmallInt = "SMALLINT"
compilePostgresType PBigInt = "BIGINT"
compilePostgresType PBoolean = "BOOLEAN"
compilePostgresType PTimestamp = "TIMESTAMP WITHOUT TIME ZONE"
compilePostgresType PTimestampWithTimezone = "TIMESTAMP WITH TIME ZONE"
compilePostgresType PReal = "REAL"
compilePostgresType PDouble = "DOUBLE PRECISION"
compilePostgresType PPoint = "POINT"
compilePostgresType PDate = "DATE"
compilePostgresType PBinary = "BYTEA"
compilePostgresType PTime = "TIME"
compilePostgresType (PNumeric (Just precision) (Just scale)) = "NUMERIC(" <> show precision <> "," <> show scale <> ")"
compilePostgresType (PNumeric (Just precision) Nothing) = "NUMERIC(" <> show precision <> ")"
compilePostgresType (PNumeric Nothing _) = "NUMERIC"
compilePostgresType (PVaryingN limit) = "CHARACTER VARYING(" <> show limit <> ")"
compilePostgresType (PCharacterN length) = "CHARACTER(" <> show length <> ")"
compilePostgresType PSerial = "SERIAL"
compilePostgresType PBigserial = "BIGSERIAL"
compilePostgresType PJSONB = "JSONB"
compilePostgresType PInet = "INET"
compilePostgresType PTSVector = "TSVECTOR"
compilePostgresType (PArray type_) = compilePostgresType type_ <> "[]"
compilePostgresType (PCustomType theType) = theType

compileIdentifier :: Text -> Text
compileIdentifier identifier = if identifierNeedsQuoting then tshow identifier else identifier
    where
        identifierNeedsQuoting = isKeyword || containsSpace
        isKeyword = IHP.Prelude.toUpper identifier `elem` keywords
        containsSpace = Text.any (' ' ==) identifier

        keywords = [ "ABORT"
            , "ABSOLUTE"
            , "ACCESS"
            , "ACTION"
            , "ADD"
            , "ADMIN"
            , "AFTER"
            , "AGGREGATE"
            , "ALSO"
            , "ALTER"
            , "ASSERTION"
            , "ASSIGNMENT"
            , "AT"
            , "BACKWARD"
            , "BEFORE"
            , "BEGIN"
            , "BY"
            , "CACHE"
            , "CALLED"
            , "CASCADE"
            , "CHAIN"
            , "CHARACTERISTICS"
            , "CHECKPOINT"
            , "CLASS"
            , "CLOSE"
            , "CLUSTER"
            , "COMMENT"
            , "COMMIT"
            , "COMMITTED"
            , "CONNECTION"
            , "CONSTRAINTS"
            , "CONVERSION"
            , "COPY"
            , "CREATEDB"
            , "CREATEROLE"
            , "CREATEUSER"
            , "CSV"
            , "CURSOR"
            , "CYCLE"
            , "DATABASE"
            , "DAY"
            , "DEALLOCATE"
            , "DECLARE"
            , "DEFAULTS"
            , "DEFERRED"
            , "DEFINER"
            , "DELETE"
            , "DELIMITER"
            , "DELIMITERS"
            , "DISABLE"
            , "DOMAIN"
            , "DOUBLE"
            , "DROP"
            , "EACH"
            , "ENABLE"
            , "ENCODING"
            , "ENCRYPTED"
            , "ESCAPE"
            , "EXCLUDING"
            , "EXCLUSIVE"
            , "EXECUTE"
            , "EXPLAIN"
            , "EXTERNAL"
            , "FETCH"
            , "FIRST"
            , "FORCE"
            , "FORWARD"
            , "FUNCTION"
            , "GLOBAL"
            , "GRANTED"
            , "HANDLER"
            , "HEADER"
            , "HOLD"
            , "HOUR"
            , "IMMEDIATE"
            , "IMMUTABLE"
            , "IMPLICIT"
            , "INCLUDING"
            , "INCREMENT"
            , "INDEX"
            , "INHERIT"
            , "INHERITS"
            , "INPUT"
            , "INSENSITIVE"
            , "INSERT"
            , "INSTEAD"
            , "INVOKER"
            , "ISOLATION"
            , "KEY"
            , "LANCOMPILER"
            , "LANGUAGE"
            , "LARGE"
            , "LAST"
            , "LEVEL"
            , "LISTEN"
            , "LOAD"
            , "LOCAL"
            , "LOCATION"
            , "LOCK"
            , "LOGIN"
            , "MATCH"
            , "MAXVALUE"
            , "MINUTE"
            , "MINVALUE"
            , "MODE"
            , "MONTH"
            , "MOVE"
            , "NAMES"
            , "NEXT"
            , "NO"
            , "NOCREATEDB"
            , "NOCREATEROLE"
            , "NOCREATEUSER"
            , "NOINHERIT"
            , "NOLOGIN"
            , "NOSUPERUSER"
            , "NOTHING"
            , "NOTIFY"
            , "NOWAIT"
            , "OBJECT"
            , "OF"
            , "OIDS"
            , "OPERATOR"
            , "OPTION"
            , "OWNER"
            , "PARTIAL"
            , "PASSWORD"
            , "PREPARE"
            , "PREPARED"
            , "PRESERVE"
            , "PRIOR"
            , "PRIVILEGES"
            , "PROCEDURAL"
            , "PROCEDURE"
            , "QUOTE"
            , "READ"
            , "RECHECK"
            , "REINDEX"
            , "RELATIVE"
            , "RELEASE"
            , "RENAME"
            , "REPEATABLE"
            , "REPLACE"
            , "RESET"
            , "RESTART"
            , "RESTRICT"
            , "RETURNS"
            , "REVOKE"
            , "ROLE"
            , "ROLLBACK"
            , "ROWS"
            , "RULE"
            , "SAVEPOINT"
            , "SCHEMA"
            , "SCROLL"
            , "SECOND"
            , "SECURITY"
            , "SEQUENCE"
            , "SERIALIZABLE"
            , "SESSION"
            , "SET"
            , "SHARE"
            , "tshow"
            , "SIMPLE"
            , "STABLE"
            , "START"
            , "STATEMENT"
            , "STATISTICS"
            , "STDIN"
            , "STDOUT"
            , "STORAGE"
            , "STRICT"
            , "SUPERUSER"
            , "SYSID"
            , "SYSTEM"
            , "TABLESPACE"
            , "TEMP"
            , "TEMPLATE"
            , "TEMPORARY"
            , "TOAST"
            , "TRANSACTION"
            , "TRIGGER"
            , "TRUNCATE"
            , "TRUSTED"
            , "TYPE"
            , "UNCOMMITTED"
            , "UNENCRYPTED"
            , "UNKNOWN"
            , "UNLISTEN"
            , "UNTIL"
            , "UPDATE"
            , "VACUUM"
            , "VALID"
            , "VALIDATOR"
            , "VALUES"
            , "VARYING"
            , "VIEW"
            , "VOLATILE"
            , "WITH"
            , "WITHOUT"
            , "WORK"
            , "WRITE"
            , "YEAR"
            , "ZONE"
            , "BIGINT"
            , "BIT"
            , "BOOLEAN"
            , "CHAR"
            , "CHARACTER"
            , "COALESCE"
            , "CONVERT"
            , "DEC"
            , "DECIMAL"
            , "EXISTS"
            , "EXTRACT"
            , "FLOAT"
            , "GREATEST"
            , "INOUT"
            , "INT"
            , "INTEGER"
            , "INTERVAL"
            , "LEAST"
            , "NATIONAL"
            , "NCHAR"
            , "NONE"
            , "NULLIF"
            , "NUMERIC"
            , "OUT"
            , "OVERLAY"
            , "POSITION"
            , "PRECISION"
            , "REAL"
            , "ROW"
            , "SETOF"
            , "SMALLINT"
            , "SUBSTRING"
            , "TIME"
            , "TIMESTAMP"
            , "TREAT"
            , "TRIM"
            , "VARCHAR"
            ]

indent text = "    " <> text