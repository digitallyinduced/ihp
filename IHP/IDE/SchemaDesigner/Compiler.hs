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
compileStatement (StatementCreateTable CreateTable { name, columns, primaryKeyConstraint, constraints }) = "CREATE TABLE " <> compileIdentifier name <> " (\n" <> intercalate ",\n" (map (\col -> "    " <> compileColumn primaryKeyConstraint col) columns <> maybe [] ((:[]) . indent) (compilePrimaryKeyConstraint primaryKeyConstraint) <> map (indent . compileConstraint) constraints) <> "\n);"
compileStatement CreateEnumType { name, values } = "CREATE TYPE " <> compileIdentifier name <> " AS ENUM (" <> intercalate ", " (values |> map TextExpression |> map compileExpression) <> ");"
compileStatement CreateExtension { name, ifNotExists } = "CREATE EXTENSION " <> (if ifNotExists then "IF NOT EXISTS " else "") <> compileIdentifier name <> ";"
compileStatement AddConstraint { tableName, constraintName = "", constraint = UniqueConstraint { columnNames } } = "ALTER TABLE " <> compileIdentifier tableName <> " ADD UNIQUE (" <> intercalate ", " columnNames <> ")" <> ";"
compileStatement AddConstraint { tableName, constraintName, constraint } = "ALTER TABLE " <> compileIdentifier tableName <> " ADD CONSTRAINT " <> compileIdentifier constraintName <> " " <> compileConstraint constraint <> ";"
compileStatement AddColumn { tableName, column } = "ALTER TABLE " <> compileIdentifier tableName <> " ADD COLUMN " <> (compileColumn (PrimaryKeyConstraint []) column) <> ";"
compileStatement DropColumn { tableName, columnName } = "ALTER TABLE " <> compileIdentifier tableName <> " DROP COLUMN " <> compileIdentifier columnName <> ";"
compileStatement RenameColumn { tableName, from, to } = "ALTER TABLE " <> compileIdentifier tableName <> " RENAME COLUMN " <> compileIdentifier from <> " TO " <> compileIdentifier to <> ";"
compileStatement DropTable { tableName } = "DROP TABLE " <> compileIdentifier tableName <> ";"
compileStatement Comment { content } = "--" <> content
compileStatement CreateIndex { indexName, unique, tableName, expressions, whereClause } = "CREATE" <> (if unique then " UNIQUE " else " ") <> "INDEX " <> indexName <> " ON " <> tableName <> " (" <> (intercalate ", " (map compileExpression expressions)) <> ")" <> (case whereClause of Just expression -> " WHERE " <> compileExpression expression; Nothing -> "") <> ";"
compileStatement CreateFunction { functionName, functionBody, orReplace, returns, language } = "CREATE " <> (if orReplace then "OR REPLACE " else "") <> "FUNCTION " <> functionName <> "() RETURNS " <> compilePostgresType returns <> " AS $$" <> functionBody <> "$$ language " <> language <> ";"
compileStatement EnableRowLevelSecurity { tableName } = "ALTER TABLE " <> tableName <> " ENABLE ROW LEVEL SECURITY;"
compileStatement CreatePolicy { name, tableName, using, check } = "CREATE POLICY " <> compileIdentifier name <> " ON " <> compileIdentifier tableName <> maybe "" (\expr -> " USING (" <> compileExpression expr <> ")") using <> maybe "" (\expr -> " WITH CHECK (" <> compileExpression expr <> ")") check <> ";"
compileStatement CreateSequence { name } = "CREATE SEQUENCE " <> compileIdentifier name <> ";"
compileStatement DropConstraint { tableName, constraintName } = "ALTER TABLE " <> compileIdentifier tableName <> " DROP CONSTRAINT " <> compileIdentifier constraintName <> ";"
compileStatement DropEnumType { name } = "DROP TYPE " <> compileIdentifier name <> ";"
compileStatement DropIndex { indexName } = "DROP INDEX " <> compileIdentifier indexName <> ";"
compileStatement DropNotNull { tableName, columnName } = "ALTER TABLE " <> compileIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " DROP NOT NULL;"
compileStatement SetNotNull { tableName, columnName } = "ALTER TABLE " <> compileIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " SET NOT NULL;"
compileStatement RenameTable { from, to } = "ALTER TABLE " <> compileIdentifier from <> " RENAME TO " <> compileIdentifier to <> ";"
compileStatement DropPolicy { tableName, policyName } =  "DROP POLICY " <> compileIdentifier policyName <> " ON " <> compileIdentifier tableName <> ";"
compileStatement SetDefaultValue { tableName, columnName, value } = "ALTER TABLE " <> compileIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " SET DEFAULT " <> compileExpression value <> ";"
compileStatement DropDefaultValue { tableName, columnName } = "ALTER TABLE " <> compileIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " DROP DEFAULT;"
compileStatement AddValueToEnumType { enumName, newValue } = "ALTER TYPE " <> compileIdentifier enumName <> " ADD VALUE " <> compileExpression (TextExpression newValue) <> ";"
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
    unwords (catMaybes
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
compileExpression (CallExpression func args) = func <> "(" <> intercalate ", " (map compileExpressionWithOptionalParenthese args) <> ")"
compileExpression (NotEqExpression a b) = compileExpression a <> " <> " <> compileExpression b
compileExpression (EqExpression a b) = compileExpressionWithOptionalParenthese a <> " = " <> compileExpressionWithOptionalParenthese b
compileExpression (IsExpression a b) = compileExpressionWithOptionalParenthese a <> " IS " <> compileExpressionWithOptionalParenthese b
compileExpression (NotExpression a) = "NOT " <> compileExpressionWithOptionalParenthese a
compileExpression (AndExpression a b) = compileExpressionWithOptionalParenthese a <> " AND " <> compileExpressionWithOptionalParenthese b
compileExpression (OrExpression a b) = compileExpressionWithOptionalParenthese a <> " OR " <> compileExpressionWithOptionalParenthese b
compileExpression (LessThanExpression a b) = compileExpressionWithOptionalParenthese a <> " < " <> compileExpressionWithOptionalParenthese b
compileExpression (LessThanOrEqualToExpression a b) = compileExpressionWithOptionalParenthese a <> " <= " <> compileExpressionWithOptionalParenthese b
compileExpression (GreaterThanExpression a b) = compileExpressionWithOptionalParenthese a <> " > " <> compileExpressionWithOptionalParenthese b
compileExpression (GreaterThanOrEqualToExpression a b) = compileExpressionWithOptionalParenthese a <> " >= " <> compileExpressionWithOptionalParenthese b
compileExpression (DoubleExpression double) = tshow double
compileExpression (IntExpression integer) = tshow integer
compileExpression (TypeCastExpression value type_) = compileExpression value <> "::" <> compilePostgresType type_
compileExpression (SelectExpression Select { columns, from, whereClause }) = "SELECT " <> intercalate ", " (map compileExpression columns) <> " FROM " <> compileExpression from <> " WHERE " <> compileExpression whereClause
compileExpression (ExistsExpression a) = "EXISTS " <> compileExpressionWithOptionalParenthese a
compileExpression (DotExpression a b) = compileExpressionWithOptionalParenthese a <> "." <> compileIdentifier b

compileExpressionWithOptionalParenthese :: Expression -> Text
compileExpressionWithOptionalParenthese expr@(VarExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(IsExpression a (NotExpression b)) = compileExpression a <> " IS " <> compileExpression (NotExpression b) -- 'IS (NOT NULL)' => 'IS NOT NULL'
compileExpressionWithOptionalParenthese expr@(IsExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(EqExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(AndExpression a@(AndExpression {}) b ) = "(" <> compileExpression a <> " AND " <> compileExpressionWithOptionalParenthese b <> ")" -- '(a AND b) AND c' => 'a AND b AND C'
compileExpressionWithOptionalParenthese expr@(AndExpression a b@(AndExpression {}) ) = "(" <> compileExpressionWithOptionalParenthese a <> " AND " <> compileExpression b <> ")" -- 'a AND (b AND c)' => 'a AND b AND C'
--compileExpressionWithOptionalParenthese expr@(OrExpression a@(IsExpression {}) b ) = compileExpressionWithOptionalParenthese a <> " OR " <> compileExpressionWithOptionalParenthese b -- '(a IS NULL) OR b' => 'A IS NULL OR b'
compileExpressionWithOptionalParenthese expr@(CallExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(TextExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(IntExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(DoubleExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(DotExpression (VarExpression {}) b) = compileExpression expr
compileExpressionWithOptionalParenthese expression = "(" <> compileExpression expression <> ")"

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
compilePostgresType (PVaryingN (Just limit)) = "CHARACTER VARYING(" <> show limit <> ")"
compilePostgresType (PVaryingN Nothing) = "CHARACTER VARYING"
compilePostgresType (PCharacterN length) = "CHARACTER(" <> show length <> ")"
compilePostgresType PSerial = "SERIAL"
compilePostgresType PBigserial = "BIGSERIAL"
compilePostgresType PJSONB = "JSONB"
compilePostgresType PInet = "INET"
compilePostgresType PTSVector = "TSVECTOR"
compilePostgresType (PArray type_) = compilePostgresType type_ <> "[]"
compilePostgresType PTrigger = "TRIGGER"
compilePostgresType (PCustomType theType) = theType

compileIdentifier :: Text -> Text
compileIdentifier identifier = if identifierNeedsQuoting then tshow identifier else identifier
    where
        identifierNeedsQuoting = isKeyword || containsChar ' ' || containsChar '-'
        isKeyword = IHP.Prelude.toUpper identifier `elem` keywords
        containsChar char = Text.any (char ==) identifier

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