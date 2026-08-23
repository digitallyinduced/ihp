{-|
Module: IHP.Postgres.Compiler
Description: Compiles AST of SQL to DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Compiler (compileSql, compileIdentifier, compileExpression, compilePostgresType, compileIndexColumn, compareStatement) where

import Prelude hiding (unlines, unwords)
import IHP.Postgres.Types
import Data.Maybe (fromJust, isJust, catMaybes, fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function ((&))

-- | Text versions of list functions
intercalate :: Text -> [Text] -> Text
intercalate = Text.intercalate

unlines :: [Text] -> Text
unlines = Text.unlines

unwords :: [Text] -> Text
unwords = Text.unwords

-- | Convert a Show-able value to Text
tshow :: Show a => a -> Text
tshow = Text.pack . show

compileSql :: [Statement] -> Text
compileSql statements = statements
    & map compileStatement
    & unlines

compileStatement :: Statement -> Text
compileStatement (StatementCreateTable CreateTable { name, columns, primaryKeyConstraint, constraints, unlogged, inherits }) = "CREATE" <> (if unlogged then " UNLOGGED" else "") <> " TABLE " <> compileQualifiedIdentifier name <> " (\n" <> intercalate ",\n" (map (\col -> "    " <> compileColumn primaryKeyConstraint col) columns <> maybe [] ((:[]) . indent) (compilePrimaryKeyConstraint primaryKeyConstraint) <> map (indent . compileConstraint) constraints) <> "\n)" <> maybe "" (\parent -> " INHERITS (" <> compileQualifiedIdentifier parent <> ")") inherits <> ";"
compileStatement CreateEnumType { name, values } = "CREATE TYPE " <> compileQualifiedIdentifier name <> " AS ENUM (" <> intercalate ", " (values & map TextExpression & map compileExpression) <> ");"
compileStatement CreateExtension { name, ifNotExists } = "CREATE EXTENSION " <> (if ifNotExists then "IF NOT EXISTS " else "") <> compileIdentifier name <> ";"
compileStatement AddConstraint { tableName, constraint = UniqueConstraint { name = Nothing, columnNames } } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ADD UNIQUE (" <> intercalate ", " columnNames <> ")" <> ";"
compileStatement AddConstraint { tableName, constraint, deferrable, deferrableType } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ADD CONSTRAINT " <> compileIdentifier (fromMaybe (error "compileStatement: Expected constraint name") (constraint.name)) <> " " <> compileConstraint constraint <> compileDeferrable deferrable deferrableType <> ";"
compileStatement AddColumn { tableName, column } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ADD COLUMN " <> (compileColumn (PrimaryKeyConstraint []) column) <> ";"
compileStatement DropColumn { tableName, columnName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " DROP COLUMN " <> compileIdentifier columnName <> ";"
compileStatement RenameColumn { tableName, from, to } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " RENAME COLUMN " <> compileIdentifier from <> " TO " <> compileIdentifier to <> ";"
compileStatement DropTable { tableName } = "DROP TABLE " <> compileQualifiedIdentifier tableName <> ";"
compileStatement Comment { content } = "--" <> content
compileStatement CreateIndex { indexName, unique, tableName, columns, whereClause, indexType, nullsDistinct } = "CREATE" <> (if unique then " UNIQUE " else " ") <> "INDEX " <> compileIdentifier indexName <> " ON " <> compileQualifiedIdentifier tableName <> (maybe "" (\indexType -> " USING " <> compileIndexType indexType) indexType) <> " (" <> (intercalate ", " (map compileIndexColumn columns)) <> ")" <> (if nullsDistinct then "" else " NULLS NOT DISTINCT") <> (case whereClause of Just expression -> " WHERE " <> compileExpression expression; Nothing -> "") <> ";"
compileStatement CreateFunction { functionName, functionArguments, functionBody, orReplace, returns, language, securityDefiner, functionAttributes, functionSettings } = "CREATE " <> (if orReplace then "OR REPLACE " else "") <> "FUNCTION " <> functionName <> "(" <> (functionArguments & map (\(argName, argType) -> argName <> " " <> compilePostgresType argType) & intercalate  ", ") <> ")" <> " RETURNS " <> compilePostgresType returns <> (if securityDefiner then " SECURITY DEFINER" else "") <> mconcat (map (" " <>) functionAttributes) <> mconcat (map compileFunctionSetting functionSettings) <> " AS $$" <> functionBody <> "$$ language " <> language <> ";"
compileStatement EnableRowLevelSecurity { tableName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ENABLE ROW LEVEL SECURITY;"
compileStatement CreatePolicy { name, action, tableName, using, check } = "CREATE POLICY " <> compileIdentifier name <> " ON " <> compileQualifiedIdentifier tableName <> maybe "" (\action -> " FOR " <> compilePolicyAction action) action  <> maybe "" (\expr -> " USING (" <> compileExpression expr <> ")") using <> maybe "" (\expr -> " WITH CHECK (" <> compileExpression expr <> ")") check <> ";"
compileStatement CreateSequence { name } = "CREATE SEQUENCE " <> compileQualifiedIdentifier name <> ";"
compileStatement DropConstraint { tableName, constraintName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " DROP CONSTRAINT " <> compileIdentifier constraintName <> ";"
compileStatement DropEnumType { name } = "DROP TYPE " <> compileQualifiedIdentifier name <> ";"
compileStatement DropIndex { indexName } = "DROP INDEX " <> compileQualifiedIdentifier indexName <> ";"
compileStatement DropNotNull { tableName, columnName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " DROP NOT NULL;"
compileStatement SetNotNull { tableName, columnName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " SET NOT NULL;"
compileStatement RenameTable { from, to } = "ALTER TABLE " <> compileQualifiedIdentifier from <> " RENAME TO " <> compileIdentifier to <> ";"
compileStatement DropPolicy { tableName, policyName } =  "DROP POLICY " <> compileIdentifier policyName <> " ON " <> compileQualifiedIdentifier tableName <> ";"
compileStatement SetDefaultValue { tableName, columnName, value } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " SET DEFAULT " <> compileExpression value <> ";"
compileStatement DropDefaultValue { tableName, columnName } = "ALTER TABLE " <> compileQualifiedIdentifier tableName <> " ALTER COLUMN " <> compileIdentifier columnName <> " DROP DEFAULT;"
compileStatement AddValueToEnumType { enumName, newValue } = "ALTER TYPE " <> compileQualifiedIdentifier enumName <> " ADD VALUE " <> compileExpression (TextExpression newValue) <> ";"
compileStatement CreateTrigger { name, eventWhen, event, tableName, for, whenCondition, functionName, arguments } = "CREATE TRIGGER " <> compileIdentifier name <> " " <> compileTriggerEventWhen eventWhen <> " " <> intercalate " OR " (map compileTriggerEvent event) <> " ON " <> compileQualifiedIdentifier tableName <> " " <> compileTriggerFor for <> " EXECUTE FUNCTION " <> compileExpression (CallExpression functionName arguments) <> ";"
compileStatement Begin = "BEGIN;"
compileStatement Commit = "COMMIT;"
compileStatement DropFunction { functionName } = "DROP FUNCTION " <> compileQualifiedIdentifier functionName <> ";"
compileStatement UnknownStatement { raw } = raw <> ";"
compileStatement Set { name, value } = "SET " <> compileIdentifier name <> " = " <> compileExpression value <> ";"
compileStatement SelectStatement { query } = "SELECT " <> query <> ";"
compileStatement DropTrigger { name, tableName } = "DROP TRIGGER " <> compileIdentifier name <> " ON " <> compileQualifiedIdentifier tableName <> ";"
compileStatement CreateEventTrigger { name, eventOn, whenCondition, functionName, arguments } = "CREATE EVENT TRIGGER " <> compileIdentifier name <> " ON " <> compileIdentifier eventOn <> " " <> (maybe "" (\expression -> "WHEN " <> compileExpression expression) whenCondition) <> " EXECUTE FUNCTION " <> compileExpression (CallExpression functionName arguments) <> ";"
compileStatement DropEventTrigger { name } = "DROP EVENT TRIGGER " <> compileIdentifier name <> ";"

-- | Emit a PRIMARY KEY constraint when there are multiple primary key columns
compilePrimaryKeyConstraint :: PrimaryKeyConstraint -> Maybe Text
compilePrimaryKeyConstraint PrimaryKeyConstraint { primaryKeyColumnNames } =
    case primaryKeyColumnNames of
        [] -> Nothing
        [_] -> Nothing
        names -> Just $ "PRIMARY KEY(" <> intercalate ", " names <> ")"

compileConstraint :: Constraint -> Text
compileConstraint ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete } = "FOREIGN KEY (" <> compileIdentifier columnName <> ") REFERENCES " <> compileQualifiedIdentifier referenceTable <> (if isJust referenceColumn then " (" <> fromJust referenceColumn <> ")" else "") <> " " <> compileOnDelete onDelete
compileConstraint UniqueConstraint { columnNames } = "UNIQUE(" <> intercalate ", " columnNames <> ")"
compileConstraint CheckConstraint { checkExpression } = "CHECK (" <> compileExpression checkExpression <> ")"
compileConstraint AlterTableAddPrimaryKey { primaryKeyConstraint } = fromMaybe "" (compilePrimaryKeyConstraint primaryKeyConstraint)
compileConstraint ExcludeConstraint { excludeElements, predicate, indexType } = "EXCLUDE" <> compiledIndexType <> " (" <> compiledExcludeElements <> ")" <> case predicate of
    Just expression -> " WHERE (" <> compileExpression expression <> ")"
    Nothing -> ""
    where
        compiledExcludeElements = intercalate ", " $ map compileExcludeElement excludeElements

        compileExcludeElement ExcludeConstraintElement { element, operator } = element <> " WITH " <> operator

        compiledIndexType = case indexType of
            Nothing -> ""
            Just indexType -> " USING " <> compileIndexType indexType

compileDeferrable :: Maybe Bool -> Maybe DeferrableType -> Text
compileDeferrable deferrable deferrableType = Text.concat $ map ((<>) " ") $ catMaybes [compileIsDeferrable <$> deferrable, compileDeferrableType <$> deferrableType]
    where
        compileIsDeferrable True = "DEFERRABLE"
        compileIsDeferrable False = "NOT DEFERRABLE"
        compileDeferrableType InitiallyImmediate = "INITIALLY IMMEDIATE"
        compileDeferrableType InitiallyDeferred = "INITIALLY DEFERRED"

compileOnDelete :: Maybe OnDelete -> Text
compileOnDelete Nothing = ""
compileOnDelete (Just NoAction) = "ON DELETE NO ACTION"
compileOnDelete (Just Restrict) = "ON DELETE RESTRICT"
compileOnDelete (Just SetNull) = "ON DELETE SET NULL"
compileOnDelete (Just SetDefault) = "ON DELETE SET DEFAULT"
compileOnDelete (Just Cascade) = "ON DELETE CASCADE"

compileColumn :: PrimaryKeyConstraint -> Column -> Text
compileColumn primaryKeyConstraint Column { name, columnType, defaultValue, notNull, isUnique, generator } =
    unwords (catMaybes
        [ Just (compileIdentifier name)
        , Just (compilePostgresType columnType)
        , fmap compileDefaultValue defaultValue
        , fmap compileGenerator generator
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
compileExpression (TextExpression value) = "'" <> Text.replace "'" "''" value <> "'"
compileExpression (VarExpression name) =
        if nameContainsSpaces
            then compileIdentifier name
            else name
    where
        nameContainsSpaces = Text.any (== ' ') name
compileExpression (CallExpression func [InExpression needle haystack])
    | Text.toUpper func == "POSITION" = func <> "(" <> compileExpressionWithOptionalParenthese needle <> " IN " <> compileExpressionWithOptionalParenthese haystack <> ")"
compileExpression (CallExpression func args) = func <> "(" <> intercalate ", " (map compileExpressionWithOptionalParenthese args) <> ")"
compileExpression (NotEqExpression a b) = compileExpression a <> " <> " <> compileExpression b
compileExpression (EqExpression a b) = compileExpressionWithOptionalParenthese a <> " = " <> compileExpressionWithOptionalParenthese b
compileExpression (IsExpression a (NotExpression b)) = compileExpressionWithOptionalParenthese a <> " IS NOT " <> compileExpressionWithOptionalParenthese b -- 'IS (NOT NULL)' => 'IS NOT NULL'
compileExpression (IsExpression a b) = compileExpressionWithOptionalParenthese a <> " IS " <> compileExpressionWithOptionalParenthese b
compileExpression (InExpression a b) = compileExpressionWithOptionalParenthese a <> " IN " <> compileExpressionWithOptionalParenthese b
compileExpression (InArrayExpression values) = "(" <> intercalate ", " (map compileExpression values) <> ")"
compileExpression (ArrayLiteralExpression values) = "ARRAY[" <> intercalate ", " (map compileExpression values) <> "]"
compileExpression (VariadicExpression value) = "VARIADIC " <> compileExpressionWithOptionalParenthese value
compileExpression (NotExpression a) = "NOT " <> compileExpressionWithOptionalParenthese a
compileExpression (AndExpression a b) = compileExpressionWithOptionalParenthese a <> " AND " <> compileExpressionWithOptionalParenthese b
compileExpression (OrExpression a b) = compileExpressionWithOptionalParenthese a <> " OR " <> compileExpressionWithOptionalParenthese b
compileExpression (LessThanExpression a b) = compileExpressionWithOptionalParenthese a <> " < " <> compileExpressionWithOptionalParenthese b
compileExpression (LessThanOrEqualToExpression a b) = compileExpressionWithOptionalParenthese a <> " <= " <> compileExpressionWithOptionalParenthese b
compileExpression (GreaterThanExpression a b) = compileExpressionWithOptionalParenthese a <> " > " <> compileExpressionWithOptionalParenthese b
compileExpression (GreaterThanOrEqualToExpression a b) = compileExpressionWithOptionalParenthese a <> " >= " <> compileExpressionWithOptionalParenthese b
compileExpression (DoubleExpression double) = tshow double
compileExpression (NumericExpression value) = value
compileExpression (IntExpression integer) = tshow integer
compileExpression (TypeCastExpression value type_) = compileExpression value <> "::" <> compilePostgresType type_
compileExpression (SelectExpression Select { columns, from, whereClause }) = "SELECT " <> intercalate ", " (map compileExpression columns) <> " FROM " <> compileExpression from <> " WHERE " <> compileExpression whereClause
compileExpression (ExistsExpression a) = "EXISTS " <> compileExpressionWithOptionalParenthese a
compileExpression (DotExpression a b) = compileExpressionWithOptionalParenthese a <> "." <> compileIdentifier b
compileExpression (ConcatenationExpression a b) = compileExpressionWithOptionalParenthese a <> " || " <> compileExpressionWithOptionalParenthese b

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
compileExpressionWithOptionalParenthese expr@(NumericExpression {}) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(DotExpression (VarExpression {}) b) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(ConcatenationExpression a b ) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(InArrayExpression values) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(ArrayLiteralExpression _) = compileExpression expr
compileExpressionWithOptionalParenthese expr@(VariadicExpression _) = compileExpression expr
compileExpressionWithOptionalParenthese expression = "(" <> compileExpression expression <> ")"

-- | Compare statements for sorting in schema output
compareStatement :: Statement -> Statement -> Ordering
compareStatement (CreateEnumType {}) _ = LT
compareStatement (StatementCreateTable CreateTable {}) (AddConstraint {}) = LT
compareStatement (AddConstraint { constraint = a }) (AddConstraint { constraint = b }) = compare (a.name) (b.name)
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
compilePostgresType PPolygon = "POLYGON"
compilePostgresType PGeometry = "GEOMETRY"
compilePostgresType (PGeometryWithModifier modifier) = "GEOMETRY(" <> modifier <> ")"
compilePostgresType PDate = "DATE"
compilePostgresType PBinary = "BYTEA"
compilePostgresType PTime = "TIME"
compilePostgresType (PInterval Nothing) = "INTERVAL"
compilePostgresType (PInterval (Just fields)) = "INTERVAL" <> " " <> fields
compilePostgresType (PNumeric (Just precision) (Just scale)) = "NUMERIC(" <> tshow precision <> "," <> tshow scale <> ")"
compilePostgresType (PNumeric (Just precision) Nothing) = "NUMERIC(" <> tshow precision <> ")"
compilePostgresType (PNumeric Nothing _) = "NUMERIC"
compilePostgresType (PVaryingN (Just limit)) = "CHARACTER VARYING(" <> tshow limit <> ")"
compilePostgresType (PVaryingN Nothing) = "CHARACTER VARYING"
compilePostgresType (PCharacterN length) = "CHARACTER(" <> tshow length <> ")"
compilePostgresType PSingleChar = "\"char\""
compilePostgresType PSerial = "SERIAL"
compilePostgresType PBigserial = "BIGSERIAL"
compilePostgresType PJSONB = "JSONB"
compilePostgresType PInet = "INET"
compilePostgresType PTSVector = "TSVECTOR"
compilePostgresType (PArray type_) = compilePostgresType type_ <> "[]"
compilePostgresType (PSetOf type_) = "SETOF " <> compilePostgresType type_
compilePostgresType (PTable columns) = "TABLE (" <> intercalate ", " (map (\(name, type_) -> compileUnqualifiedIdentifier name <> " " <> compilePostgresType type_) columns) <> ")"
compilePostgresType PTrigger = "TRIGGER"
compilePostgresType PEventTrigger = "EVENT_TRIGGER"
compilePostgresType (PCustomType theType) = theType

compileQualifiedIdentifier :: Text -> Text
compileQualifiedIdentifier = Text.intercalate "." . map compileIdentifier . Text.splitOn "."

compileIdentifier :: Text -> Text
compileIdentifier identifier
    | identifierNeedsQuoting = tshow identifier
    | otherwise = identifier
    where
        identifierNeedsQuoting = isKeyword || containsChar ' ' || containsChar '-' || containsChar '.' || isUsingUppercase
        isKeyword = Text.toUpper identifier `elem` keywords
        containsChar char = Text.any (char ==) identifier
        isUsingUppercase = Text.toLower identifier /= identifier

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
            , "ALL"
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
            , "SHOW"
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

compileUnqualifiedIdentifier :: Text -> Text
compileUnqualifiedIdentifier identifier
    | isValidUnquotedIdentifier && compileIdentifier identifier == identifier = identifier
    | otherwise = "\"" <> Text.replace "\"" "\"\"" identifier <> "\""
    where
        isValidUnquotedIdentifier = case Text.uncons identifier of
            Nothing -> False
            Just (firstCharacter, remainingCharacters) ->
                isIdentifierStart firstCharacter && Text.all isIdentifierContinuation remainingCharacters
        isIdentifierStart character = character == '_' || isAsciiLower character || character >= '\x80'
        isIdentifierContinuation character = isIdentifierStart character || isAsciiDigit character || character == '$'
        isAsciiLower character = character >= 'a' && character <= 'z'
        isAsciiDigit character = character >= '0' && character <= '9'

indent text = "    " <> text

compileTriggerEventWhen :: TriggerEventWhen -> Text
compileTriggerEventWhen Before = "BEFORE"
compileTriggerEventWhen After = "AFTER"
compileTriggerEventWhen InsteadOf = "INSTEAD OF"

compileTriggerEvent :: TriggerEvent -> Text
compileTriggerEvent TriggerOnInsert = "INSERT"
compileTriggerEvent TriggerOnUpdate = "UPDATE"
compileTriggerEvent TriggerOnDelete = "DELETE"
compileTriggerEvent TriggerOnTruncate = "TRUNCATE"

compileTriggerFor :: TriggerFor -> Text
compileTriggerFor ForEachRow = "FOR EACH ROW"
compileTriggerFor ForEachStatement = "FOR EACH STATEMENT"

compilePolicyAction :: PolicyAction -> Text
compilePolicyAction PolicyForAll = "ALL"
compilePolicyAction PolicyForSelect = "SELECT"
compilePolicyAction PolicyForInsert = "INSERT"
compilePolicyAction PolicyForUpdate = "UPDATE"
compilePolicyAction PolicyForDelete = "DELETE"

compileGenerator :: ColumnGenerator -> Text
compileGenerator ColumnGenerator { generate, stored } =
    "GENERATED ALWAYS AS ("
    <> compileExpressionWithOptionalParenthese generate
    <> ")"
    <> (if stored then " STORED" else "")

compileIndexType :: IndexType -> Text
compileIndexType Btree = "BTREE"
compileIndexType Hash = "HASH"
compileIndexType Gist = "GIST"
compileIndexType Spgist = "SPGIST"
compileIndexType Gin = "GIN"
compileIndexType Brin = "BRIN"
compileIndexType Hnsw = "HNSW"
compileIndexType Ivfflat = "IVFFLAT"

compileFunctionSetting :: FunctionSetting -> Text
compileFunctionSetting FunctionSetting { settingName, settingValue } = " SET " <> settingName <> " = " <> settingValue

compileIndexColumn :: IndexColumn -> Text
compileIndexColumn IndexColumn { column, columnOperatorClass, columnOrder } =
    unwords ([compileExpression column] <> maybeToList (compileIdentifier <$> columnOperatorClass) <> (columnOrder & map compileIndexColumnOrder))

compileIndexColumnOrder :: IndexColumnOrder -> Text
compileIndexColumnOrder Asc = "ASC"
compileIndexColumnOrder Desc = "DESC"
compileIndexColumnOrder NullsFirst = "NULLS FIRST"
compileIndexColumnOrder NullsLast = "NULLS LAST"
