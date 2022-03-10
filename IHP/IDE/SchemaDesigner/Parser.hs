{-|
Module: IHP.IDE.SchemaDesigner.Types
Description: Parser for Application/Schema.sql
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Parser
( parseSchemaSql
, parseSqlFile
, schemaFilePath
, parseDDL
, expression
, sqlType
, removeTypeCasts
) where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified Prelude
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Char
import Control.Monad.Combinators.Expr

schemaFilePath = "Application/Schema.sql"

parseSchemaSql :: IO (Either ByteString [Statement])
parseSchemaSql = parseSqlFile schemaFilePath

parseSqlFile :: FilePath -> IO (Either ByteString [Statement])
parseSqlFile schemaFilePath = do
    schemaSql <- Text.readFile schemaFilePath
    let result = runParser parseDDL (cs schemaFilePath) schemaSql
    case result of
        Left error -> pure (Left (cs $ errorBundlePretty error))
        Right r -> pure (Right r)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space
    space1
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '\'' *> manyTill Lexer.charLiteral (char '\'')

parseDDL :: Parser [Statement]
parseDDL = optional space >> manyTill statement eof

statement = do
    space
    let create = try createExtension <|> try (StatementCreateTable <$> createTable) <|> try createIndex <|> try createFunction <|> try createTrigger <|> try createEnumType <|> try createPolicy <|> try createSequence
    let alter = do
            lexeme "ALTER"
            alterTable <|> alterType <|> alterSequence
    s <- setStatement <|> create <|> alter <|> selectStatement <|> try dropTable <|> try dropIndex <|> try dropPolicy <|> dropType <|> commentStatement <|> comment <|> begin <|> commit
    space
    pure s


createExtension = do
    lexeme "CREATE"
    lexeme "EXTENSION"
    ifNotExists <- isJust <$> optional (lexeme "IF" >> lexeme "NOT" >> lexeme "EXISTS")
    name <- qualifiedIdentifier
    optional do
        space
        lexeme "WITH"
        lexeme "SCHEMA"
        lexeme "public"
    char ';'
    pure CreateExtension { name, ifNotExists = True }

createTable = do
    lexeme "CREATE"
    lexeme "TABLE"
    name <- qualifiedIdentifier

    -- Process columns (tagged if they're primary key) and table constraints
    -- together, as they can be in any order
    (taggedColumns, allConstraints) <- between (char '(' >> space) (char ')' >> space) do
        columnsAndConstraints <- ((Right <$> parseTableConstraint) <|> (Left <$> parseColumn)) `sepBy` (char ',' >> space)
        pure (lefts columnsAndConstraints, rights columnsAndConstraints)

    char ';'

    -- Check that either there is a single column with a PRIMARY KEY constraint,
    -- or there is a single PRIMARY KEY table constraint
    let
        columns = map snd taggedColumns
        constraints = rights allConstraints

    primaryKeyConstraint <- case filter fst taggedColumns of
        [] -> case lefts allConstraints of
            [] -> pure $ PrimaryKeyConstraint []
            [primaryKeyConstraint] -> pure primaryKeyConstraint
            _ -> Prelude.fail ("Multiple PRIMARY KEY constraints on table " <> cs name)
        [(_, Column { name })] -> case lefts allConstraints of
            [] -> pure $ PrimaryKeyConstraint [name]
            _ -> Prelude.fail ("Primary key defined in both column and table constraints on table " <> cs name)
        _ -> Prelude.fail "Multiple columns with PRIMARY KEY constraint"

    pure CreateTable { name, columns, primaryKeyConstraint, constraints }

createEnumType = do
    lexeme "CREATE"
    lexeme "TYPE"
    optional do
        lexeme "public"
        char '.'
    name <- identifier
    lexeme "AS"
    lexeme "ENUM"
    values <- between (char '(' >> space) (space >> char ')' >> space) (textExpr' `sepBy` (char ',' >> space))
    space
    char ';'
    pure CreateEnumType { name, values }

addConstraint tableName = do
    constraint <- parseTableConstraint >>= \case
      Left primaryKeyConstraint -> pure AlterTableAddPrimaryKey { name = Nothing, primaryKeyConstraint }
      Right constraint -> pure constraint
    deferrable <- optional parseDeferrable
    deferrableType <- optional parseDeferrableType
    char ';'
    pure AddConstraint { tableName, constraint, deferrable, deferrableType }

parseDeferrable = do
    isDeferrable <- lexeme "DEFERRABLE" <|> lexeme "NOT DEFERRABLE"
    pure $ case isDeferrable of
        "DEFERRABLE" -> True
        "NOT DEFERRABLE" -> False

parseDeferrableType = do
    lexeme "INITIALLY"
    dtype <- lexeme "IMMEDIATE" <|> lexeme "DEFERRED"
    case dtype of
        "IMMEDIATE" -> pure InitiallyImmediate
        "DEFERRED" -> pure InitiallyDeferred

parseTableConstraint = do
    name <- optional do
        lexeme "CONSTRAINT"
        identifier
    (Left <$> parsePrimaryKeyConstraint) <|>
      (Right <$> (parseForeignKeyConstraint name <|> parseUniqueConstraint name <|> parseCheckConstraint name <|> parseExcludeConstraint name))

parsePrimaryKeyConstraint = do
    lexeme "PRIMARY"
    lexeme "KEY"
    primaryKeyColumnNames <- between (char '(' >> space) (char ')' >> space) (identifier `sepBy1` (char ',' >> space))
    pure PrimaryKeyConstraint { primaryKeyColumnNames }

parseForeignKeyConstraint name = do
    lexeme "FOREIGN"
    lexeme "KEY"
    columnName <- between (char '(' >> space) (char ')' >> space) identifier
    lexeme "REFERENCES"
    referenceTable <- qualifiedIdentifier
    referenceColumn <- optional $ between (char '(' >> space) (char ')' >> space) identifier
    onDelete <- optional do
        lexeme "ON"
        lexeme "DELETE"
        parseOnDelete
    pure ForeignKeyConstraint { name, columnName, referenceTable, referenceColumn, onDelete }

parseUniqueConstraint name = do
    lexeme "UNIQUE"
    columnNames <- between (char '(' >> space) (char ')' >> space) (identifier `sepBy1` (char ',' >> space))
    pure UniqueConstraint { name, columnNames }

parseCheckConstraint name = do
    lexeme "CHECK"
    checkExpression <- between (char '(' >> space) (char ')' >> space) expression
    pure CheckConstraint { name, checkExpression }

parseExcludeConstraint name = do
    lexeme "EXCLUDE"
    indexType <- optional parseIndexType
    excludeElements <- between (char '(' >> space) (char ')' >> space) $ excludeElement `sepBy` (char ',' >> space)
    predicate <- optional do
        lexeme "WHERE"
        between (char '(' >> space) (char ')' >> space) expression
    pure ExcludeConstraint { name, excludeElements, predicate, indexType }
    where
        excludeElement = do
            element <- identifier
            space
            lexeme "WITH"
            space
            operator <- parseCommutativeInfixOperator
            pure ExcludeConstraintElement { element, operator }

        parseCommutativeInfixOperator = choice $ map lexeme
            [ "="
            , "<>"
            , "!="
            , "AND"
            , "OR"
            ]

parseOnDelete = choice
        [ (lexeme "NO" >> lexeme "ACTION") >> pure NoAction
        , (lexeme "RESTRICT" >> pure Restrict)
        , (lexeme "SET" >> ((lexeme "NULL" >> pure SetNull) <|> (lexeme "DEFAULT" >> pure SetDefault)))
        , (lexeme "CASCADE" >> pure Cascade)
        ]

parseColumn :: Parser (Bool, Column)
parseColumn = do
    name <- identifier
    columnType <- sqlType
    space
    defaultValue <- optional do
        lexeme "DEFAULT"
        expression
    generator <- optional do
        lexeme "GENERATED"
        lexeme "ALWAYS"
        lexeme "AS"
        generate <- expression
        stored <- isJust <$> optional (lexeme "STORED")
        pure ColumnGenerator { generate, stored }
    primaryKey <- isJust <$> optional (lexeme "PRIMARY" >> lexeme "KEY")
    notNull <- isJust <$> optional (lexeme "NOT" >> lexeme "NULL")
    isUnique <- isJust <$> optional (lexeme "UNIQUE")
    pure (primaryKey, Column { name, columnType, defaultValue, notNull, isUnique, generator })

sqlType :: Parser PostgresType
sqlType = choice $ map optionalArray
        [ uuid
        , text
        , bigint
        , smallint
        , int   -- order int after smallint/bigint because symbol INT is prefix og INT2, INT8
        , bool
        , timestamp
        , timestampZ
        , timestampZ'
        , timestamp'
        , real
        , double
        , point
        , polygon
        , date
        , binary
        , time
        , numericPS
        , numeric
        , character
        , varchar
        , serial
        , bigserial
        , jsonb
        , inet
        , tsvector
        , trigger
        , singleChar
        , customType
        ]
            where
                timestamp = do
                    try (symbol' "TIMESTAMP" >> symbol' "WITHOUT" >> symbol' "TIME" >> symbol' "ZONE")
                    pure PTimestamp

                timestampZ = do
                    try (symbol' "TIMESTAMP" >> symbol' "WITH" >> symbol' "TIME" >> symbol' "ZONE")
                    pure PTimestampWithTimezone

                timestampZ' = do
                    try (symbol' "TIMESTAMPZ")
                    pure PTimestampWithTimezone

                timestamp' = do
                    try (symbol' "TIMESTAMP")
                    pure PTimestamp

                uuid = do
                    try (symbol' "UUID")
                    pure PUUID

                text = do
                    try (symbol' "TEXT")
                    pure PText

                bigint = do
                    try (symbol' "BIGINT") <|> try (symbol' "INT8")
                    pure PBigInt

                smallint = do
                    try (symbol' "SMALLINT") <|> try (symbol' "INT2")
                    pure PSmallInt

                int = do
                    try (symbol' "INTEGER") <|> try (symbol' "INT4") <|> try (symbol' "INT")
                    pure PInt

                bool = do
                    try (symbol' "BOOLEAN") <|> try (symbol' "BOOL")
                    pure PBoolean

                real = do
                    try (symbol' "REAL") <|> try (symbol' "FLOAT4")
                    pure PReal

                double = do
                    try (symbol' "DOUBLE PRECISION") <|> try (symbol' "FLOAT8")
                    pure PDouble

                point = do
                    try (symbol' "POINT")
                    pure PPoint

                polygon = do
                    try (symbol' "POLYGON")
                    pure PPolygon

                date = do
                    try (symbol' "DATE")
                    pure PDate

                binary = do
                    try (symbol' "BYTEA")
                    pure PBinary

                time = do
                    try (symbol' "TIME")
                    optional do
                        symbol' "WITHOUT"
                        symbol' "TIME"
                        symbol' "ZONE"
                    pure PTime

                numericPS = do
                    try (symbol' "NUMERIC(")
                    values <- between (space) (char ')' >> space) (varExpr `sepBy` (char ',' >> space))
                    case values of
                        [VarExpression precision, VarExpression scale] -> do
                            let p = textToInt precision
                            let s = textToInt scale
                            when (or [isNothing p, isNothing s]) do
                                Prelude.fail "Failed to parse NUMERIC(..) expression"
                            pure (PNumeric p s)
                        [VarExpression precision] -> do
                            let p = textToInt precision
                            when (isNothing p) do
                                Prelude.fail "Failed to parse NUMERIC(..) expression"
                            pure (PNumeric p Nothing)
                        _ -> Prelude.fail "Failed to parse NUMERIC(..) expression"

                numeric = do
                    try (symbol' "NUMERIC")
                    pure (PNumeric Nothing Nothing)

                varchar = do
                    try (symbol' "CHARACTER VARYING") <|> try (symbol' "VARCHAR")
                    value <- optional $ between (char '(' >> space) (char ')' >> space) (varExpr)
                    case value of
                        Just (VarExpression limit) -> do
                            let l = textToInt limit
                            case l of
                                Nothing -> Prelude.fail "Failed to parse CHARACTER VARYING(..) expression"
                                Just l -> pure (PVaryingN (Just l))
                        Nothing -> pure (PVaryingN Nothing)
                        _ -> Prelude.fail "Failed to parse CHARACTER VARYING(..) expression"

                character = do
                    try (symbol' "CHAR(") <|> try (symbol' "CHARACTER(")
                    value <- between (space) (char ')' >> space) (varExpr)
                    case value of
                        VarExpression length -> do
                            let l = textToInt length
                            case l of
                                Nothing -> Prelude.fail "Failed to parse CHARACTER VARYING(..) expression"
                                Just l -> pure (PCharacterN l)
                        _ -> Prelude.fail "Failed to parse CHARACTER VARYING(..) expression"

                singleChar = do
                    try (symbol "\"char\"")
                    pure PSingleChar

                serial = do
                    try (symbol' "SERIAL")
                    pure PSerial

                bigserial = do
                    try (symbol' "BIGSERIAL")
                    pure PBigserial

                jsonb = do
                    try (symbol' "JSONB")
                    pure PJSONB

                inet = do
                    try (symbol' "INET")
                    pure PInet

                tsvector = do
                    try (symbol' "TSVECTOR")
                    pure PTSVector

                optionalArray typeParser= do
                    arrayType <- typeParser;
                    (try do symbol' "[]"; pure $ PArray arrayType) <|> pure arrayType

                trigger = do
                    try (symbol' "TRIGGER")
                    pure PTrigger

                customType = do
                    optional do
                        lexeme "public"
                        char '.'
                    theType <- try (takeWhile1P (Just "Custom type") (\c -> isAlphaNum c || c == '_'))
                    pure (PCustomType theType)

term = parens expression <|> try callExpr <|> try doubleExpr <|> try intExpr <|> selectExpr <|> varExpr <|> (textExpr <* optional space)
    where
        parens f = between (char '(' >> space) (char ')' >> space) f

table = [
            [ binary  "<>"  NotEqExpression
            , binary "="  EqExpression

            , binary "<=" LessThanOrEqualToExpression
            , binary "<"  LessThanExpression
            , binary ">="  GreaterThanOrEqualToExpression
            , binary ">"  GreaterThanExpression
            , binary "||" ConcatenationExpression

            , binary "IS" IsExpression
            , prefix "NOT" NotExpression
            , prefix "EXISTS" ExistsExpression
            , typeCast
            , dot
            ],
            [ binary "AND" AndExpression, binary "OR" OrExpression ]
        ]
    where
        binary  name f = InfixL  (f <$ try (symbol name))
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

        -- Cannot be implemented as a infix operator as that requires two expression operands,
        -- but the second is the type-cast type which is not an expression
        typeCast = Postfix do
            symbol "::"
            castType <- sqlType
            pure $ \expr -> TypeCastExpression expr castType

        dot = Postfix do
            char '.'
            name <- identifier
            pure $ \expr -> DotExpression expr name

-- | Parses a SQL expression
--
-- This parser makes use of makeExprParser as described in https://hackage.haskell.org/package/parser-combinators-1.2.0/docs/Control-Monad-Combinators-Expr.html
expression :: Parser Expression
expression = do
    e <- makeExprParser term table <?> "expression"
    space
    pure e

varExpr :: Parser Expression
varExpr = VarExpression <$> identifier

doubleExpr :: Parser Expression
doubleExpr = DoubleExpression <$> (Lexer.signed spaceConsumer Lexer.float)

intExpr :: Parser Expression
intExpr = IntExpression <$> (Lexer.signed spaceConsumer Lexer.decimal)

callExpr :: Parser Expression
callExpr = do
    func <- qualifiedIdentifier
    args <- between (char '(') (char ')') (expression `sepBy` (char ',' >> space))
    space
    pure (CallExpression func args)

textExpr :: Parser Expression
textExpr = TextExpression <$> textExpr'

textExpr' :: Parser Text
textExpr' = cs <$> do
    let emptyByteString = do
            string "'\\x'"
            pure ""
    (try (char '\'' *> manyTill Lexer.charLiteral (char '\''))) <|> emptyByteString

selectExpr :: Parser Expression
selectExpr = do
    symbol' "SELECT"
    columns <- expression `sepBy` (char ',' >> space)
    symbol' "FROM"
    from <- expression


    let whereClause alias = do
            symbol' "WHERE"
            whereClause <- expression
            pure (SelectExpression Select { .. })

    let explicitAs = do
            symbol' "AS"
            alias <- identifier
            whereClause (Just alias)

    let implicitAs = do
            alias <- identifier
            whereClause (Just alias)

    whereClause Nothing <|> explicitAs <|> implicitAs
    
    
    
    

identifier :: Parser Text
identifier = do
    i <- (between (char '"') (char '"') (takeWhile1P Nothing (\c -> c /= '"'))) <|> takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_')
    space
    pure i

comment = do
    (char '-' >> char '-') <?> "Line comment"
    content <- takeWhileP Nothing (/= '\n')
    pure Comment { content }

createIndex = do
    lexeme "CREATE"
    unique <- isJust <$> optional (lexeme "UNIQUE")
    lexeme "INDEX"
    indexName <- identifier
    lexeme "ON"
    tableName <- qualifiedIdentifier
    indexType <- optional parseIndexType
    expressions <- between (char '(' >> space) (char ')' >> space) (expression `sepBy1` (char ',' >> space))
    whereClause <- optional do
        lexeme "WHERE"
        expression
    char ';'
    pure CreateIndex { indexName, unique, tableName, expressions, whereClause, indexType }

parseIndexType = do
    lexeme "USING"

    choice $ map (\(s, v) -> do symbol' s; pure v)
        [ ("btree", Btree)
        , ("gin", Gin)
        , ("gist", Gist)
        ]

createFunction = do
    lexeme "CREATE"
    orReplace <- isJust <$> optional (lexeme "OR" >> lexeme "REPLACE")
    lexeme "FUNCTION"
    functionName <- qualifiedIdentifier
    lexeme "()"
    lexeme "RETURNS"
    returns <- sqlType

    language <- optional do
        lexeme "language" <|> lexeme "LANGUAGE"
        symbol' "plpgsql" <|> symbol' "SQL"

    lexeme "AS"
    space
    functionBody <- cs <$> between (char '$' >> char '$') (char '$' >> char '$') (many (anySingleBut '$'))
    space

    language <- case language of
        Just language -> pure language
        Nothing -> do
            lexeme "language" <|> lexeme "LANGUAGE"
            symbol "plpgsql" <|> symbol "SQL"
    char ';'
    pure CreateFunction { functionName, functionBody, orReplace, returns, language }

createTrigger = do
    lexeme "CREATE"
    lexeme "TRIGGER"

    name <- qualifiedIdentifier
    eventWhen <- (lexeme "AFTER" >> pure After) <|> (lexeme "BEFORE" >> pure Before) <|> (lexeme "INSTEAD OF" >> pure InsteadOf)
    event <- (lexeme "INSERT" >> pure TriggerOnInsert) <|> (lexeme "UPDATE" >> pure TriggerOnUpdate) <|> (lexeme "DELETE" >> pure TriggerOnDelete) <|> (lexeme "TRUNCATE" >> pure TriggerOnTruncate)

    lexeme "ON"
    tableName <- qualifiedIdentifier

    lexeme "FOR"
    optional (lexeme "EACH")

    for <- (lexeme "ROW" >> pure ForEachRow) <|> (lexeme "STATEMENT" >> pure ForEachStatement)

    whenCondition <- optional do
        lexeme "WHEN"
        expression

    lexeme "EXECUTE"
    optional (lexeme "FUNCTION" <|> lexeme "PROCEDURE")

    (CallExpression functionName arguments) <- callExpr

    char ';'

    pure CreateTrigger
        { name
        , eventWhen
        , event
        , tableName
        , for
        , whenCondition
        , functionName
        , arguments
        }

alterTable = do
    lexeme "TABLE"
    optional (lexeme "ONLY")
    tableName <- qualifiedIdentifier
    let add = do
            lexeme "ADD"
            let addUnique = do
                    unique <- parseUniqueConstraint Nothing
                    deferrable <- optional parseDeferrable
                    deferrableType <- optional parseDeferrableType
                    char ';'
                    pure (AddConstraint tableName unique deferrable deferrableType)
            addConstraint tableName <|> addColumn tableName <|> addUnique
    let drop = do
            lexeme "DROP"
            dropColumn tableName <|> dropConstraint tableName
    let rename = do
            lexeme "RENAME"
            renameColumn tableName <|> renameTable tableName
    let alter = do
            lexeme "ALTER"
            alterColumn tableName
    enableRowLevelSecurity tableName <|> add <|> drop <|> rename <|> alter

alterType = do
    lexeme "TYPE"
    typeName <- qualifiedIdentifier
    addValue typeName

alterSequence = do
    lexeme "SEQUENCE"
    raw <- cs <$> someTill (anySingle) (char ';')
    pure UnknownStatement { raw = "ALTER SEQUENCE " <> raw };

-- | ALTER TABLE users ALTER COLUMN email DROP NOT NULL;
--  ALTER TABLE users ALTER COLUMN email SET NOT NULL;
--  ALTER TABLE users ALTER COLUMN email SET DEFAULT 'value';
--  ALTER TABLE users ALTER COLUMN email DROP DEFAULT;
alterColumn tableName = do
    lexeme "COLUMN"
    columnName <- identifier

    let drop = do
            lexeme "DROP"
            let notNull = do
                    lexeme "NOT"
                    lexeme "NULL"
                    char ';'
                    pure DropNotNull { tableName, columnName }
            let defaultValue = do
                    lexeme "DEFAULT"
                    char ';'
                    pure DropDefaultValue { tableName, columnName }
            notNull <|> defaultValue
    
    let set = do
            lexeme "SET"
            let notNull = do
                    lexeme "NOT"
                    lexeme "NULL"
                    char ';'
                    pure SetNotNull { tableName, columnName }
            let defaultValue = do
                    lexeme "DEFAULT"
                    value <- expression
                    char ';'
                    pure SetDefaultValue { tableName, columnName, value }
            notNull <|> defaultValue

    drop <|> set
    

    

enableRowLevelSecurity tableName = do
    lexeme "ENABLE"
    lexeme "ROW"
    lexeme "LEVEL"
    lexeme "SECURITY"
    char ';'
    pure EnableRowLevelSecurity { tableName }

createPolicy = do
    lexeme "CREATE"
    lexeme "POLICY"
    name <- identifier
    lexeme "ON"
    tableName <- qualifiedIdentifier

    action <- optional (lexeme "FOR" >> policyAction)

    using <- optional do
        lexeme "USING"
        expression

    check <- optional do
        lexeme "WITH"
        lexeme "CHECK"
        expression

    char ';'

    pure CreatePolicy { name, action, tableName, using, check }

policyAction =
    (lexeme "ALL" >> pure PolicyForAll)
    <|> (lexeme "SELECT" >> pure PolicyForSelect)
    <|> (lexeme "INSERT" >> pure PolicyForInsert)
    <|> (lexeme "UPDATE" >> pure PolicyForUpdate)
    <|> (lexeme "DELETE" >> pure PolicyForDelete)

setStatement = do
    lexeme "SET"
    name <- identifier
    lexeme "="
    value <- expression
    char ';'
    pure Set { name, value }

selectStatement = do
    lexeme "SELECT"
    query <- takeWhile1P (Just "SQL Query") (\c -> c /= ';')
    char ';'
    pure SelectStatement { query }


commentStatement = do
    lexeme "COMMENT"
    content <- takeWhile1P (Just "SQL Query") (\c -> c /= ';')
    char ';'
    pure Comment { content }

qualifiedIdentifier = do
    optional do
        lexeme "public"
        char '.'
    identifier

addColumn tableName = do
    lexeme "COLUMN"
    (_, column) <- parseColumn
    char ';'
    pure AddColumn { tableName, column }

dropColumn tableName = do
    lexeme "COLUMN"
    columnName <- identifier
    char ';'
    pure DropColumn { tableName, columnName }

dropConstraint tableName = do
    lexeme "CONSTRAINT"
    constraintName <- identifier
    char ';'
    pure DropConstraint { tableName, constraintName }

renameColumn tableName = do
    lexeme "COLUMN"
    from <- identifier
    lexeme "TO"
    to <- identifier
    char ';'
    pure RenameColumn { tableName, from, to }

renameTable tableName = do
    lexeme "TO"
    to <- identifier
    char ';'
    pure RenameTable { from = tableName, to }

dropTable = do
    lexeme "DROP"
    lexeme "TABLE"
    tableName <- identifier
    char ';'
    pure DropTable { tableName }

dropType = do
    lexeme "DROP"
    lexeme "TYPE"
    name <- qualifiedIdentifier
    char ';'
    pure DropEnumType { name }

dropIndex = do
    lexeme "DROP"
    lexeme "INDEX"
    indexName <- qualifiedIdentifier
    char ';'
    pure DropIndex { indexName }

dropPolicy = do
    lexeme "DROP"
    lexeme "POLICY"
    policyName <- qualifiedIdentifier
    lexeme "ON"
    tableName <- qualifiedIdentifier
    char ';'
    pure DropPolicy { tableName, policyName }

createSequence = do
    lexeme "CREATE"
    lexeme "SEQUENCE"
    name <- qualifiedIdentifier

    -- We accept all the following SEQUENCE attributes, but don't save them
    -- This is mostly to void issues in migrations when parsing the pg_dump output
    optional do
        lexeme "AS"
        sqlType

    optional do
        lexeme "START"
        lexeme "WITH"
        expression

    optional do
        lexeme "INCREMENT"
        lexeme "BY"
        expression

    optional do
        lexeme "NO"
        lexeme "MINVALUE"

    optional do
        lexeme "NO"
        lexeme "MAXVALUE"

    optional do
        lexeme "CACHE"
        expression

    char ';'
    pure CreateSequence { name }

addValue typeName = do
    lexeme "ADD"
    lexeme "VALUE"
    ifNotExists <- isJust <$> optional do
            lexeme "IF"
            lexeme "NOT"
            lexeme "EXISTS"
    newValue <- textExpr'
    char ';'
    pure AddValueToEnumType { enumName = typeName, newValue, ifNotExists }

begin = do
    lexeme "BEGIN"
    char ';'
    pure Begin

commit = do
    lexeme "COMMIT"
    char ';'
    pure Commit

-- | Turns sql like '1::double precision' into just '1'
removeTypeCasts :: Expression -> Expression
removeTypeCasts (TypeCastExpression value _) = value
removeTypeCasts otherwise = otherwise
