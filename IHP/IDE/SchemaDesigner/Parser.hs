{-|
Module: IHP.IDE.SchemaDesigner.Types
Description: Parser for Application/Schema.sql
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Parser
( parseSchemaSql
, schemaFilePath
, parseDDL
, expression
, sqlType
) where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Char
import IHP.IDE.SchemaDesigner.Compiler (compileSql)
import Control.Monad.Combinators.Expr

schemaFilePath = "Application/Schema.sql"

parseSchemaSql :: IO (Either ByteString [Statement])
parseSchemaSql = do
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
parseDDL = manyTill statement eof

statement = do
    s <- try createExtension <|> try (StatementCreateTable <$> createTable) <|> try createIndex <|> try createFunction <|> try createTrigger <|> createEnumType <|> addConstraint <|> comment
    space
    pure s


createExtension = do
    lexeme "CREATE"
    lexeme "EXTENSION"
    ifNotExists <- isJust <$> optional (lexeme "IF" >> lexeme "NOT" >> lexeme "EXISTS")
    name <- cs <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))
    char ';'
    pure CreateExtension { name, ifNotExists = True }

createTable = do
    lexeme "CREATE"
    lexeme "TABLE"
    optional do
        lexeme "public"
        char '.'
    name <- identifier

    -- Process columns (tagged if they're primary key) and table constraints
    -- together, as they can be in any order
    (taggedColumns, allConstraints) <- between (char '(' >> space) (char ')' >> space) do
        columnsAndConstraints <- ((Right <$> parseTableConstraint) <|> (Left <$> column)) `sepBy` (char ',' >> space)
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
    name <- identifier
    lexeme "AS"
    lexeme "ENUM"
    values <- between (char '(' >> space) (space >> char ')' >> space) (textExpr' `sepBy` (char ',' >> space))
    space
    char ';'
    pure CreateEnumType { name, values }

addConstraint = do
    lexeme "ALTER"
    lexeme "TABLE"
    tableName <- identifier
    lexeme "ADD"
    lexeme "CONSTRAINT"
    constraintName <- identifier
    constraint <- parseTableConstraint >>= \case
      Left _ -> Prelude.fail "Cannot add new PRIMARY KEY constraint to table"
      Right constraint -> pure constraint
    char ';'
    pure AddConstraint { tableName, constraintName, constraint }

parseTableConstraint = do
    optional do
        lexeme "CONSTRAINT"
        identifier
    (Left <$> parsePrimaryKeyConstraint) <|>
      (Right <$> (parseForeignKeyConstraint <|> parseUniqueConstraint <|> parseCheckConstraint))

parsePrimaryKeyConstraint = do
    lexeme "PRIMARY"
    lexeme "KEY"
    primaryKeyColumnNames <- between (char '(' >> space) (char ')' >> space) (identifier `sepBy1` (char ',' >> space))
    pure PrimaryKeyConstraint { primaryKeyColumnNames }

parseForeignKeyConstraint = do
    lexeme "FOREIGN"
    lexeme "KEY"
    columnName <- between (char '(' >> space) (char ')' >> space) identifier
    lexeme "REFERENCES"
    referenceTable <- identifier
    referenceColumn <- optional $ between (char '(' >> space) (char ')' >> space) identifier
    onDelete <- optional do
        lexeme "ON"
        lexeme "DELETE"
        parseOnDelete
    pure ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete }

parseUniqueConstraint = do
    lexeme "UNIQUE"
    columnNames <- between (char '(' >> space) (char ')' >> space) (identifier `sepBy1` (char ',' >> space))
    pure UniqueConstraint { columnNames }

parseCheckConstraint = do
    lexeme "CHECK"
    checkExpression <- between (char '(' >> space) (char ')' >> space) expression
    pure CheckConstraint { checkExpression }

parseOnDelete = choice
        [ (lexeme "NO" >> lexeme "ACTION") >> pure NoAction
        , (lexeme "RESTRICT" >> pure Restrict)
        , (lexeme "SET" >> ((lexeme "NULL" >> pure SetNull) <|> (lexeme "DEFAULT" >> pure SetDefault)))
        , (lexeme "CASCADE" >> pure Cascade)
        ]

column = do
    name <- identifier
    columnType <- sqlType
    space
    defaultValue <- optional do
        lexeme "DEFAULT"
        expression
    primaryKey <- isJust <$> optional (lexeme "PRIMARY" >> lexeme "KEY")
    notNull <- isJust <$> optional (lexeme "NOT" >> lexeme "NULL")
    isUnique <- isJust <$> optional (lexeme "UNIQUE")
    pure (primaryKey, Column { name, columnType, defaultValue, notNull, isUnique })

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

                date = do
                    try (symbol' "DATE")
                    pure PDate

                binary = do
                    try (symbol' "BYTEA")
                    pure PBinary

                time = do
                    try (symbol' "TIME")
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
                    try (symbol' "CHARACTER VARYING(") <|> try (symbol' "VARCHAR(")
                    value <- between (space) (char ')' >> space) (varExpr)
                    case value of
                        VarExpression limit -> do
                            let l = textToInt limit
                            case l of
                                Nothing -> Prelude.fail "Failed to parse CHARACTER VARYING(..) expression"
                                Just l -> pure (PVaryingN l)
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

                optionalArray typeParser= do
                    arrayType <- typeParser;
                    (try do symbol' "[]"; pure $ PArray arrayType) <|> pure arrayType

                customType = do
                    theType <- try (takeWhile1P (Just "Custom type") (\c -> isAlphaNum c || c == '_'))
                    pure (PCustomType theType)

term = parens expression <|> try callExpr <|> varExpr <|> textExpr
    where
        parens f = between (char '(' >> space) (char ')' >> space) f

table = [ [ binary  "<>"  NotEqExpression ] ]
    where
        binary  name f = InfixL  (f <$ symbol name)
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

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

callExpr :: Parser Expression
callExpr = do
    func <- identifier
    args <- between (char '(') (char ')') (expression `sepBy` char ',')
    pure (CallExpression func args)

textExpr :: Parser Expression
textExpr = TextExpression <$> textExpr'

textExpr' :: Parser Text
textExpr' = cs <$> (char '\'' *> manyTill Lexer.charLiteral (char '\''))

identifier :: Parser Text
identifier = do
    i <- (between (char '"') (char '"') (takeWhile1P Nothing (\c -> c /= '"'))) <|> takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_')
    space
    pure i

comment = do
    lexeme "--" <?> "Line comment"
    content <- takeWhileP Nothing (/= '\n')
    pure Comment { content }

createIndex = do
    lexeme "CREATE"
    lexeme "INDEX"
    indexName <- identifier
    lexeme "ON"
    tableName <- identifier
    columnNames <- between (char '(' >> space) (char ')' >> space) (identifier `sepBy1` (char ',' >> space))
    char ';'
    pure CreateIndex { indexName, tableName, columnNames }

createFunction = do
    lexeme "CREATE"
    orReplace <- isJust <$> optional (lexeme "OR" >> lexeme "REPLACE")
    lexeme "FUNCTION"
    functionName <- identifier
    lexeme "()"
    lexeme "RETURNS"
    lexeme "TRIGGER"
    lexeme "AS"
    space
    functionBody <- cs <$> between (char '$' >> char '$') (char '$' >> char '$') (many (anySingleBut '$'))
    space
    lexeme "language"
    lexeme "plpgsql"
    char ';'
    pure CreateFunction { functionName, functionBody, orReplace }

-- | Triggers are not currently used by IHP, therefore they're implemented using UnknownStatement
-- This avoid errors when having custom triggers in Schema.sql
createTrigger = do
    lexeme "CREATE"
    lexeme "TRIGGER"
    raw <- cs <$> someTill (anySingle) (char ';')
    pure UnknownStatement { raw = "CREATE TRIGGER " <> raw }