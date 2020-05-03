{-|
Module: TurboHaskell.IDE.SchemaDesigner.Types
Description: Parser for Application/Schema.sql
Copyright: (c) digitally induced GmbH, 2020
-}
module TurboHaskell.IDE.SchemaDesigner.Parser (parseSchemaSql, schemaFilePath) where

import TurboHaskell.Prelude
import TurboHaskell.IDE.SchemaDesigner.Types
import qualified Prelude
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Char
import TurboHaskell.IDE.SchemaDesigner.Compiler (compileSql)

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

stringLiteral :: Parser String
stringLiteral = char '\'' *> manyTill Lexer.charLiteral (char '\'')

parseDDL :: Parser [Statement]
parseDDL = manyTill statement eof
    
statement = do
    s <- try createExtension <|> try createTable <|> createEnumType <|> addConstraint <|> comment
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
    name <- identifier
    columns <- between (char '(' >> space) (char ')' >> space) (column `sepBy` (char ',' >> space))
    char ';'
    pure CreateTable { name, columns }

createEnumType = do
    lexeme "CREATE"
    lexeme "TYPE"
    name <- identifier
    lexeme "AS"
    lexeme "ENUM"
    values <- between (char '(' >> space) (char ')' >> space) (stringExpr `sepBy` (char ',' >> space))
    char ';'
    pure CreateEnumType { name, values }

addConstraint = do
    lexeme "ALTER"
    lexeme "TABLE"
    tableName <- identifier
    lexeme "ADD"
    lexeme "CONSTRAINT"
    constraintName <- identifier
    constraint <- parseConstraint
    char ';'
    pure AddConstraint { tableName, constraintName, constraint }

parseConstraint = do
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

parseOnDelete = choice
        [ (lexeme "NO" >> lexeme "ACTION") >> pure NoAction
        , (lexeme "RESTRICT" >> pure Restrict)
        , (lexeme "SET" >> lexeme "NULL") >> pure SetNull
        , (lexeme "CASCADE" >> pure Cascade)
        ]

column = do
    name <- identifier
    columnType <- cs <$> sqlType
    space
    defaultValue <- optional do
        lexeme "DEFAULT"
        value <- expression
        pure (cs value)
    primaryKey <- isJust <$> optional (lexeme "PRIMARY" >> lexeme "KEY")
    notNull <- isJust <$> optional (lexeme "NOT" >> lexeme "NULL")
    isUnique <- isJust <$> optional (lexeme "UNIQUE")
    pure Column { name, columnType, primaryKey, defaultValue, notNull, isUnique }


sqlType = choice
        [ (try $ lexeme "UUID")
        , (try $ lexeme "TEXT")
        , (try $ lexeme "INT")
        , (try $ lexeme "BOOLEAN")
        , (try $ lexeme "TIMESTAMP WITH TIME ZONE")
        , (try $ lexeme "DOUBLE PRECISION")
        , (try $ takeWhile1P (Just "Custom type") (\c -> isAlphaNum c || c == '_'))
        ]


expression = do
    e <- try callExpr <|> varExpr <|> stringExpr 
    space 
    pure e

varExpr :: Parser Text
varExpr = identifier

callExpr = do
    func <- identifier
    args <- between (char '(') (char ')') (expression `sepBy` char ',')
    pure (func <> "(" <> intercalate ", " args <> ")")

stringExpr :: Parser Text
stringExpr = do
    str <- cs <$> (char '\'' *> manyTill Lexer.charLiteral (char '\''))
    pure ("'" <> str <> "'")

identifier :: Parser Text
identifier = do
    i <- takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_')
    space
    pure i

comment = do
    lexeme "--" <?> "Line comment"
    content <- takeWhileP Nothing (/= '\n')
    pure Comment { content }

