{-|
Module: IHP.Postgres.Parser
Description: Parser for PostgreSQL DDL statements
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Parser
( parseSqlFile
, parseSqlText
, parseCreateExtensionMigration
, containsCreateExtensionStatement
, parseDDL
, expression
, sqlType
, removeTypeCasts
, parseIndexColumns
, unsetComment
, normalizeComment
) where

import Prelude
import IHP.Postgres.Types hiding (table)
import IHP.Postgres.Compiler (compilePostgresType)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Data.Maybe (isJust, catMaybes, isNothing, listToMaybe, fromMaybe)
import Data.Either (lefts, rights)
import Data.Functor (($>))
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower, toUpper)
import qualified Data.List as List
import Control.Monad (when)
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import System.OsPath (OsPath, decodeUtf)
import Control.Monad.Combinators.Expr

-- | Helper to convert Int parsing results
textToInt :: Text -> Maybe Int
textToInt text = case reads (Text.unpack text) of
    [(n, "")] -> Just n
    _ -> Nothing

parseSqlFile :: OsPath -> IO (Either ByteString [Statement])
parseSqlFile schemaFilePath = do
    fp <- decodeUtf schemaFilePath
    schemaSql <- Text.readFile fp
    pure (parseSqlWithSource fp schemaSql)

-- | Parses SQL text into statements.
parseSqlText :: Text -> Either ByteString [Statement]
parseSqlText = parseSqlWithSource "input"

parseSqlWithSource :: FilePath -> Text -> Either ByteString [Statement]
parseSqlWithSource source sql =
    let result = runParser parseDDL source sql
    in case result of
        Left error -> Left (cs $ errorBundlePretty error)
        Right statements -> Right statements

-- | Parses a migration containing only @CREATE EXTENSION@ statements.
--
-- Comments and whitespace are allowed, but every executable statement must be
-- a @CREATE EXTENSION@. This parser is deliberately separate from 'parseDDL':
-- it is used as a privilege boundary by @ihp-migrate@ and therefore fails
-- closed when it encounters any other statement.
parseCreateExtensionMigration :: Text -> Either ByteString [Statement]
parseCreateExtensionMigration sql =
    case runParser createExtensionMigration "migration" sql of
        Left error -> Left (cs $ errorBundlePretty error)
        Right statements -> Right statements

createExtensionMigration :: Parser [Statement]
createExtensionMigration =
    extensionSpaceConsumer
        *> some (createExtensionForMigration <* extensionSpaceConsumer)
        <* eof

createExtensionForMigration :: Parser Statement
createExtensionForMigration = do
    extensionKeyword "CREATE"
    extensionKeyword "EXTENSION"
    ifNotExists <- isJust <$> optional do
        extensionKeyword "IF"
        extensionKeyword "NOT"
        extensionKeyword "EXISTS"
    name <- extensionIdentifier
    optional (extensionKeyword "WITH")
    extensionOptions <- many extensionOption
    when (hasDuplicateExtensionOptions extensionOptions) (fail "duplicate CREATE EXTENSION option")
    extensionSymbol ";"
    pure CreateExtension { name, ifNotExists, extensionOptions }
    where
        extensionOption = choice
            [ extensionKeyword "SCHEMA" >> (ExtensionSchema <$> extensionIdentifier)
            , extensionKeyword "VERSION" >> (ExtensionVersion <$> extensionVersionText)
            , extensionKeyword "CASCADE" $> ExtensionCascade
            ]

extensionSpaceConsumer :: Parser ()
extensionSpaceConsumer = Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockCommentNested "/*" "*/")

extensionLexeme :: Parser a -> Parser a
extensionLexeme = Lexer.lexeme extensionSpaceConsumer

extensionKeyword :: Text -> Parser ()
extensionKeyword keyword = extensionLexeme (string' keyword <* notFollowedBy (satisfy isIdentifierCharacter)) $> ()

extensionSymbol :: Text -> Parser ()
extensionSymbol value = extensionLexeme (string value) $> ()

extensionIdentifier :: Parser Text
extensionIdentifier = extensionLexeme (quotedIdentifier <|> unquotedIdentifier)
    where
        quotedIdentifier = Text.pack <$> between (char '"') (char '"') (some quotedIdentifierCharacter)
        quotedIdentifierCharacter = try (string "\"\"" $> '"') <|> satisfy (/= '"')
        unquotedIdentifier = do
            firstCharacter <- satisfy (\character -> isAlpha character || character == '_')
            remainingCharacters <- many (satisfy isIdentifierCharacter)
            pure (Text.toLower (Text.pack (firstCharacter : remainingCharacters)))

extensionVersionText :: Parser Text
extensionVersionText = extensionLexeme (quotedVersion <|> unquotedVersion)
    where
        quotedVersion = Text.pack <$> between (char '\'') (char '\'') (many quotedVersionCharacter)
        quotedVersionCharacter = try (string "''" $> '\'') <|> satisfy (/= '\'')
        unquotedVersion = Text.pack <$> some (satisfy isIdentifierCharacter)

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character = isAlphaNum character || character == '_' || character == '$'

-- | Returns 'True' when SQL contains a top-level @CREATE EXTENSION@ token
-- sequence. Quoted strings, quoted identifiers, dollar-quoted bodies, and SQL
-- comments are ignored. It is used only to reject mixed or unsupported
-- extension migrations; a positive match never causes SQL to run with elevated
-- privileges unless 'parseCreateExtensionMigration' also accepts the full file.
containsCreateExtensionStatement :: Text -> Bool
containsCreateExtensionStatement = scan True . Text.unpack
    where
        scan _ [] = False
        scan statementStart ('-' : '-' : rest) = scan statementStart (dropLineComment rest)
        scan statementStart ('/' : '*' : rest) = scan statementStart (dropBlockComment 1 rest)
        scan _ ('\'' : rest) = scan False (dropQuoted '\'' rest)
        scan _ ('"' : rest) = scan False (dropQuoted '"' rest)
        scan statementStart ('$' : rest) = case dollarQuoteDelimiter rest of
            Just (delimiter, afterDelimiter) -> scan False (dropDollarQuoted delimiter afterDelimiter)
            Nothing -> scan False rest
        scan _ (';' : rest) = scan True rest
        scan statementStart input@(character : rest)
            | isSpace character = scan statementStart rest
            | isAlpha character || character == '_' =
                let (wordRest, remaining) = span isIdentifierCharacter rest
                    keyword = map toLower (character : wordRest)
                in if statementStart && keyword == "create" && startsWithExtension remaining
                    then True
                    else scan False remaining
            | otherwise = scan False (drop 1 input)

        startsWithExtension input =
            case dropSqlTrivia input of
                character : rest
                    | isAlpha character || character == '_' ->
                        let (wordRest, _) = span isIdentifierCharacter rest
                        in map toLower (character : wordRest) == "extension"
                _ -> False

dropSqlTrivia :: String -> String
dropSqlTrivia input =
    case dropWhile isSpace input of
        '-' : '-' : rest -> dropSqlTrivia (dropLineComment rest)
        '/' : '*' : rest -> dropSqlTrivia (dropBlockComment 1 rest)
        rest -> rest

dropLineComment :: String -> String
dropLineComment = drop 1 . dropWhile (/= '\n')

dropBlockComment :: Int -> String -> String
dropBlockComment _ [] = []
dropBlockComment depth ('/' : '*' : rest) = dropBlockComment (depth + 1) rest
dropBlockComment 1 ('*' : '/' : rest) = rest
dropBlockComment depth ('*' : '/' : rest) = dropBlockComment (depth - 1) rest
dropBlockComment depth (_ : rest) = dropBlockComment depth rest

dropQuoted :: Char -> String -> String
dropQuoted _ [] = []
dropQuoted quote (character : next : rest)
    | character == quote && next == quote = dropQuoted quote rest
    | character == '\\' = dropQuoted quote rest
dropQuoted quote (character : rest)
    | character == quote = rest
    | otherwise = dropQuoted quote rest

dollarQuoteDelimiter :: String -> Maybe (String, String)
dollarQuoteDelimiter ('$' : afterDelimiter) = Just ("$$", afterDelimiter)
dollarQuoteDelimiter (firstCharacter : rest)
    | isAlpha firstCharacter || firstCharacter == '_' =
        let (remainingTag, remaining) = span (\character -> isAlphaNum character || character == '_') rest
            tag = firstCharacter : remainingTag
        in case remaining of
            '$' : afterDelimiter -> Just ("$" <> tag <> "$", afterDelimiter)
            _ -> Nothing
dollarQuoteDelimiter _ = Nothing

dropDollarQuoted :: String -> String -> String
dropDollarQuoted _ [] = []
dropDollarQuoted delimiter input
    | delimiter `List.isPrefixOf` input = drop (length delimiter) input
dropDollarQuoted delimiter (_ : rest) = dropDollarQuoted delimiter rest

type Parser = Parsec Void Text

-- | Whitespace between two tokens of the same statement.
--
-- PostgreSQL's line comment is @--@, not @\/\/@, so an inline comment such as
-- @id UUID PRIMARY KEY, -- surrogate key@ used to stop the parser mid statement
-- even though the server accepts it.
--
-- At statement level a @--@ comment is not trivia: 'comment' turns it into a
-- 'Comment' statement so the schema keeps it. 'statement' and 'parseDDL'
-- therefore consume plain whitespace with 'Char.space' and leave comments for
-- 'comment' to claim.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "/*" "*/")

-- | Whitespace inside a statement, where a comment is trivia. Shadows
-- 'Char.space' so that every statement parser gets comment handling.
space :: Parser ()
space = spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '\'' *> manyTill stringCharacter (try (char '\'' <* notFollowedBy (char '\'')))
    where
        stringCharacter = try (string "''" $> '\'') <|> Lexer.charLiteral

parseDDL :: Parser [Statement]
parseDDL = optional Char.space >> (manyTill statement eof)

statement = do
    Char.space
    let create = try createExtension <|> try (StatementCreateTable <$> createTable) <|> try createIndex <|> try createFunction <|> try createTrigger <|> try createEnumType <|> try createPolicy <|> try createSequence
    let alter = do
            lexeme "ALTER"
            alterTable <|> alterType <|> alterSequence
    s <- setStatement <|> create <|> alter <|> selectStatement <|> try opaqueStatement <|> try doStatement <|> try dropTable <|> try dropIndex <|> try dropPolicy <|> try dropFunction <|> try dropType <|> dropTrigger <|> commentStatement <|> comment <|> begin <|> commit <|> restrict <|> unrestrict
    Char.space
    pure s


createExtension = do
    lexeme "CREATE"
    lexeme "EXTENSION"
    ifNotExists <- isJust <$> optional (lexeme "IF" >> lexeme "NOT" >> lexeme "EXISTS")
    name <- qualifiedIdentifier
    optional (lexeme "WITH")
    extensionOptions <- many extensionOption
    when (hasDuplicateExtensionOptions extensionOptions) (fail "duplicate CREATE EXTENSION option")
    char ';'
    pure CreateExtension { name, ifNotExists, extensionOptions }
    where
        extensionOption = choice
            [ lexeme "SCHEMA" >> (ExtensionSchema <$> extensionIdentifier)
            , lexeme "VERSION" >> (ExtensionVersion <$> extensionVersionText)
            , lexeme "CASCADE" $> ExtensionCascade
            ]

hasDuplicateExtensionOptions :: [ExtensionOption] -> Bool
hasDuplicateExtensionOptions options = length optionKinds /= length (List.nub optionKinds)
    where
        optionKinds = map (\case ExtensionSchema {} -> (0 :: Int); ExtensionVersion {} -> 1; ExtensionCascade -> 2) options

-- | Privilege and SQL COMMENT statements are not modeled structurally, but
-- discarding them would change schema permissions or metadata. Keep their body.
opaqueStatement :: Parser Statement
opaqueStatement = do
    keyword <- choice (map opaqueKeyword ["GRANT", "REVOKE", "COMMENT"])
    raw <- mconcat <$> someTill opaqueChunk (char ';')
    let statementRaw = keyword <> raw
    let trailingWhitespace = Text.drop (Text.length (Text.stripEnd statementRaw)) statementRaw
    let normalizedRaw
            | Text.any (== '\n') trailingWhitespace = Text.stripEnd statementRaw <> "\n"
            | otherwise = Text.stripEnd statementRaw
    pure UnknownStatement { raw = normalizedRaw }
    where
        opaqueKeyword keyword = try do
            value <- string' keyword
            notFollowedBy (satisfy (\character -> isAlphaNum character || character == '_'))
            pure value

sqlTrivia1 :: Parser ()
sqlTrivia1 = some triviaChunk $> ()
    where
        triviaChunk =
            (space1 $> ())
            <|> Lexer.skipLineComment "--"
            <|> Lexer.skipBlockCommentNested "/*" "*/"

-- | Builds the inverse of an executable COMMENT statement. The `IS` keyword
-- is located lexically, so occurrences inside identifiers, strings, comments,
-- or dollar-quoted text cannot be mistaken for the comment-value delimiter.
unsetComment :: Text -> Maybe Text
unsetComment = parseMaybe do
    keyword <- fst <$> match do
        string' "COMMENT"
        notFollowedBy (satisfy isIdentifierCharacter)
    target <- mconcat <$> manyTill opaqueChunk commentValueDelimiter
    _ <- some anySingle
    eof
    pure (Text.stripEnd (keyword <> target) <> " IS NULL")
    where
        commentValueDelimiter = try do
            sqlTrivia1
            string' "IS"
            notFollowedBy (satisfy isIdentifierCharacter)
            sqlTrivia1

-- | Canonicalizes an executable COMMENT for schema comparison. PostgreSQL
-- folds unquoted identifiers and pg_dump qualifies public objects, while a
-- hand-written Schema.sql commonly keeps their shorter unqualified spelling.
normalizeComment :: Text -> Maybe Text
normalizeComment = parseMaybe do
    string' "COMMENT"
    notFollowedBy (satisfy isIdentifierCharacter)
    sqlTrivia1
    string' "ON"
    notFollowedBy (satisfy isIdentifierCharacter)
    sqlTrivia1
    targetChunks <- manyTill normalizedTargetChunk commentValueDelimiter
    value <- try (normalizedCommentValue <* spaceConsumer <* eof) <|> (Text.strip . Text.pack <$> some anySingle <* eof)
    pure ("COMMENT ON " <> normalizeTarget targetChunks <> " IS " <> value)
    where
        commentValueDelimiter = try do
            sqlTrivia1
            string' "IS"
            notFollowedBy (satisfy isIdentifierCharacter)
            sqlTrivia1

        normalizedTargetChunk =
            try publicQualification
            <|> try escapeStringChunk
            <|> quotedChunk '\''
            <|> quotedChunk '"'
            <|> try dollarQuoted
            <|> (Text.toLower <$> identifierChunk)
            <|> (space1 $> " ")
            <|> (Lexer.skipLineComment "--" $> " ")
            <|> (Lexer.skipBlockCommentNested "/*" "*/" $> " ")
            <|> (Text.singleton <$> anySingle)

        publicQualification = try do
            schema <- identifierChunk
            when (Text.toLower schema /= "public") (fail "not the public schema")
            char '.'
            pure ""

        normalizeTarget :: [Text] -> Text
        normalizeTarget chunks = Text.toUpper objectType <> rest
            where
                target = Text.strip (List.foldl' appendChunk "" chunks)
                (objectType, rest) = Text.break isSpace target

        appendChunk :: Text -> Text -> Text
        appendChunk normalized " "
            | " " `Text.isSuffixOf` normalized = normalized
            | otherwise = normalized <> " "
        appendChunk normalized "(" = Text.stripEnd normalized <> "("
        appendChunk normalized ")" = Text.stripEnd normalized <> ")"
        appendChunk normalized "," = Text.stripEnd normalized <> ", "
        appendChunk normalized chunk = normalized <> chunk

        normalizedCommentValue = quoteString <$> (try dollarQuotedValue <|> standardStringValue)

        dollarQuotedValue = do
            delimiter <- dollarQuoteTag
            Text.pack <$> manyTill anySingle (try (string delimiter))

        standardStringValue = do
            char '\''
            value <- mconcat <$> many (try (string "''" $> "'") <|> (Text.singleton <$> anySingleBut '\''))
            char '\''
            pure value

        quoteString value = "'" <> Text.replace "'" "''" value <> "'"

opaqueChunk :: Parser Text
opaqueChunk =
    try escapeStringChunk
    <|> quotedChunk '\''
    <|> quotedChunk '"'
    <|> identifierChunk
    <|> try dollarQuoted
    <|> (fst <$> match (Lexer.skipLineComment "--"))
    <|> (fst <$> match (Lexer.skipBlockCommentNested "/*" "*/"))
    <|> (Text.singleton <$> anySingle)

quotedChunk :: Char -> Parser Text
quotedChunk quote = fst <$> match do
    char quote
    many (try (char quote >> char quote) <|> anySingleBut quote)
    char quote

escapeStringChunk :: Parser Text
escapeStringChunk = fst <$> match do
    oneOf ['e', 'E']
    char '\''
    many (try (char '\'' >> char '\'') <|> try (char '\\' >> anySingle) <|> anySingleBut '\'')
    char '\''

identifierChunk :: Parser Text
identifierChunk = fst <$> match do
    _ <- satisfy (\character -> isAlpha character || character == '_')
    takeWhileP Nothing (\character -> isAlphaNum character || character == '_' || character == '$')

-- | An anonymous DO block. Its body can contain semicolons, so parse through
-- the matching dollar-quote delimiter before consuming the statement terminator.
doStatement :: Parser Statement
doStatement = do
    sqlLexeme (string' "DO")
    languageBefore <- optional (sqlLexeme (string' "LANGUAGE") >> languageIdentifier)
    body <- dollarQuoted <|> concatenatedStringLiterals
    sqlSpaceConsumer
    languageAfter <- optional (sqlLexeme (string' "LANGUAGE") >> languageIdentifier)
    char ';'
    let language = maybe "" (\name -> "LANGUAGE " <> name <> " ") (languageBefore <|> languageAfter)
    pure UnknownStatement { raw = "DO " <> language <> body }
    where
        concatenatedStringLiterals = do
            first <- stringLiteral
            rest <- many $ try do
                separator <- fst <$> match stringConcatenationSeparator
                next <- stringLiteral
                pure (separator <> next)
            pure (mconcat (first : rest))
        stringLiteral = try unicodeEscapeStringLiteral <|> try prefixedStandardStringLiteral <|> try escapeStringLiteral <|> standardStringLiteral
        stringConcatenationSeparator = do
            separator <- fst <$> match (some separatorChunk)
            when (not (Text.any (`elem` ['\n', '\r']) separator)) (fail "adjacent SQL string literals require a newline")
        separatorChunk =
            (some (satisfy isSpace) $> ())
            <|> Lexer.skipLineComment "--"
            <|> Lexer.skipBlockCommentNested "/*" "*/"
        unicodeEscapeStringLiteral = fst <$> match do
            string' "U&"
            char '\''
            many (try (char '\'' >> char '\'') <|> anySingleBut '\'')
            char '\''
            optional $ try do
                sqlSpaceConsumer
                string' "UESCAPE"
                notFollowedBy (satisfy isIdentifierCharacter)
                sqlSpaceConsumer
                char '\''
                anySingleBut '\''
                char '\''
        escapeStringLiteral = fst <$> match do
            oneOf ['e', 'E']
            char '\''
            many (try (char '\'' >> char '\'') <|> try (char '\\' >> anySingle) <|> anySingleBut '\'')
            char '\''
        prefixedStandardStringLiteral = fst <$> match do
            oneOf ['n', 'N']
            char '\''
            many (try (char '\'' >> char '\'') <|> anySingleBut '\'')
            char '\''
        standardStringLiteral = fst <$> match do
            char '\''
            many (try (char '\'' >> char '\'') <|> anySingleBut '\'')
            char '\''
        languageIdentifier = sqlLexeme (fst <$> match (try unicodeDelimitedIdentifier <|> rawIdentifier))
        unicodeDelimitedIdentifier = do
            string' "U&"
            char '"'
            many (try (string "\"\"") <|> (Text.singleton <$> anySingleBut '"'))
            char '"'
            optional $ try do
                sqlSpaceConsumer
                string' "UESCAPE"
                notFollowedBy (satisfy isIdentifierCharacter)
                sqlSpaceConsumer
                char '\''
                anySingleBut '\''
                char '\''
            pure ()
        rawIdentifier =
            between (char '"') (char '"') (many (try (string "\"\"") <|> (Text.singleton <$> anySingleBut '"'))) $> ()
            <|> some (satisfy (\character -> isAlphaNum character || character == '_' || character == '$')) $> ()
        sqlLexeme = Lexer.lexeme sqlSpaceConsumer
        sqlSpaceConsumer = Lexer.space space1 (Lexer.skipLineComment "--") (Lexer.skipBlockCommentNested "/*" "*/")

-- | A dollar-quoted string including its delimiter.
dollarQuoted :: Parser Text
dollarQuoted = do
    delimiter <- dollarQuoteTag
    body <- cs <$> manyTill anySingle (try (string delimiter))
    pure (delimiter <> body <> delimiter)

dollarQuoteTag :: Parser Text
dollarQuoteTag = do
    char '$'
    tag <- optional do
        firstCharacter <- satisfy (\character -> isAlpha character || character == '_')
        remainingCharacters <- takeWhileP (Just "dollar quote tag") (\character -> isAlphaNum character || character == '_')
        pure (Text.cons firstCharacter remainingCharacters)
    char '$'
    pure ("$" <> maybe "" id tag <> "$")

createTable = do
    lexeme "CREATE"
    unlogged <- isJust <$> optional (lexeme "UNLOGGED")
    lexeme "TABLE"
    name <- qualifiedIdentifier

    -- Process columns (tagged if they're primary key) and table constraints
    -- together, as they can be in any order
    (taggedColumns, allConstraints) <- between (char '(' >> space) (char ')' >> space) do
        columnsAndConstraints <- ((Right <$> parseTableConstraint) <|> (Left <$> parseColumn)) `sepBy` (char ',' >> space)
        pure (lefts columnsAndConstraints, rights columnsAndConstraints)

    inherits <- optional do
        lexeme "INHERITS"
        between (char '(' >> space) (char ')' >> space) qualifiedIdentifier

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
            _ -> fail ("Multiple PRIMARY KEY constraints on table " <> cs name)
        [(_, Column { name })] -> case lefts allConstraints of
            [] -> pure $ PrimaryKeyConstraint [name]
            _ -> fail ("Primary key defined in both column and table constraints on table " <> cs name)
        _ -> fail "Multiple columns with PRIMARY KEY constraint"

    pure CreateTable { name, columns, primaryKeyConstraint, constraints, unlogged, inherits }

createEnumType = do
    lexeme "CREATE"
    lexeme "TYPE"
    name <- qualifiedIdentifier
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

parseDeferrable :: Parser Bool
parseDeferrable = (lexeme "NOT DEFERRABLE" $> False) <|> (lexeme "DEFERRABLE" $> True)

parseDeferrableType :: Parser DeferrableType
parseDeferrableType = do
    lexeme "INITIALLY"
    (lexeme "IMMEDIATE" $> InitiallyImmediate) <|> (lexeme "DEFERRED" $> InitiallyDeferred)

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
            element <- Text.stripEnd . mconcat <$> someTill excludeElementChunk withDelimiter
            operator <- parseCommutativeInfixOperator
            pure ExcludeConstraintElement { element, operator }

        withDelimiter = try do
            optional space1
            string' "WITH"
            notFollowedBy (satisfy isIdentifierCharacter)
            space

        excludeElementChunk =
            try dollarQuotedChunk
            <|> try escapeStringChunk
            <|> quotedChunk '\''
            <|> quotedChunk '"'
            <|> (fst <$> match (Lexer.skipLineComment "--"))
            <|> (fst <$> match (Lexer.skipBlockCommentNested "/*" "*/"))
            <|> identifierChunk
            <|> (Text.singleton <$> anySingle)

        identifierChunk = fst <$> match do
            _ <- satisfy (\character -> isAlpha character || character == '_')
            takeWhileP Nothing isIdentifierCharacter

        dollarQuotedChunk :: Parser Text
        dollarQuotedChunk = fst <$> match do
            delimiter <- do
                char '$'
                tag <- takeWhileP (Just "dollar quote tag") (\c -> isAlphaNum c || c == '_')
                char '$'
                pure ("$" <> tag <> "$")
            _ <- manyTill anySingle (try (string delimiter))
            pure ()

        quotedChunk quote = fst <$> match do
            char quote
            many (try (char quote >> char quote) <|> anySingleBut quote)
            char quote

        escapeStringChunk = fst <$> match do
            oneOf ['e', 'E']
            char '\''
            many (try (char '\\' >> anySingle) <|> try (char '\'' >> char '\'') <|> anySingleBut '\'')
            char '\''

        parseCommutativeInfixOperator = lexeme do
            try identifier <|> takeWhile1P (Just "operator") (`elem` ("+-*/<>=~!@#%^&|`?" :: String))

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
    let
        column = Column
            { name
            , columnType
            , defaultValue = Nothing
            , notNull = False
            , notNullConstraintName = Nothing
            , isUnique = False
            , generator = Nothing
            }
    parseColumnAttributes column False
    where
        parseColumnAttributes column primaryKey = choice
            [ do
                lexeme "DEFAULT"
                value <- expression
                parseColumnAttributes column { defaultValue = Just value } primaryKey
            , do
                lexeme "GENERATED"
                lexeme "ALWAYS"
                lexeme "AS"
                generate <- expression
                stored <- isJust <$> optional (lexeme "STORED")
                parseColumnAttributes column { generator = Just ColumnGenerator { generate, stored } } primaryKey
            , do
                lexeme "PRIMARY"
                lexeme "KEY"
                parseColumnAttributes column True
            , do
                lexeme "NOT"
                lexeme "NULL"
                parseColumnAttributes column { notNull = True } primaryKey
            -- PostgreSQL 18 stores NOT NULL constraints in pg_constraint, so
            -- pg_dump can emit their name before NOT NULL. IHP does not model
            -- constraint names for columns, but it must accept this spelling.
            , do
                lexeme "CONSTRAINT"
                constraintName <- identifier
                lexeme "NOT"
                lexeme "NULL"
                parseColumnAttributes column { notNull = True, notNullConstraintName = Just constraintName } primaryKey
            , do
                lexeme "UNIQUE"
                parseColumnAttributes column { isUnique = True } primaryKey
            , pure (primaryKey, column)
            ]

sqlType :: Parser PostgresType
sqlType = choice $ map optionalArray
        [ uuid
        , text
        , interval --Needs higher precedence otherwise parsed as int
        , bigint
        , smallint
        , int   -- order int after smallint/bigint because symbol INT is prefix of INT2, INT8
        , bool
        , timestamp
        , timestampZ
        , timestampZ'
        , timestamp'
        , real
        , double
        , point
        , polygon
        , geometry -- PostGIS extension type; must come before customType fallback
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
        , eventTrigger
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
                    try (symbol' "TIMESTAMPTZ") <|> try (symbol' "TIMESTAMPZ")
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

                geometry = do
                    try (symbol' "GEOMETRY")
                    modifier <- optional $ between (char '(' >> space) (char ')' >> space)
                        (takeWhile1P (Just "geometry type modifier") (/= ')'))
                    pure (maybe PGeometry (PGeometryWithModifier . Text.strip) modifier)

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

                interval = do
                    try (symbol' "INTERVAL")
                    fields <- optional do
                        choice $ map symbol' intervalFields
                    pure (PInterval fields)

                numericPS = do
                    try (symbol' "NUMERIC(")
                    values <- between (space) (char ')' >> space) (varExpr `sepBy` (char ',' >> space))
                    case values of
                        [VarExpression precision, VarExpression scale] -> do
                            let p = textToInt precision
                            let s = textToInt scale
                            when (or [isNothing p, isNothing s]) do
                                fail "Failed to parse NUMERIC(..) expression"
                            pure (PNumeric p s)
                        [VarExpression precision] -> do
                            let p = textToInt precision
                            when (isNothing p) do
                                fail "Failed to parse NUMERIC(..) expression"
                            pure (PNumeric p Nothing)
                        _ -> fail "Failed to parse NUMERIC(..) expression"

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
                                Nothing -> fail "Failed to parse CHARACTER VARYING(..) expression"
                                Just l -> pure (PVaryingN (Just l))
                        Nothing -> pure (PVaryingN Nothing)
                        _ -> fail "Failed to parse CHARACTER VARYING(..) expression"

                character = do
                    try (symbol' "CHAR(") <|> try (symbol' "CHARACTER(")
                    value <- between (space) (char ')' >> space) (varExpr)
                    case value of
                        VarExpression length -> do
                            let l = textToInt length
                            case l of
                                Nothing -> fail "Failed to parse CHARACTER VARYING(..) expression"
                                Just l -> pure (PCharacterN l)
                        _ -> fail "Failed to parse CHARACTER VARYING(..) expression"

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

                eventTrigger = do
                    try (symbol' "EVENT_TRIGGER")
                    pure PEventTrigger

                customType = do
                    theType <- sourceQualifiedTypeIdentifier
                    -- Custom typmods are flat here; nested parenthesized
                    -- modifiers are not supported by this parser.
                    typeModifier <- optional $ try do
                        char '('
                        value <- takeWhile1P (Just "Custom type modifier") (/= ')')
                        char ')'
                        space
                        pure value
                    pure (PCustomType (maybe theType (\value -> theType <> "(" <> value <> ")") typeModifier))

                sourceQualifiedTypeIdentifier = do
                    schemaOrName <- sourceTypeIdentifier
                    maybeName <- optional (char '.' >> sourceTypeIdentifier)
                    pure case maybeName of
                        Nothing -> schemaOrName
                        Just name
                            | schemaOrName == "public" || schemaOrName == "\"public\"" -> name
                            | otherwise -> schemaOrName <> "." <> name

                sourceTypeIdentifier = quotedTypeIdentifier <|> unquotedTypeIdentifier
                quotedTypeIdentifier = fst <$> match do
                    char '"'
                    _ <- many (try (string "\"\"" $> ()) <|> (anySingleBut '"' $> ()))
                    char '"'
                unquotedTypeIdentifier = do
                    first <- satisfy (\character -> isAlpha character || character == '_')
                    rest <- many (satisfy (\character -> isAlphaNum character || character == '_' || character == '$'))
                    pure (Text.toLower (Text.pack (first : rest)))


intervalFields :: [Text]
intervalFields =  [ "YEAR TO MONTH", "DAY TO HOUR", "DAY TO MINUTE", "DAY TO SECOND"
                   , "HOUR TO MINUTE", "HOUR TO SECOND", "MINUTE TO SECOND"
                   , "YEAR",  "MONTH", "DAY", "HOUR", "MINUTE", "SECOND"]


term = parens expression <|> try variadicExpr <|> try arrayExpr <|> try typedLiteralExpr <|> try callExpr <|> try doubleExpr <|> try intExpr <|> selectExpr <|> varExpr <|> (textExpr <* optional space)
    where
        parens f = between (char '(' >> space) (char ')' >> space) f

table = highPrecedenceTable <> genericOperatorTable <>
        [
            [ Postfix (foldl1 (flip (.)) <$> some (try notInOp <|> try notBetweenOp <|> try betweenOp <|> inOp))
            ],
            [ keywordOperator "NOT LIKE", keywordOperator "NOT ILIKE"
            , keywordOperator "LIKE", keywordOperator "ILIKE"
            ],
            [ Postfix escapeOp
            ],
            [ binary  "<>"  NotEqExpression
            -- `!=` is PostgreSQL's spelling of `<>`; the compiler prints the
            -- canonical `<>` back, which is what pg_dump emits.
            , binary  "!="  NotEqExpression
            , binary "="  EqExpression

            , binary "<=" LessThanOrEqualToExpression
            , binary "<"  LessThanExpression
            , binary ">="  GreaterThanOrEqualToExpression
            , binary ">"  GreaterThanExpression
            , binary "IS" IsExpression
            , prefix "NOT" NotExpression
            , prefix "EXISTS" ExistsExpression
            ],
            [ binary "AND" AndExpression, binary "OR" OrExpression ]
        ]
    where
        highPrecedenceTable =
            [ -- Chained postfix operators bind tighter than every infix
              -- operator, so `a::integer + 1` casts `a`, not the sum.
              [ Postfix (foldl1 (flip (.)) <$> some (typeCastOp <|> dotOp))
              ]
            , [ keywordOperator "AT TIME ZONE" ]
            , [ operator "^" ]
            , [ operator "*", operator "/", operator "%" ]
            , [ operator "+", operator "-" ]
            ]

        genericOperatorTable = [[genericOperator, InfixL (ConcatenationExpression <$ try (symbol "||"))]]

        binary  name f = InfixL  (f <$ try (symbol name))
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)

        -- | An operator kept verbatim in 'BinaryOperatorExpression'.
        operator name = InfixL (BinaryOperatorExpression name <$ try (lexeme (string name <* notFollowedBy (satisfy isOperatorCharacter))))

        genericOperator = InfixL do
            name <- try do
                name <- lexeme (Text.pack <$> some (satisfy isOperatorCharacter))
                when (name `elem` dedicatedOperators) (fail "operator has dedicated precedence")
                pure name
            pure (BinaryOperatorExpression name)

        isOperatorCharacter character = character `elem` ("+-*/<>=~!@#%^&|`?" :: String)
        dedicatedOperators = ["*", "/", "%", "+", "-", "<>", "!=", "=", "<=", "<", ">=", ">", "||"]

        -- | Same, for operators spelled as words, which need a word boundary so
        -- that e.g. `LIKE` does not match the start of a `likelihood` column.
        keywordOperator name = InfixL (BinaryOperatorExpression name <$ try (mapM_ keyword (Text.words name)))

        keyword name = try do
            lexeme (string' name <* notFollowedBy (satisfy isIdentifierCharacter))

        escapeOp = do
            keyword "ESCAPE"
            escapeCharacter <- boundExpression
            pure (\patternExpression -> BinaryOperatorExpression "ESCAPE" patternExpression escapeCharacter)

        -- Cannot be implemented as a infix operator as that requires two expression operands,
        -- but the second is the type-cast type which is not an expression
        typeCastOp = do
            symbol "::"
            castType <- sqlType
            pure $ \expr -> TypeCastExpression expr castType

        dotOp = do
            char '.'
            name <- identifier
            pure $ \expr -> DotExpression expr name

        inOp = do
            keyword "IN"
            right <- try inArrayExpression <|> expression
            pure $ \expr -> InExpression expr right

        notInOp = do
            keyword "NOT"
            keyword "IN"
            right <- try inArrayExpression <|> expression
            pure $ \expr -> BinaryOperatorExpression "NOT IN" expr right

        notBetweenOp = do
            keyword "NOT"
            keyword "BETWEEN"
            lower <- boundExpression
            keyword "AND"
            upper <- boundExpression
            pure $ \expr -> NotExpression (AndExpression (GreaterThanOrEqualToExpression expr lower) (LessThanOrEqualToExpression expr upper))

        betweenOp = do
            keyword "BETWEEN"
            lower <- boundExpression
            keyword "AND"
            upper <- boundExpression
            pure $ \expr -> AndExpression (GreaterThanOrEqualToExpression expr lower) (LessThanOrEqualToExpression expr upper)

        boundExpression = do
            value <- makeExprParser term (highPrecedenceTable <> genericOperatorTable <> [[binary "||" ConcatenationExpression]])
            space
            pure value

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

-- | Numeric literals are lexemes: without consuming the whitespace that follows
-- them, `makeExprParser` cannot see the operator behind it and
-- @CHECK (a > 0 AND b > 0)@ fails where @CHECK (a > 'x' AND …)@ succeeds.
doubleExpr :: Parser Expression
doubleExpr = NumericExpression . fst <$> lexeme (match (Lexer.signed spaceConsumer Lexer.float))

intExpr :: Parser Expression
intExpr = IntExpression <$> lexeme (Lexer.signed spaceConsumer Lexer.decimal)

-- | PostgreSQL's @TYPE 'value'@ syntax, normalized to the equivalent cast.
typedLiteralExpr :: Parser Expression
typedLiteralExpr = do
    literalType <- sqlType
    value <- textExpr <* space
    literalType <- case literalType of
        PInterval Nothing -> PInterval <$> optional (choice (map symbol' intervalFields))
        _ -> pure literalType
    pure (TypeCastExpression value literalType)

callExpr :: Parser Expression
callExpr = do
    func <- qualifiedIdentifier
    args <- between (char '(') (char ')') (expression `sepBy` (char ',' >> space))
    space
    pure (CallExpression func args)

-- | Parses a PostgreSQL array literal like @ARRAY['a', 'b', 'c']@.
--
-- pg_dump normalizes @x IN ('a', 'b', 'c')@ CHECK constraints to
-- @x = ANY (ARRAY['a'::text, 'b'::text, 'c'::text])@, so the parser must
-- understand array literals to round-trip the dump output.
arrayExpr :: Parser Expression
arrayExpr = do
    symbol' "ARRAY"
    values <- between (char '[' >> space) (char ']') (expression `sepBy` (char ',' >> space))
    space
    pure (ArrayLiteralExpression values)

-- | Parses a PostgreSQL VARIADIC function argument like @VARIADIC ARRAY['a']@.
variadicExpr :: Parser Expression
variadicExpr = do
    lexeme do
        string' "VARIADIC"
        notFollowedBy (satisfy \c -> isAlphaNum c || c == '_')
    -- Use expression rather than term so postfix type casts stay part of the
    -- variadic argument, e.g. VARIADIC ARRAY['a']::text[].
    VariadicExpression <$> expression

textExpr :: Parser Expression
textExpr = TextExpression <$> textExpr'

textExpr' :: Parser Text
textExpr' = cs <$> do
    let emptyByteString = do
            string "'\\x'"
            pure ""
    (try (char '\'' *> many (try (string "''" $> '\'') <|> (notFollowedBy (char '\'') *> Lexer.charLiteral)) <* char '\'')) <|> emptyByteString

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

inArrayExpression :: Parser Expression
inArrayExpression = do
    values <- between (char '(') (char ')') (expression `sepBy` (char ',' >> space))
    pure (InArrayExpression values)



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
    columns <- between (char '(' >> space) (char ')' >> space) parseIndexColumns
    nullsDistinct <- option True $ lexeme "NULLS" *> (
            (lexeme "NOT" *> lexeme "DISTINCT" $> False)
        <|> (lexeme "DISTINCT" $> True)
        )
    whereClause <- optional do
        lexeme "WHERE"
        expression
    char ';'
    pure CreateIndex { indexName, unique, tableName, columns, whereClause, indexType, nullsDistinct }

parseIndexColumns = parseIndexColumn `sepBy` (char ',' >> space)

parseIndexColumn = do
    column <- expression
    columnOperatorClass <- optional parseIndexColumnOperatorClass
    orderOption1 <- optional $ space *> lexeme "ASC" $> Asc <|> space *> lexeme "DESC" $> Desc
    orderOption2 <- optional $ space *> lexeme "NULLS FIRST" $> NullsFirst <|> space *> lexeme "NULLS LAST" $> NullsLast
    pure IndexColumn { column, columnOperatorClass, columnOrder = catMaybes [orderOption1, orderOption2] }

parseIndexColumnOperatorClass = try do
    operatorClass <- qualifiedIdentifier
    -- These tokens belong to index column ordering, not operator classes.
    -- Extend this list if the parser grows support for more index-column
    -- clauses such as COLLATE.
    when (Text.toUpper operatorClass `elem` ["ASC", "DESC", "NULLS"]) do
        fail "Expected index operator class"
    pure operatorClass

parseIndexType = do
    lexeme "USING"

    choice $ map (uncurry parseIndexTypeKeyword)
        [ ("btree", Btree)
        , ("hash", Hash)
        , ("spgist", Spgist)
        , ("gist", Gist)
        , ("gin", Gin)
        , ("brin", Brin)
        , ("hnsw", Hnsw)
        , ("ivfflat", Ivfflat)
        ]

parseIndexTypeKeyword :: Text -> IndexType -> Parser IndexType
parseIndexTypeKeyword keyword indexType = try do
    string' keyword
    notFollowedBy (satisfy \c -> isAlphaNum c || c == '_')
    space
    pure indexType

data FunctionOption
    = FunctionLanguage Text
    | FunctionSecurityDefiner
    | FunctionSettingOption FunctionSetting
    | FunctionAttribute Text

createFunction = do
    lexeme "CREATE"
    orReplace <- isJust <$> optional (lexeme "OR" >> lexeme "REPLACE")
    lexeme "FUNCTION"
    functionName <- functionIdentifier
    functionArguments <- between (char '(') (char ')') (functionArgument `sepBy` (char ',' >> space))
    space
    lexeme "RETURNS"
    returns <- functionReturnType
    space

    functionOptionsBeforeBody <- many parseFunctionOption

    lexeme "AS"
    space
    functionBody <- functionBodyText
    space

    functionOptionsAfterBody <- many parseFunctionOption
    let functionOptions = functionOptionsBeforeBody <> functionOptionsAfterBody
    let securityDefiner = any isSecurityDefiner functionOptions
    let functionAttributes = [attribute | FunctionAttribute attribute <- functionOptions]
    let functionSettings = [functionSetting | FunctionSettingOption functionSetting <- functionOptions]
    language <- case listToMaybe [language | FunctionLanguage language <- functionOptions] of
        Just language -> pure language
        Nothing -> fail "CREATE FUNCTION requires a LANGUAGE option"
    char ';'
    pure CreateFunction { functionName, functionArguments, functionBody, orReplace, returns, language, securityDefiner, functionAttributes, functionSettings }
    where
        functionArgument = do
            argumentName <- functionArgumentName
            space
            argumentType <- sqlType
            pure (argumentName, argumentType)
        functionArgumentName = quotedArgumentName <|> unquotedArgumentName
        quotedArgumentName = Text.pack <$> between (char '"') (char '"') (some quotedArgumentNameCharacter)
        quotedArgumentNameCharacter = try (string "\"\"" $> '"') <|> anySingleBut '"'
        unquotedArgumentName = Text.toLower <$> takeWhile1P (Just "function argument name") (\c -> isAlphaNum c || c == '_')
        functionReturnType =
            try (symbol' "SETOF" >> (PSetOf <$> sqlType))
            <|> try do
                symbol' "TABLE"
                PTable <$> between (char '(' >> space) (char ')' >> space) (functionArgument `sepBy1` (char ',' >> space))
            <|> sqlType
        isSecurityDefiner FunctionSecurityDefiner = True
        isSecurityDefiner _ = False

-- | Parse the function body without assuming the delimiter is exactly $$ or
-- that the body contains no dollar sign (for example a $1 parameter reference).
functionBodyText :: Parser Text
functionBodyText = do
    delimiter <- dollarQuoteTag
    cs <$> manyTill anySingle (try (string delimiter))

parseFunctionOption :: Parser FunctionOption
parseFunctionOption =
    try parseFunctionLanguage
    <|> try parseFunctionSecurityDefiner
    <|> try parseFunctionSetting
    <|> try parseFunctionAttribute

parseFunctionLanguage :: Parser FunctionOption
parseFunctionLanguage = do
    symbol' "language"
    FunctionLanguage <$> (symbol' "plpgsql" <|> symbol' "SQL")

parseFunctionSecurityDefiner :: Parser FunctionOption
parseFunctionSecurityDefiner = do
    symbol' "SECURITY"
    symbol' "DEFINER"
    pure FunctionSecurityDefiner

parseFunctionSetting :: Parser FunctionOption
parseFunctionSetting = do
    symbol' "SET"
    settingName <- qualifiedIdentifier
    symbol "=" <|> symbol' "TO"
    settingValue <- Text.intercalate ", " <$> settingValueItem `sepBy1` symbol ","
    pure (FunctionSettingOption FunctionSetting { settingName, settingValue })
    where
        settingValueItem = lexeme
            (try dollarQuotedSettingValue <|> try unicodeEscapeStringSettingValue <|> try escapeStringSettingValue <|> singleQuotedSettingValue <|> doubleQuotedSettingValue <|> takeWhile1P (Just "setting value") (\c -> not (isSpace c) && c /= ','))
        dollarQuotedSettingValue = fst <$> match do
            delimiter <- settingDollarQuoteDelimiter
            _ <- manyTill anySingle (try (string delimiter))
            pure ()
        settingDollarQuoteDelimiter = do
            char '$'
            tag <- optional do
                first <- satisfy (\character -> isAlpha character || character == '_')
                rest <- many (satisfy (\character -> isAlphaNum character || character == '_'))
                pure (Text.pack (first : rest))
            char '$'
            pure ("$" <> maybe "" id tag <> "$")
        escapeStringSettingValue = fst <$> match do
            oneOf ['e', 'E']
            char '\''
            _ <- many (try (char '\'' >> char '\'') <|> try (char '\\' >> anySingle) <|> anySingleBut '\'')
            char '\''
        unicodeEscapeStringSettingValue = fst <$> match do
            string' "U&"
            char '\''
            _ <- many (try (char '\'' >> char '\'') <|> anySingleBut '\'')
            char '\''
            optional $ try do
                space1
                string' "UESCAPE"
                notFollowedBy (satisfy isIdentifierCharacter)
                space
                char '\''
                anySingleBut '\''
                char '\''
        singleQuotedSettingValue = fst <$> match do
            char '\''
            _ <- many (try (string "''") <|> (Text.singleton <$> satisfy (/= '\'')))
            char '\''
        doubleQuotedSettingValue = fst <$> match do
            char '"'
            _ <- many (try (string "\"\"") <|> (Text.singleton <$> satisfy (/= '"')))
            char '"'

-- | Volatility, strictness, parallelism and cost attributes as printed by pg_dump.
-- They affect function behaviour, so keep their canonical spelling in the AST
-- and emit them again when compiling the schema.
parseFunctionAttribute :: Parser FunctionOption
parseFunctionAttribute = do
    FunctionAttribute <$> choice
        [ keyword "IMMUTABLE"
        , keyword "STABLE"
        , keyword "VOLATILE"
        , keyword "LEAKPROOF"
        , keyword "WINDOW"
        , keyword "STRICT"
        , phrase ["NOT", "LEAKPROOF"]
        , phrase ["CALLED", "ON", "NULL", "INPUT"]
        , phrase ["RETURNS", "NULL", "ON", "NULL", "INPUT"]
        , phrase ["SECURITY", "INVOKER"]
        , try do
            keywordPrefix "PARALLEL"
            mode <- keyword "SAFE" <|> keyword "RESTRICTED" <|> keyword "UNSAFE"
            pure ("PARALLEL " <> mode)
        , numericAttribute "COST"
        , numericAttribute "ROWS"
        , supportAttribute
        , transformAttribute
        ]
    where
        keyword value = try (functionOptionBoundaryKeyword value) $> value
        phrase values = try (mapM_ functionOptionBoundaryKeyword values) $> Text.unwords values
        keywordPrefix value = try (functionOptionBoundaryKeyword value)
        numericAttribute name = try do
            functionOptionBoundaryKeyword name
            value <- fst <$> match do
                _ <- try (some digitChar >> optional (char '.' >> many digitChar) $> ())
                    <|> (char '.' >> some digitChar $> ())
                optional do
                    oneOf ['e', 'E']
                    optional (oneOf ['+', '-'])
                    _ <- some digitChar
                    pure ()
                pure ()
            space
            pure (name <> " " <> value)
        supportAttribute = try do
            functionOptionBoundaryKeyword "SUPPORT"
            supportFunction <- supportFunctionIdentifier
            pure ("SUPPORT " <> supportFunction)
        transformAttribute = try do
            functionOptionBoundaryKeyword "TRANSFORM"
            firstType <- transformType
            additionalTypes <- many $ try do
                char ','
                space
                transformType
            pure ("TRANSFORM FOR TYPE " <> Text.intercalate ", FOR TYPE " (firstType : additionalTypes))

        transformType = do
            functionOptionBoundaryKeyword "FOR"
            functionOptionBoundaryKeyword "TYPE"
            type_ <- sqlType
            space
            pure (compilePostgresType type_)

        supportFunctionIdentifier = do
            (schemaOrName, _) <- sourceIdentifier
            maybeName <- optional (char '.' >> sourceIdentifier)
            space
            pure case maybeName of
                Nothing -> schemaOrName
                Just (name, _)
                    | schemaOrName == "public" || schemaOrName == "\"public\"" -> name
                    | otherwise -> schemaOrName <> "." <> name

        sourceIdentifier = quotedIdentifier <|> unquotedIdentifier
        quotedIdentifier = do
            raw <- fst <$> match do
                char '"'
                _ <- many (try (string "\"\"" $> ()) <|> (anySingleBut '"' $> ()))
                char '"'
            pure (raw, True)
        unquotedIdentifier = do
            first <- satisfy (\c -> isAlpha c || c == '_')
            rest <- many (satisfy isIdentifierCharacter)
            pure (Text.toLower (Text.pack (first : rest)), False)

functionOptionBoundaryKeyword :: Text -> Parser ()
functionOptionBoundaryKeyword keyword = do
    string' keyword
    notFollowedBy (satisfy \c -> isAlphaNum c || c == '_')
    space

createTrigger = do
    lexeme "CREATE"
    createEventTrigger <|> createTrigger'

createEventTrigger = do
    lexeme "EVENT"
    lexeme "TRIGGER"

    name <- qualifiedIdentifier
    lexeme "ON"
    eventOn <- identifier

    whenCondition <- optional do
        lexeme "WHEN"
        expression

    lexeme "EXECUTE"
    (lexeme "FUNCTION") <|> (lexeme "PROCEDURE")

    (CallExpression functionName arguments) <- callExpr

    char ';'

    pure CreateEventTrigger
        { name
        , eventOn
        , whenCondition
        , functionName
        , arguments
        }



createTrigger' = do
    lexeme "TRIGGER"

    name <- qualifiedIdentifier
    eventWhen <- (lexeme "AFTER" >> pure After) <|> (lexeme "BEFORE" >> pure Before) <|> (lexeme "INSTEAD OF" >> pure InsteadOf)
    event <- triggerEvent `sepBy1` lexeme "OR"

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

triggerEvent :: Parser TriggerEvent
triggerEvent = (lexeme "INSERT" >> pure TriggerOnInsert) <|> (lexeme "UPDATE" >> pure TriggerOnUpdate) <|> (lexeme "DELETE" >> pure TriggerOnDelete) <|> (lexeme "TRUNCATE" >> pure TriggerOnTruncate)

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
    enableRowLevelSecurity tableName <|> noForceRowLevelSecurity tableName <|> forceRowLevelSecurity tableName <|> add <|> drop <|> rename <|> alter

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

forceRowLevelSecurity tableName = do
    lexeme "FORCE"
    lexeme "ROW"
    lexeme "LEVEL"
    lexeme "SECURITY"
    char ';'
    pure ForceRowLevelSecurity { tableName }

noForceRowLevelSecurity tableName = do
    lexeme "NO"
    lexeme "FORCE"
    lexeme "ROW"
    lexeme "LEVEL"
    lexeme "SECURITY"
    char ';'
    pure NoForceRowLevelSecurity { tableName }

createPolicy = do
    lexeme "CREATE"
    lexeme "POLICY"
    name <- identifier
    lexeme "ON"
    tableName <- qualifiedIdentifier

    action <- optional (lexeme "FOR" >> policyAction)

    roles <- fromMaybe [] <$> optional do
        lexeme "TO"
        policyRole `sepBy1` (char ',' >> space)

    using <- optional do
        lexeme "USING"
        expression

    check <- optional do
        lexeme "WITH"
        lexeme "CHECK"
        expression

    char ';'

    pure CreatePolicy { name, action, tableName, roles, using, check }

policyRole :: Parser PolicyRole
policyRole = quotedRole <|> unquotedRole
    where
        quotedRole = do
            role <- Text.pack <$> between (char '"') (char '"') (some (try (string "\"\"" $> '"') <|> anySingleBut '"'))
            space
            pure (QuotedPolicyRole role)
        unquotedRole = do
            first <- satisfy (\character -> isAlpha character || character == '_')
            rest <- takeWhileP (Just "policy role") (\character -> isAlphaNum character || character == '_' || character == '$')
            space
            let role = Text.cons first rest
            let upperRole = foldAsciiUpper role
            pure if upperRole `elem` ["PUBLIC", "CURRENT_ROLE", "CURRENT_USER", "SESSION_USER"]
                then SpecialPolicyRole upperRole
                else PolicyRole (foldAscii role)
        foldAscii = Text.map \character ->
            if character >= 'A' && character <= 'Z'
                then toLower character
                else character
        foldAsciiUpper = Text.map \character ->
            if character >= 'a' && character <= 'z'
                then toUpper character
                else character

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

-- | Parse a possibly schema-qualified identifier. The default public schema is
-- normalized away for backward compatibility; every other schema is preserved.
qualifiedIdentifier :: Parser Text
qualifiedIdentifier = do
    schemaOrName <- identifier
    maybeName <- optional (char '.' >> identifier)
    pure $ case maybeName of
        Nothing -> schemaOrName
        Just name
            | schemaOrName == "public" -> name
            | otherwise -> schemaOrName <> "." <> name

-- | Parses a (possibly schema-qualified) function name.
--
-- Like 'qualifiedIdentifier' this normalizes the default @public@ schema away
-- (@public.foo@ becomes @foo@) so function names compare equal regardless of
-- whether they were written qualified or not. Unlike 'qualifiedIdentifier' it
-- preserves non-@public@ schemas (e.g. @private.sync_access@) as emitted by
-- pg_dump.
functionIdentifier :: Parser Text
functionIdentifier = do
    schemaOrName <- identifier
    maybeName <- optional (char '.' >> identifier)
    pure $ case maybeName of
        Nothing -> schemaOrName
        Just name
            | schemaOrName == "public" -> name
            | otherwise -> schemaOrName <> "." <> name

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
    tableName <- qualifiedIdentifier
    char ';'
    pure DropTable { tableName }

dropType = do
    lexeme "DROP"
    lexeme "TYPE"
    name <- qualifiedIdentifier
    char ';'
    pure DropEnumType { name }

dropFunction = do
    lexeme "DROP"
    lexeme "FUNCTION"
    functionName <- functionIdentifier
    char ';'
    pure DropFunction { functionName }

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

dropTrigger = do
    lexeme "DROP"

    dropEventTrigger <|> dropTrigger'

dropTrigger' = do
    lexeme "TRIGGER"
    name <- qualifiedIdentifier
    lexeme "ON"
    tableName <- qualifiedIdentifier
    char ';'
    pure DropTrigger { name, tableName }


dropEventTrigger = do
    lexeme "EVENT"
    lexeme "TRIGGER"
    name <- qualifiedIdentifier
    char ';'
    pure DropEventTrigger { name }

createSequence = do
    lexeme "CREATE"
    lexeme "SEQUENCE"
    name <- qualifiedIdentifier
    sequenceOptions <- many sequenceOption
    char ';'
    pure CreateSequence { name, sequenceOptions }
    where
        sequenceOption = choice
            [ lexeme "AS" >> (SequenceAs <$> sqlType)
            , lexeme "START" >> optional (lexeme "WITH") >> (SequenceStart <$> sequenceValue)
            , lexeme "INCREMENT" >> optional (lexeme "BY") >> (SequenceIncrement <$> sequenceValue)
            , lexeme "NO" >> ((lexeme "MINVALUE" $> SequenceNoMinValue) <|> (lexeme "MAXVALUE" $> SequenceNoMaxValue) <|> (lexeme "CYCLE" $> SequenceCycle False))
            , lexeme "MINVALUE" >> (SequenceMinValue <$> sequenceValue)
            , lexeme "MAXVALUE" >> (SequenceMaxValue <$> sequenceValue)
            , lexeme "CACHE" >> (SequenceCache <$> sequenceValue)
            , lexeme "CYCLE" $> SequenceCycle True
            ]
        sequenceValue = lexeme (try doubleExpr <|> intExpr)

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

-- | pg_dump 17.5 and later fence their output with @\restrict <key>@. The key is
-- read with 'restrictKey' rather than 'identifier' because 'identifier' consumes
-- the trivia behind it, and here that trivia is the next statement: a comment.
restrict = do
    lexeme "\\restrict"
    key <- restrictKey
    pure Comment { content = "" }

unrestrict = do
    lexeme "\\unrestrict"
    key <- restrictKey
    pure Comment { content = "" }

restrictKey :: Parser Text
restrictKey = takeWhile1P (Just "restrict key") (\c -> isAlphaNum c || c == '_')
