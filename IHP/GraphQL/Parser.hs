{-|
Module: IHP.GraphQL.Parser
Description: Parser for GraphQL requests
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.GraphQL.Parser where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified Data.Text as Text
import Data.Attoparsec.Text

parseDocument :: Parser Document
parseDocument = Document <$> many1 parseDefinition

parseDefinition :: Parser Definition
parseDefinition = executableDefinition

executableDefinition :: Parser Definition
executableDefinition = do
    selectionSet <- parseSelectionSet
    pure ExecutableDefinition { operation = OperationDefinition { selectionSet }, fragment = FragmentDefinition }

parseSelectionSet :: Parser [Selection]
parseSelectionSet = (do
    char '{'
    skipSpace
    selectionSet <- many1 parseSelection
    skipSpace
    char '}'
    pure selectionSet) <?> "selectionSet"

parseSelection :: Parser Selection
parseSelection = (do
    name <- parseName
    skipSpace
    selectionSet <- option [] parseSelectionSet
    pure Field { alias = Nothing, name, arguments = [], directives = [], selectionSet }
    ) <?> "selection"

parseName :: Parser Text
parseName = takeWhile1 isNameChar <?> "Name"
    where
        isNameChar :: Char -> Bool
        isNameChar !char = (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || (char == '_') || (char >= '0' && char <= '9')