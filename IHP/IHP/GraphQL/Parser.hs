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
import qualified Data.HashMap.Strict as HashMap

parseDocument :: Parser Document
parseDocument = Document <$> many1 parseDefinition

parseDefinition :: Parser Definition
parseDefinition = skipSpace >> (executableDefinition <|> parseFragmentDefinition)

executableDefinition :: Parser Definition
executableDefinition = do
    let query = string "query" >> pure Query
    let mutation = string "mutation" >> pure Mutation
    let subscription = string "subscription" >> pure Subscription

    (operationType,  name, variableDefinitions) <- option (Query, Nothing, []) do
        operationType <- (query <|> mutation <|> subscription) <?> "OperationType"
        skipSpace
        name <- option Nothing (Just <$> parseName)
        skipSpace
        variableDefinitions <- option [] parseVariableDefinitions
        pure (operationType, name, variableDefinitions)

    selectionSet <- parseSelectionSet
    pure ExecutableDefinition { operation = OperationDefinition { operationType, name, selectionSet, variableDefinitions } }

parseFragmentDefinition :: Parser Definition
parseFragmentDefinition = do
    string "fragment"
    skipSpace
    name <- parseName
    skipSpace
    selectionSet <- parseSelectionSet
    pure (FragmentDefinition Fragment { name, selectionSet })


parseVariableDefinitions :: Parser [VariableDefinition]
parseVariableDefinitions = do
    char '('
    skipSpace
    variableDefinitions <- parseVariableDefinition `sepBy` (char ',' >> skipSpace)
    skipSpace
    char ')'
    skipSpace
    pure variableDefinitions

parseVariableDefinition :: Parser VariableDefinition
parseVariableDefinition = do
    variableName <- parseVariableName
    skipSpace
    char ':'
    skipSpace
    variableType <- parseType
    pure VariableDefinition { variableName, variableType }

parseSelectionSet :: Parser [Selection]
parseSelectionSet = (do
    char '{'
    skipSpace
    selectionSet <- many1 parseSelection
    skipSpace
    char '}'
    skipSpace
    pure selectionSet) <?> "selectionSet"

parseSelection :: Parser Selection
parseSelection = parseField <|> parseFragmentSpread

parseField :: Parser Selection
parseField = (do
    nameOrAlias <- parseName
    skipSpace
    name' <- option Nothing do
        char ':'
        skipSpace
        Just <$> parseName

    let alias = case name' of
            Just _ -> Just nameOrAlias
            Nothing -> Nothing
    let name = case name' of
            Just name -> name
            Nothing -> nameOrAlias

    skipSpace

    arguments <- option [] parseArguments

    selectionSet <- option [] parseSelectionSet
    pure Field { alias, name, arguments, directives = [], selectionSet }
    ) <?> "field"

parseFragmentSpread :: Parser Selection
parseFragmentSpread = (do
    string "..."
    fragmentName <- parseName
    skipSpace
    pure FragmentSpread { fragmentName }
    ) <?> "FragmentSpread"

parseArguments :: Parser [Argument]
parseArguments = do
    char '('
    skipSpace
    arguments <- parseArgument `sepBy` (char ',' >> skipSpace)
    skipSpace
    char ')'
    skipSpace
    pure arguments

parseArgument :: Parser Argument
parseArgument = do
    argumentName <- parseName
    skipSpace
    char ':'
    skipSpace
    argumentValue <- parseValue
    pure Argument { argumentName, argumentValue }

parseValue :: Parser Value
parseValue = do
    let variable = Variable <$> parseVariableName
    let object = do
            char '{'
            skipSpace
            values <- parseArgument `sepBy` (char ',' >> skipSpace)
            skipSpace
            char '}'
            skipSpace

            let hashMap :: HashMap.HashMap Text Value = values
                    |> map (\Argument { argumentName, argumentValue } -> (argumentName, argumentValue))
                    |> HashMap.fromList
            pure (ObjectValue hashMap)
    let string = do
            char '"'
            body <- takeTill (== '\"')
            char '"'
            skipSpace
            pure (StringValue body)
    (variable <?> "Variable") <|> (object <?> "Object") <|> (string <?> "String")

parseName :: Parser Text
parseName = takeWhile1 isNameChar <?> "Name"
    where
        isNameChar :: Char -> Bool
        isNameChar !char = (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || (char == '_') || (char >= '0' && char <= '9')

parseVariableName :: Parser Text
parseVariableName = (char '$' >> parseName) <?> "Variable"

parseType :: Parser Type
parseType = do
    inner <- parseNamedType
    option inner do
        string "!"
        pure (NonNullType inner)

parseNamedType :: Parser Type
parseNamedType = NamedType <$> parseName