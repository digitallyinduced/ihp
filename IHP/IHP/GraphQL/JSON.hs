module IHP.GraphQL.JSON where

import IHP.Prelude
import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.Parser as GraphQL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import Data.Aeson ((.:))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Attoparsec.Text as Attoparsec

instance Aeson.FromJSON GraphQL.GraphQLRequest where
    parseJSON = Aeson.withObject "GraphQLRequest" \v -> do
        query <- v .: "query"
        variables <- (v .: "variables") <|> (pure (GraphQL.Variables []))
        pure GraphQL.GraphQLRequest { query, variables }

instance Aeson.FromJSON GraphQL.Document where
    parseJSON = Aeson.withText "Document" \gql -> do
        case Attoparsec.parseOnly GraphQL.parseDocument gql of
            Left parserError -> fail (cs $ (tshow parserError) <> " while parsing: " <> gql)
            Right statements -> pure statements

instance Aeson.FromJSON GraphQL.Variables where
    parseJSON = Aeson.withObject "Variables" \keyMap -> do
        GraphQL.Variables <$>
            keyMap
                |> Aeson.toList
                |> map (\(argumentName, argumentValue) -> GraphQL.Argument { argumentName = Aeson.toText argumentName, argumentValue = aesonValueToGraphQLValue argumentValue })
                |> pure

aesonValueToGraphQLValue :: Aeson.Value -> GraphQL.Value
aesonValueToGraphQLValue (Aeson.String text) = GraphQL.StringValue text
aesonValueToGraphQLValue (Aeson.Bool bool) = GraphQL.BooleanValue bool
aesonValueToGraphQLValue (Aeson.Object keyMap) = GraphQL.ObjectValue (HashMap.map aesonValueToGraphQLValue (keyMap |> Aeson.toHashMap |> HashMap.mapKeys Aeson.toText))
aesonValueToGraphQLValue Aeson.Null = GraphQL.NullValue
