module IHP.GraphQL.JSON where

import IHP.Prelude
import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.Parser as GraphQL
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
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
    parseJSON = Aeson.withObject "Variables" \hashMap -> do
        GraphQL.Variables <$>
            hashMap
                |> HashMap.toList
                |> map (\(argumentName, argumentValue) -> GraphQL.Argument { argumentName, argumentValue = aesonValueToGraphQLValue argumentValue })
                |> pure

aesonValueToGraphQLValue :: Aeson.Value -> GraphQL.Value
aesonValueToGraphQLValue (Aeson.String text) = GraphQL.StringValue text
aesonValueToGraphQLValue (Aeson.Bool bool) = GraphQL.BooleanValue bool
aesonValueToGraphQLValue (Aeson.Object hashMap) = GraphQL.ObjectValue (HashMap.map aesonValueToGraphQLValue hashMap)
aesonValueToGraphQLValue Aeson.Null = GraphQL.NullValue

instance Aeson.ToJSON GraphQL.GraphQLErrorResponse where
    toJSON GraphQL.GraphQLErrorResponse { errors } = Aeson.object
            [ "data" .= Aeson.Null
            , "errors" .= map errorToObj errors
            ]
        where
            errorToObj text = Aeson.object [ "message" .= text ]
