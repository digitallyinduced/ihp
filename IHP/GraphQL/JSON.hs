module IHP.GraphQL.JSON where

import IHP.Prelude
import qualified IHP.GraphQL.Types as GraphQL
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

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
