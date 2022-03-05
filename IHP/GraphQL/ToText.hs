module IHP.GraphQL.ToText where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified Data.Text as Text

toText :: [Definition] -> Text
toText definitions =
    definitions
    |> map definitionToText
    |> Text.intercalate "\n" 

definitionToText :: Definition -> Text
definitionToText ExecutableDefinition { operation } = operationToText operation
definitionToText TypeSystemDefinition { typeSystemDefinition } = typeSystemDefinitionToText typeSystemDefinition

operationToText :: OperationDefinition -> Text
operationToText operation = type_
    where
        type_ = case get #operationType operation of
            Query -> "query"
            Mutation -> "mutation"
            Subscription -> "subscription"

typeSystemDefinitionToText :: TypeSystemDefinition -> Text
typeSystemDefinitionToText SchemaDefinition { queryType, mutationType } = [trimming|
    schema {
        query: $query
        mutation: $mutation
    }
|]
    where
        query = typeToText queryType
        mutation = typeToText mutationType

typeSystemDefinitionToText (TypeDefinition typeDefinition) = typeDefinitionToText typeDefinition

typeDefinitionToText ScalarTypeDefinition { name } = [trimming|
    scalar $name
|]
typeDefinitionToText ObjectTypeDefinition { name, implementsInterfaces, fieldDefinitions } = [trimming|
    type $name {
        $fields
    }
|]
    where
        fields = fieldDefinitions
            |> map fieldDefinitionToText
            |> Text.intercalate "\n"
typeDefinitionToText InputObjectTypeDefinition { name, fieldDefinitions } = [trimming|
    input $name {
        $fields
    }
|]
    where
        fields = fieldDefinitions
            |> map fieldDefinitionToText
            |> Text.intercalate "\n"

fieldDefinitionToText :: FieldDefinition -> Text
fieldDefinitionToText FieldDefinition { description, name, argumentsDefinition, type_ } =
    descriptionBlock
    <> [trimming|
        $name$arguments: $renderedType
    |]
    where
        renderedType = typeToText type_
        descriptionBlock = case description of
            Just text -> [trimming|    "$text"|] <> "\n"
            Nothing -> ""
        arguments =
            case argumentsDefinition of
                [] -> ""
                argumentDefinitions -> "(" <> Text.intercalate ", " (map argumentDefinitionToText argumentDefinitions) <> ")"

typeToText :: Type -> Text
typeToText (NamedType name) = name
typeToText (ListType type_) = "[" <> typeToText type_ <> "]"
typeToText (NonNullType type_) = typeToText type_ <> "!"

argumentDefinitionToText :: ArgumentDefinition -> Text
argumentDefinitionToText ArgumentDefinition { name, argumentType } = name <> ": " <> typeToText argumentType