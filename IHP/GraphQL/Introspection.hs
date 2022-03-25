module IHP.GraphQL.Introspection where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

resolveStaticGraph :: StaticGraph -> Document -> StaticGraph
resolveStaticGraph graph document@(Document { definitions = (ExecutableDefinition { operation = OperationDefinition { selectionSet } }:rest) }) = mergeManyNodes $ map (makeSubGraph graph) selectionSet
    where
        nameOrAlias :: Selection -> Text
        nameOrAlias field = fromMaybe (get #name field) (get #alias field)

        findFragmentByName :: Document -> Text -> Fragment
        findFragmentByName document name =
            let
                allFragmentNames = document
                        |> get #definitions
                        |> mapMaybe (\case FragmentDefinition (Fragment { name }) -> Just name; _ -> Nothing)
                couldNotFindFragmentErrorMessage = "Could not find fragment named " <> name <> ". These fragments are defined: " <> Text.intercalate ", " allFragmentNames
            in
                document
                    |> get #definitions
                    |> find (\case
                            FragmentDefinition (Fragment { name = fragmentName }) -> name == fragmentName
                            otherwise -> False
                        )
                    |> fromMaybe (error couldNotFindFragmentErrorMessage)
                    |> \case
                        FragmentDefinition fragment -> fragment

        makeSubGraph :: StaticGraph -> Selection -> StaticGraph
        makeSubGraph graph field@(Field { name, selectionSet = [] }) =
            let
                targetLeaf = graph
                        |> (\case
                                ObjectNode { objectValues } -> objectValues
                                otherwise -> error $ "expected object node, got " <> tshow otherwise
                            )
                        |> HashMap.lookup name
                        |> \case
                            Just leaf@(Leaf value) -> leaf
                            otherwise -> error $ "expected leaf node at " <> name <> ", got " <> tshow otherwise <> " in graph " <> tshow graph
            in
                ObjectNode { objectValues = HashMap.singleton (nameOrAlias field) targetLeaf }
        makeSubGraph graph field@(Field { name, selectionSet }) =
            let
                targetNode :: StaticGraph
                targetNode = graph
                        |> (\case
                                ObjectNode { objectValues } -> objectValues
                                otherwise -> error $ "expected object node, got " <> tshow otherwise
                            )
                        |> HashMap.lookup name
                        |> fromMaybe (error $ "Could not find node " <> name)
            in
                case targetNode of
                    ObjectNode {} ->
                        ObjectNode { objectValues = HashMap.singleton (nameOrAlias field) (mergeManyNodes $ map (makeSubGraph targetNode) selectionSet) }
                    ArrayNode { arrayElements } ->
                        ObjectNode { objectValues = HashMap.singleton (nameOrAlias field) (ArrayNode (map (\targetNode -> mergeManyNodes $ map (makeSubGraph targetNode) selectionSet) arrayElements)) }
                    Leaf { value = NullValue } ->  ObjectNode { objectValues = HashMap.singleton (nameOrAlias field) (Leaf NullValue) }
                    otherwise -> error $ "Expected object or array, got " <> tshow otherwise <> " while trying to access " <> name
        makeSubGraph graph FragmentSpread { fragmentName } =
            let
                fragment = findFragmentByName document fragmentName
                selectionSet = get #selectionSet fragment
            in
                mergeManyNodes (map (makeSubGraph graph) selectionSet)

mergeNodes (ObjectNode { objectValues = a }) (ObjectNode { objectValues = b }) = ObjectNode { objectValues = HashMap.union b a }
mergeManyNodes = foldl' mergeNodes (ObjectNode HashMap.empty)

introspectionGraph :: [Definition] -> StaticGraph
introspectionGraph definitions = 
    object
        [ ("__schema", object
                [ ("queryType", object [ ("name", Leaf (StringValue "Query")) ])
                , ("mutationType", object [ ("name", Leaf (StringValue "Mutation")) ] )
                , ("subscriptionType", object [ ("name", Leaf (StringValue "Subscription")) ] )
                , ("types", types definitions)
                , ("directives", ArrayNode [])
                ]
            )
        ]

types definitions = ArrayNode (mapMaybe introspectType definitions)
    where
        introspectType TypeSystemDefinition { typeSystemDefinition = TypeDefinition ObjectTypeDefinition { name, fieldDefinitions } } =
            Just $ object
                [ ("kind", Leaf (StringValue "OBJECT"))
                , ("name", Leaf (StringValue name))
                , ("description", Leaf (StringValue ""))
                , ("fields", ArrayNode (map introspectFieldDefinition fieldDefinitions))
                , ("inputFields", Leaf NullValue)
                , ("interfaces", ArrayNode [])
                , ("enumValues", Leaf NullValue)
                , ("possibleTypes", Leaf NullValue)
                ]
        introspectType TypeSystemDefinition { typeSystemDefinition = TypeDefinition InputObjectTypeDefinition { name, fieldDefinitions } } =
            Just $ object
                [ ("kind", Leaf (StringValue "INPUT_OBJECT"))
                , ("name", Leaf (StringValue name))
                , ("description", Leaf (StringValue ""))
                , ("fields", Leaf NullValue)
                , ("inputFields", ArrayNode (map introspectInputFieldDefinition fieldDefinitions))
                , ("interfaces", Leaf NullValue)
                , ("enumValues", Leaf NullValue)
                , ("possibleTypes", Leaf NullValue)
                ]
        introspectType _ = Nothing

introspectInputFieldDefinition FieldDefinition { description, name, argumentsDefinition, type_ } =
    object
        [ ("name", Leaf (StringValue name))
        , ("description", Leaf (maybe NullValue StringValue description))
        , ("type", introspectType type_)
        , ("defaultValue", Leaf NullValue)
        ]

introspectFieldDefinition FieldDefinition { description, name, argumentsDefinition, type_ } =
    object
        [ ("name", Leaf (StringValue name))
        , ("description", Leaf (maybe NullValue StringValue description))
        , ("args", ArrayNode (map introspectArgumentDefinition argumentsDefinition ))
        , ("type", introspectType type_)
        , ("isDeprecated", Leaf NullValue)
        , ("deprecationReason", Leaf NullValue)
        ]

introspectArgumentDefinition ArgumentDefinition { name, argumentType, defaultValue } =
    object
        [ ("name", Leaf (StringValue name))
        , ("description", Leaf NullValue)
        , ("type", introspectType argumentType)
        , ("defaultValue", Leaf (fromMaybe NullValue defaultValue))
        ]

introspectType :: Type -> StaticGraph
introspectType (NamedType name) =
    object
    [ ("kind", Leaf (StringValue "OBJECT"))
    , ("name", Leaf (StringValue name))
    , ("ofType", Leaf NullValue)
    ]
introspectType (ListType inner) =
    object
    [ ("kind", Leaf (StringValue "LIST"))
    , ("name", Leaf NullValue)
    , ("ofType", introspectType inner)
    ]
introspectType (NonNullType inner) =
    object
    [ ("kind", Leaf (StringValue "NON_NULL"))
    , ("name", Leaf NullValue)
    , ("ofType", introspectType inner)
    ]

object values = ObjectNode (HashMap.fromList values)