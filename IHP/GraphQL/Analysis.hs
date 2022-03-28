module IHP.GraphQL.Analysis where

import IHP.Prelude
import IHP.GraphQL.Types

import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson
import Data.Aeson ((.:))
import qualified Data.Vector as Vector
import qualified Data.UUID as UUID
import qualified Data.List as List
import qualified Data.Text as Text

type TableName = Text

-- | Returns the database tables used by a GraphQL query
tablesUsedInDocument :: Document -> Set TableName
tablesUsedInDocument Document { definitions } = mconcat (map tablesUsedInDefinition definitions)
    where
        tablesUsedInDefinition :: Definition -> Set Text        
        tablesUsedInDefinition ExecutableDefinition { operation } = tablesUsedInOperation operation
    
        tablesUsedInOperation :: OperationDefinition -> Set Text        
        tablesUsedInOperation OperationDefinition { selectionSet, operationType } = tablesUsedInSelectionSet operationType selectionSet

        tablesUsedInSelectionSet :: OperationType -> [Selection] -> Set Text
        tablesUsedInSelectionSet operationType selectionSet = mconcat (map (tablesUsedInSelection operationType) selectionSet)

        tablesUsedInSelection :: OperationType -> Selection -> Set Text
        tablesUsedInSelection _ Field { selectionSet = [] } = Set.empty
        tablesUsedInSelection operationType Field { name, selectionSet, arguments } = Set.singleton normalizedName <> tablesUsedInSelectionSet Query selectionSet
            where
                -- `createTask` => tasks
                -- `deleteTask` => tasks
                -- `updateTask` => tasks
                normalizedName = case operationType of
                    Mutation ->
                        case Text.stripPrefix "create" name of
                            Just suffix -> modelNameToTableName suffix
                            Nothing -> case Text.stripPrefix "delete" name of
                                Just suffix -> modelNameToTableName suffix
                                Nothing -> case Text.stripPrefix "update" name of
                                    Just suffix -> modelNameToTableName suffix
                                    Nothing -> name
                    _ -> case arguments of
                        -- `project(id: $projectId)` => projects
                        [Argument { argumentName = "id", argumentValue }] -> pluralize name
                        otherwise -> name


recordIds :: Document -> Aeson.Value -> HashMap TableName (Set UUID)
recordIds Document { definitions } result = mconcat (map recordIdsInDefinition definitions)
    where
        recordIdsInDefinition :: Definition -> HashMap TableName (Set UUID)
        recordIdsInDefinition ExecutableDefinition { operation } = recordIdsInOperation operation
    
        recordIdsInOperation :: OperationDefinition -> HashMap TableName (Set UUID)
        recordIdsInOperation OperationDefinition { selectionSet } = recordIdsInSelectionSet selectionSet result

        recordIdsInSelectionSet :: [Selection] -> Aeson.Value -> HashMap TableName (Set UUID)
        recordIdsInSelectionSet selectionSet result = mconcat (map (recordIdsInSelection result) selectionSet)

        recordIdsInSelection :: Aeson.Value -> Selection -> HashMap TableName (Set UUID)
        recordIdsInSelection result Field { selectionSet = [] } = HashMap.empty
        recordIdsInSelection result Field { name, alias, selectionSet } = mconcat $
                (HashMap.singleton tableName selectionIds):(map (recordIdsInSelection selectedResult) childNodes)
            where
                (selectionIds, tableName) = selectionIdsAndName

                aliasOrName :: Text
                aliasOrName = fromMaybe name alias

                childNodes = selectionSet
                    |> filter selectionIsNode

                selectedResult :: Aeson.Value
                selectedResult = case result of
                        Aeson.Object hashMap -> hashMap
                                |> HashMap.lookup aliasOrName
                                |> \case
                                    Just result -> result
                                    Nothing -> error ("Could not find " <> tshow aliasOrName <> " in result set")
                        Aeson.Array vector -> vector
                                |> Vector.toList
                                |> map (\case
                                    Aeson.Object hashMap -> hashMap
                                        |> HashMap.lookup aliasOrName
                                        |> \case
                                            Just (Aeson.Array result) -> result
                                            Nothing -> error ("Could not find " <> tshow aliasOrName <> " in result set")
                                    otherwise -> error ("selectedResult -> array: Object expxected")
                                )
                                |> map Vector.toList
                                |> concat
                                |> Vector.fromList
                                |> Aeson.Array
                        otherwise -> error ("selectedResult at " <> name <> ": Expected an object here, got: " <> tshow otherwise)

                selectionIdsAndName :: (Set UUID, Text)
                selectionIdsAndName = case selectedResult of
                    Aeson.Array vector ->
                        vector
                        |> Vector.toList
                        |> map (\case
                                Aeson.Object record -> extractId record
                                otherwise -> error ("selectionIds: unexpected " <> tshow selectedResult)
                            )
                        |> Set.fromList
                        |> \ids -> (ids, name)
                    Aeson.Object hashMap -> (Set.singleton (extractId hashMap), pluralize name)
                    _ -> error "unexpected object here"

                extractId :: HashMap Text Aeson.Value -> UUID
                extractId record = record
                        |> HashMap.lookup "id"
                        |> \case
                            Just (Aeson.String string) ->
                                case UUID.fromText string of
                                    Just uuid -> uuid
                                    Nothing -> error "Failed to parse UUID"
                            Just otherwise -> error "Expected 'id' field to be a string"
                            Nothing -> error "Could not find 'id' field for record"

selectionIsNode :: Selection -> Bool
selectionIsNode Field { selectionSet = [] } = False
selectionIsNode otherwise                   = True

extractRecordById :: UUID -> Aeson.Value -> Maybe Aeson.Value
extractRecordById id result =
    case result of
        record@(Aeson.Object hashMap) ->
            let traverseObjectKeys =
                    hashMap
                    |> HashMap.elems
                    |> mapMaybe (extractRecordById id)
                    |> headMay
            in case HashMap.lookup "id" hashMap of
                Just (Aeson.String idString) ->
                    case UUID.fromText idString of
                        Just uuid -> if uuid == id
                            then Just record
                            else traverseObjectKeys
                        Nothing -> error "Failed to parse UUID"
                otherwise -> traverseObjectKeys
        Aeson.Array vector ->
            vector
            |> Vector.toList
            |> mapMaybe (extractRecordById id)
            |> headMay
        otherwise -> Nothing

isSubscriptionDocument :: Document -> Bool
isSubscriptionDocument Document { definitions } = foldl' (&&) True (map isSubscriptionDefinition definitions)
    where
        isSubscriptionDefinition ExecutableDefinition { operation = OperationDefinition { operationType } } = operationType == Subscription

newtype Path = Path [Text]
    deriving (Eq, Show)

nodePathsForTable :: Text -> Document -> [Path]
nodePathsForTable tableName Document { definitions } = reversePath <$> mconcat (map nodePathsForTableDefinition definitions)
    where
        -- e.g. "users" or "userProjects"
        targetSelectionName = lcfirst $ tableNameToControllerName tableName

        reversePath :: Path -> Path
        reversePath (Path path) = (Path (reverse path))

        nodePathsForTableDefinition :: Definition -> [Path]
        nodePathsForTableDefinition ExecutableDefinition { operation } = nodePathsForTableOperation operation
    
        nodePathsForTableOperation :: OperationDefinition -> [Path]
        nodePathsForTableOperation OperationDefinition { selectionSet } = nodePathsForTableSelectionSet [] selectionSet

        nodePathsForTableSelectionSet :: [Text] -> [Selection] -> [Path]
        nodePathsForTableSelectionSet path selectionSet = mconcat (map (nodePathsForTableSelection path) selectionSet)

        nodePathsForTableSelection :: [Text] -> Selection -> [Path]
        nodePathsForTableSelection path Field { selectionSet = [] } = []
        nodePathsForTableSelection path Field { name, alias, selectionSet } =
            let
                nameOrAlias = (fromMaybe name alias)
                cur = Path (nameOrAlias:path)
                rec = nodePathsForTableSelectionSet (nameOrAlias:path) selectionSet
            in
                if name == targetSelectionName
                    then cur:rec
                    else rec

applyFunctionAtNode :: (Aeson.Value -> Aeson.Value) -> Path -> Aeson.Value -> Aeson.Value
applyFunctionAtNode function (Path path) json = applyFunctionAtNode' function path json
    where
        applyFunctionAtNode' function [] value = function value
        applyFunctionAtNode' function (curPath:restPath) (Aeson.Object hashMap) = Aeson.Object (HashMap.adjust (applyFunctionAtNode' function restPath) curPath hashMap)
        applyFunctionAtNode' function path (Aeson.Array vector) = Aeson.Array (Vector.map (applyFunctionAtNode' function path) vector)

documentIsExecutable :: Document -> Bool
documentIsExecutable Document { definitions } = isJust (find isExecutableDefinition definitions)

isExecutableDefinition :: Definition -> Bool
isExecutableDefinition ExecutableDefinition {} = True
isExecutableDefinition _ = False

splitDocumentIntoResolvableUnits :: Document -> [(Resolver, Document)]
splitDocumentIntoResolvableUnits Document { definitions } = removeEmptyResolvers $ split [] [] definitions
    where
        isPostgresSelection Field { name = "__schema" } = False
        isPostgresSelection otherwise                   = True

        removeEmptyResolvers :: [(Resolver, Document)] -> [(Resolver, Document)]
        removeEmptyResolvers documentsWithResolver = filter (\(resolver, document) -> documentIsExecutable document) documentsWithResolver

        split :: [Definition] -> [Definition] -> [Definition] -> [(Resolver, Document)]
        split postgresDefinitions introspectionDefinitions (ed@(ExecutableDefinition { operation = od@(OperationDefinition { selectionSet }) }):rest) =
            case List.partition isPostgresSelection selectionSet of
                (postgresSelection, []) -> split (postgresDefinitions <> [ed]) introspectionDefinitions rest
                ([], introspectionSelection) -> split postgresDefinitions (introspectionDefinitions <> [ed]) rest
                (postgresSelection, introspectionSelection) -> split (postgresDefinitions <> [ ExecutableDefinition { operation = od { selectionSet = postgresSelection } } ]) (introspectionDefinitions <> [ ExecutableDefinition { operation = od { selectionSet = introspectionSelection } } ]) rest
        split postgresDefinitions introspectionDefinitions (x:xs) =
            if isExecutableDefinition x
                then split (postgresDefinitions <> [x]) introspectionDefinitions xs
                else split (postgresDefinitions <> [x]) (introspectionDefinitions <> [x]) xs -- E.g. fragments need to be in both queries
        split postgresDefinitions introspectionDefinitions [] = [(PostgresResolver, Document postgresDefinitions), (IntrospectionResolver, Document introspectionDefinitions)]