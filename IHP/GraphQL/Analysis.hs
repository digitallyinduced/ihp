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

type TableName = Text

-- | Returns the database tables used by a GraphQL query
tablesUsedInDocument :: Document -> Set TableName
tablesUsedInDocument Document { definitions } = mconcat (map tablesUsedInDefinition definitions)
    where
        tablesUsedInDefinition :: Definition -> Set Text        
        tablesUsedInDefinition ExecutableDefinition { operation } = tablesUsedInOperation operation
    
        tablesUsedInOperation :: OperationDefinition -> Set Text        
        tablesUsedInOperation OperationDefinition { selectionSet } = tablesUsedInSelectionSet selectionSet

        tablesUsedInSelectionSet :: [Selection] -> Set Text
        tablesUsedInSelectionSet selectionSet = mconcat (map tablesUsedInSelection selectionSet)

        tablesUsedInSelection :: Selection -> Set Text
        tablesUsedInSelection Field { selectionSet = [] } = Set.empty
        tablesUsedInSelection Field { name, selectionSet } = Set.singleton name <> tablesUsedInSelectionSet selectionSet

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
                (HashMap.singleton name selectionIds):(map (recordIdsInSelection selectedResult) childNodes)
            where
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

                selectionIds :: Set UUID
                selectionIds = case selectedResult of
                    Aeson.Array vector ->
                        vector
                        |> Vector.toList
                        |> map (\case
                                Aeson.Object record -> extractId record
                                otherwise -> error ("selectionIds: unexpected " <> tshow selectedResult)
                            )
                        |> Set.fromList
                    _ -> error "Expected an array here"

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
            case HashMap.lookup "id" hashMap of
                Just (Aeson.String idString) ->
                    case UUID.fromText idString of
                        Just uuid -> if uuid == id
                            then Just record
                            else Nothing
                        Nothing -> error "Failed to parse UUID"
                otherwise ->
                    hashMap
                    |> HashMap.elems
                    |> mapMaybe (extractRecordById id)
                    |> headMay
        Aeson.Array vector ->
            vector
            |> Vector.toList
            |> mapMaybe (extractRecordById id)
            |> headMay

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