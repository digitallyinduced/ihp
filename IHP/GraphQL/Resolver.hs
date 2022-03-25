module IHP.GraphQL.Resolver where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified IHP.GraphQL.Introspection as Introspection
import qualified IHP.GraphQL.Analysis as Analysis
import qualified IHP.GraphQL.Compiler as Compiler
import IHP.DataSync.DynamicQuery (UndecodedJSON (UndecodedJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Database.PostgreSQL.Simple as PG

resolve schema sqlQueryWithRLS graphQLRequest = do
    let rootQuery = get #query graphQLRequest
    let variables = get #variables graphQLRequest

    rootQuery
        |> Analysis.splitDocumentIntoResolvableUnits
        |> \case
            -- Avoid decoding the JSON in the common fast-path with a single resolver
            [(PostgresResolver, document)] -> resolvePostgres sqlQueryWithRLS variables document
            multipleResolvers -> do
                results <- forM multipleResolvers \(resolver, document) -> do
                    case resolver of
                        PostgresResolver      -> undecodedJSONToAesonValue <$> (resolvePostgres sqlQueryWithRLS variables document)
                        IntrospectionResolver -> pure $ staticGraphToAesonValue (resolveIntrospection schema document)
                let mergedResult = (foldl1 mergeAeson results)
                pure $ UndecodedJSON (cs $ Aeson.encode mergedResult)

resolvePostgres sqlQueryWithRLS variables document = do
    let [(theQuery, theParams)] = Compiler.compileDocument variables document
    result <- sqlQueryWithRLS theQuery theParams
    case result of
        [PG.Only graphQLResult] -> pure graphQLResult
        otherwise -> error "resolvePostgres: Unexpected result"

resolveIntrospection schema document = Introspection.resolveStaticGraph (Introspection.introspectionGraph schema) document

undecodedJSONToAesonValue :: UndecodedJSON -> Aeson.Value
undecodedJSONToAesonValue (UndecodedJSON json) = case Aeson.decode (cs json) of
        Just result -> result
        Nothing -> error "undecodedJSONToAesonValue: Failed to decode postgres result"

staticGraphToAesonValue :: StaticGraph -> Aeson.Value
staticGraphToAesonValue ObjectNode { objectValues } = Aeson.Object (HashMap.map staticGraphToAesonValue objectValues)
staticGraphToAesonValue ArrayNode { arrayElements } = Aeson.Array (Vector.fromList $ map staticGraphToAesonValue arrayElements)
staticGraphToAesonValue Leaf { value } = valueToAeson value
    where
        valueToAeson (StringValue string) = Aeson.toJSON string
        valueToAeson (BooleanValue boolean) = Aeson.toJSON boolean
        valueToAeson NullValue = Aeson.Null

mergeAeson (Aeson.Object a) (Aeson.Object b) = Aeson.Object (HashMap.union b a)