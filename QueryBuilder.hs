{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes #-}

module Foundation.QueryBuilder where

import Foundation.HaskellSupport
import ClassyPrelude hiding (UTCTime, find)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs)
import Data.Time.Clock (UTCTime)
import Unsafe.Coerce
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.Records
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Foundation.ModelSupport (ModelFieldValue, GetTableName, ModelContext, GetModelById, NewTypeWrappedUUID)
import qualified Foundation.ModelSupport
import Foundation.NameSupport (fieldNameToColumnName)

query :: forall model. QueryBuilder model
query = NewQueryBuilder

data QueryBuilder model where
    NewQueryBuilder :: QueryBuilder model
    FilterByQueryBuilder :: KnownSymbol field => (Proxy field, Action) -> QueryBuilder model -> QueryBuilder model
    OrderByQueryBuilder :: KnownSymbol field => (Proxy field, OrderByDirection) -> QueryBuilder model -> QueryBuilder model
    IncludeQueryBuilder :: KnownSymbol field => (Proxy field) -> QueryBuilder model -> QueryBuilder model

deriving instance Show (QueryBuilder a)

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: Text,
        whereConditions :: [(Text, Action)],
        orderByClause :: Maybe (Text, OrderByDirection),
        limitClause :: Maybe Text,
        includes :: [Text]
    }

buildQuery :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> SQLQuery
buildQuery queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolVal @(GetTableName model) Proxy
            in SQLQuery { selectFrom = cs tableName, whereConditions = [], orderByClause = Nothing, limitClause = Nothing, includes = [] }
        FilterByQueryBuilder (fieldProxy, value) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { whereConditions = (whereConditions query) <> [((fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " = ?", value)] }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { orderByClause = Just (fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection) }
        IncludeQueryBuilder (fieldProxy) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { includes = (includes query) <> [fieldNameToColumnName . cs $ symbolVal fieldProxy] }
        otherwise -> error $ show otherwise

fetch :: (?modelContext :: Foundation.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO [model]
fetch queryBuilder = do
    let (theQuery, theParameters) = toSQL' (buildQuery queryBuilder)
    putStrLn $ tshow (theQuery, theParameters)
    Foundation.ModelSupport.query (Query $ cs theQuery) theParameters

fetchOneOrNothing :: (?modelContext :: Foundation.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO (Maybe model)
fetchOneOrNothing queryBuilder = do
    let (theQuery, theParameters) = toSQL' (buildQuery queryBuilder) { limitClause = Just "LIMIT 1"}
    putStrLn $ tshow (theQuery, theParameters)
    results <- Foundation.ModelSupport.query (Query $ cs theQuery) theParameters
    return $ listToMaybe results

fetchOne :: (?modelContext :: Foundation.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO model
fetchOne queryBuilder = do
    maybeModel <- fetchOneOrNothing queryBuilder
    return $ case maybeModel of
        Just model -> model
        Nothing -> error "Cannot find model"

toSQL :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> (Text, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
toSQL' sqlQuery@SQLQuery { selectFrom, whereConditions, orderByClause, limitClause } =
        (theQuery, theParams)
    where
        theQuery =
            "SELECT " <> selectors <> " FROM "
            <> fromClause
            <> whereConditions' <> " "
            <> orderByClause' <> " "
            <> limitClause'

        commaSep = intercalate ", "
        selectors :: Text
        selectors = commaSep (rootTableSelector:joinedTableSelectors)
            where
                rootTableSelector :: Text
                rootTableSelector = selectFrom <> ".*"
                joinedTableSelectors :: [Text]
                joinedTableSelectors = map (\tableName -> tableName <> ".*") (includes sqlQuery)
        fromClause :: Text
        fromClause = commaSep (rootTableFromClause:joinedTableFromClauses)
            where
                rootTableFromClause = selectFrom
                joinedTableFromClauses = includes sqlQuery
        theParams = map snd whereConditions
        toQualifiedName unqualifiedName = selectFrom <> "." <> unqualifiedName
        whereConditions' =
            if length whereConditions == 0
                then mempty
                else " WHERE " <> intercalate " AND " (map toQualifiedName $ map fst whereConditions)
        orderByClause' =
            case orderByClause of
                Just (column, direction) -> " ORDER BY " <> column <> (if direction == Desc then " DESC" else mempty)
                Nothing -> mempty
        limitClause' = fromMaybe "" limitClause

instance {-# OVERLAPS #-} forall name model f value. (KnownSymbol name) => IsLabel name ((Proxy OrderByTag -> QueryBuilder model -> OrderByDirection -> QueryBuilder model)) where
    fromLabel _ queryBuilder orderByDirection = OrderByQueryBuilder (Proxy @name, orderByDirection) queryBuilder

instance {-# OVERLAPS #-} forall name model f value. (KnownSymbol name) => IsLabel name ((Proxy IncludeTag -> QueryBuilder model -> QueryBuilder model)) where
    fromLabel _ queryBuilder = IncludeQueryBuilder (Proxy @name) queryBuilder

instance {-# OVERLAPS #-} forall name model f value. (KnownSymbol name, ToField (ModelFieldValue model name), f ~ (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model), value ~ ModelFieldValue model name) => IsLabel name f where
    fromLabel _ queryBuilder value = FilterByQueryBuilder (Proxy @name, toField value) queryBuilder



--filterBy :: forall model value fieldName field. (IsLabel (fieldName :: Symbol) field, value ~ ModelFieldValue model fieldName, ToField (ModelFieldValue model fieldName)) => (field, value) -> QueryBuilder model -> QueryBuilder model
--filterBy :: forall name value model symbol. ((QueryBuilder model -> Proxy (ModelFieldValue model name) -> QueryBuilder model), ModelFieldValue model name) -> QueryBuilder model -> QueryBuilder model
  -- FilterByQueryBuilder (Just criteria) queryBuilder
--filterBy :: forall model name value makeCriteria. (value ~ ModelFieldValue model name, makeCriteria ~ (QueryBuilder model -> value -> QueryBuilder model)) => (makeCriteria, value) -> QueryBuilder model -> QueryBuilder model

data FilterWhereTag
filterWhere (makeCriteria, value) queryBuilder = makeCriteria (Proxy @FilterWhereTag) queryBuilder value

data OrderByTag
orderBy :: (Proxy OrderByTag -> QueryBuilder model -> OrderByDirection -> QueryBuilder model) -> QueryBuilder model -> QueryBuilder model
orderBy makeOrderBy queryBuilder = makeOrderBy (Proxy @OrderByTag) queryBuilder Asc

data IncludeTag
include :: (Proxy IncludeTag -> QueryBuilder model -> QueryBuilder model) -> QueryBuilder model -> QueryBuilder model
include makeInclude queryBuilder = makeInclude (Proxy @IncludeTag) queryBuilder

infixl 9 `isEq`
a `isEq` b = filterWhere (a, b)


findBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> queryBuilder -> value -> QueryBuilder model) -> value -> queryBuilder -> IO model
findBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

findMaybeBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> queryBuilder -> value -> QueryBuilder model) -> value -> queryBuilder -> IO (Maybe model)
findMaybeBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

findById :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), ToField (ModelFieldValue model "id")) => ModelFieldValue model "id" -> QueryBuilder model -> IO model
findById value queryBuilder = queryBuilder |> filterWhere (#id, value) |> fetchOne

findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model) -> value -> QueryBuilder model -> IO [model]
findManyBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #workflowId id
