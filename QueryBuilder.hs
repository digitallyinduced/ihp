{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances #-}

module Foundation.QueryBuilder (query, findManyBy, findById, findMaybeBy, filterWhere, fetch, fetchOne, QueryBuilder, findBy, In (In), orderBy, queryUnion, queryOr) where

import Foundation.HaskellSupport
import ClassyPrelude hiding (UTCTime, find)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
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

data FilterOperator = EqOp | InOp | IsOp deriving (Show, Eq)


compileOperator _ EqOp = "="
compileOperator _ InOp = "IN"
compileOperator _ IsOp = "IS"

data QueryBuilder model where
    NewQueryBuilder :: QueryBuilder model
    FilterByQueryBuilder :: (KnownSymbol field) => (Proxy field, FilterOperator, Action) -> QueryBuilder model -> QueryBuilder model
    OrderByQueryBuilder :: KnownSymbol field => (Proxy field, OrderByDirection) -> QueryBuilder model -> QueryBuilder model
    IncludeQueryBuilder :: KnownSymbol field => (Proxy field) -> QueryBuilder model -> QueryBuilder model
    UnionQueryBuilder :: QueryBuilder model -> QueryBuilder model -> QueryBuilder model

data Condition = VarCondition Text Action | OrCondition Condition Condition | AndCondition Condition Condition deriving (Show)

deriving instance Show (QueryBuilder a)

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: Text,
        whereCondition :: Maybe Condition,
        orderByClause :: Maybe (Text, OrderByDirection),
        limitClause :: Maybe Text,
        includes :: [Text]
    }

buildQuery :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> SQLQuery
buildQuery queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolVal @(GetTableName model) Proxy
            in SQLQuery { selectFrom = cs tableName, whereCondition = Nothing, orderByClause = Nothing, limitClause = Nothing, includes = [] }
        FilterByQueryBuilder (fieldProxy, operator, value) queryBuilder ->
            let
                query = (buildQuery queryBuilder)
                condition = VarCondition ((fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " " <> compileOperator fieldProxy operator <> " ?") value
            in
                query { whereCondition = Just $ case whereCondition query of Just c -> AndCondition c condition; Nothing -> condition }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { orderByClause = Just (fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection) }
        IncludeQueryBuilder (fieldProxy) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { includes = (includes query) <> [fieldNameToColumnName . cs $ symbolVal fieldProxy] }
        UnionQueryBuilder firstQueryBuilder secondQueryBuilder ->
            let
                firstQuery = buildQuery firstQueryBuilder
                secondQuery = buildQuery secondQueryBuilder
                isSimpleQuery query = orderByClause query == Nothing && limitClause query == Nothing
                isSimpleUnion = isSimpleQuery firstQuery && isSimpleQuery secondQuery
                unionWhere =
                    case (whereCondition firstQuery, whereCondition secondQuery) of
                        (Nothing, whereCondition) -> whereCondition
                        (whereCondition, Nothing) -> whereCondition
                        (Just firstWhere, Just secondWhere) -> Just $ OrCondition firstWhere secondWhere
                        (Nothing, Nothing) -> Nothing
            in
                if isSimpleUnion then
                    firstQuery { whereCondition = unionWhere }
                else
                    error "buildQuery: Union of complex queries not supported yet"

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
toSQL' sqlQuery@SQLQuery { selectFrom, orderByClause, limitClause } =
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
        theParams =
            case whereCondition sqlQuery of
                Just condition -> compileConditionArgs condition
                Nothing -> mempty
        toQualifiedName unqualifiedName = selectFrom <> "." <> unqualifiedName
        whereConditions' =
            case whereCondition sqlQuery of
                Just condition -> " WHERE " <> compileConditionQuery condition
                Nothing -> mempty
        orderByClause' =
            case orderByClause of
                Just (column, direction) -> " ORDER BY " <> column <> (if direction == Desc then " DESC" else mempty)
                Nothing -> mempty
        limitClause' = fromMaybe "" limitClause

compileConditionQuery :: Condition -> Text
compileConditionQuery (VarCondition var _) =  var
compileConditionQuery (OrCondition a b) =  "(" <> compileConditionQuery a <> ") OR (" <> compileConditionQuery b <> ")"
compileConditionQuery (AndCondition a b) =  "(" <> compileConditionQuery a <> ") AND (" <> compileConditionQuery b <> ")"

compileConditionArgs :: Condition -> [Action]
compileConditionArgs (VarCondition _ arg) = [arg]
compileConditionArgs (OrCondition a b) = compileConditionArgs a <> compileConditionArgs b
compileConditionArgs (AndCondition a b) = compileConditionArgs a <> compileConditionArgs b

instance {-# OVERLAPS #-} forall name model f value. (KnownSymbol name) => IsLabel name ((Proxy OrderByTag -> QueryBuilder model -> OrderByDirection -> QueryBuilder model)) where
    fromLabel _ queryBuilder orderByDirection = OrderByQueryBuilder (Proxy @name, orderByDirection) queryBuilder

instance {-# OVERLAPS #-} forall name model f value. (KnownSymbol name) => IsLabel name ((Proxy IncludeTag -> QueryBuilder model -> QueryBuilder model)) where
    fromLabel _ queryBuilder = IncludeQueryBuilder (Proxy @name) queryBuilder

instance {-# OVERLAPS #-} forall name model value. (KnownSymbol name, ToField (ModelFieldValue model name), value ~ ModelFieldValue model name) => IsLabel name (Proxy FilterWhereTag -> QueryBuilder model -> [value] -> QueryBuilder model) where
    fromLabel _ queryBuilder value = FilterByQueryBuilder (Proxy @name, InOp, toField (In value)) queryBuilder

instance forall name model value. (KnownSymbol name, ToField (ModelFieldValue model name), value ~ ModelFieldValue model name, EqOrIsOperator value) => IsLabel name (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model) where
    fromLabel _ queryBuilder value = FilterByQueryBuilder (Proxy @name, toEqOrIsOperator value, toField value) queryBuilder

-- Helper to deal with `some_field IS NULL` vs `some_field = 'some value'`
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp



--filterBy :: forall model value fieldName field. (IsLabel (fieldName :: Symbol) field, value ~ ModelFieldValue model fieldName, ToField (ModelFieldValue model fieldName)) => (field, value) -> QueryBuilder model -> QueryBuilder model
--filterBy :: forall name value model symbol. ((QueryBuilder model -> Proxy (ModelFieldValue model name) -> QueryBuilder model), ModelFieldValue model name) -> QueryBuilder model -> QueryBuilder model
  -- FilterByQueryBuilder (Just criteria) queryBuilder
--filterBy :: forall model name value makeCriteria. (value ~ ModelFieldValue model name, makeCriteria ~ (QueryBuilder model -> value -> QueryBuilder model)) => (makeCriteria, value) -> QueryBuilder model -> QueryBuilder model

data FilterWhereTag
filterWhere :: (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model, value) -> QueryBuilder model -> QueryBuilder model
filterWhere (makeCriteria, value) queryBuilder = makeCriteria (Proxy @FilterWhereTag) queryBuilder value

data OrderByTag
orderBy :: (Proxy OrderByTag -> QueryBuilder model -> OrderByDirection -> QueryBuilder model) -> QueryBuilder model -> QueryBuilder model
orderBy makeOrderBy queryBuilder = makeOrderBy (Proxy @OrderByTag) queryBuilder Asc

data IncludeTag
include :: (Proxy IncludeTag -> QueryBuilder model -> QueryBuilder model) -> QueryBuilder model -> QueryBuilder model
include makeInclude queryBuilder = makeInclude (Proxy @IncludeTag) queryBuilder

infixl 9 `isEq`
a `isEq` b = filterWhere (a, b)


findBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model) -> value -> QueryBuilder model -> IO model
findBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

findMaybeBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model) -> value -> QueryBuilder model -> IO (Maybe model)
findMaybeBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

findById :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), ToField (ModelFieldValue model "id")) => ModelFieldValue model "id" -> QueryBuilder model -> IO model
findById value queryBuilder = queryBuilder |> filterWhere (#id, value) |> fetchOne

findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model)) => (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model) -> value -> QueryBuilder model -> IO [model]
findManyBy field value queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #workflowId id

queryUnion :: QueryBuilder model -> QueryBuilder model -> QueryBuilder model
queryUnion = UnionQueryBuilder

queryOr a b queryBuilder = (a queryBuilder) `UnionQueryBuilder` (b queryBuilder)
