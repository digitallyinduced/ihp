{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs #-}

module TurboHaskell.QueryBuilder (query, findManyBy, findById, findMaybeBy, filterWhere, QueryBuilder, findBy, In (In), orderBy, orderByDesc, queryUnion, queryOr, DefaultScope (..), filterWhereIn, genericFetchId, genericfetchIdOneOrNothing, genericFetchIdOne, Fetchable (..), include,  genericFetchIds, genericfetchIdsOneOrNothing, genericFetchIdsOne, EqOrIsOperator) where


import Control.Lens hiding ((|>))
import Data.Generics.Product
import GHC.Generics
import TurboHaskell.HaskellSupport
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
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import TurboHaskell.ModelSupport (GetTableName, ModelContext, GetModelById)
import qualified TurboHaskell.ModelSupport
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.NameSupport (fieldNameToColumnName)
import TurboHaskell.ModelSupport (Id, Id')
import qualified TurboHaskell.SchemaTypes as Schema

query :: forall model. DefaultScope model => QueryBuilder model
query = defaultScope NewQueryBuilder

class DefaultScope model where
    defaultScope :: QueryBuilder model -> QueryBuilder model

instance {-# OVERLAPPABLE #-} DefaultScope model where defaultScope queryBuilder = queryBuilder

instance Default (QueryBuilder model) where
    def = NewQueryBuilder

data FilterOperator = EqOp | InOp | IsOp deriving (Show, Eq)


{-# INLINE compileOperator #-}
compileOperator _ EqOp = "="
compileOperator _ InOp = "IN"
compileOperator _ IsOp = "IS"

data QueryBuilder model where
    NewQueryBuilder :: QueryBuilder model
    FilterByQueryBuilder :: (KnownSymbol field) => !(Proxy field, FilterOperator, Action) -> !(QueryBuilder model) -> QueryBuilder model
    OrderByQueryBuilder :: KnownSymbol field => !(Proxy field, OrderByDirection) -> !(QueryBuilder model) -> QueryBuilder model
    IncludeQueryBuilder :: (KnownSymbol field, KnownSymbol (GetTableName model)) => !(Proxy field, QueryBuilder relatedModel) -> !(QueryBuilder model) -> QueryBuilder (TurboHaskell.ModelSupport.Include field model)
    UnionQueryBuilder :: !(QueryBuilder model) -> !(QueryBuilder model) -> QueryBuilder model

data Condition = VarCondition !Text !Action | OrCondition !Condition !Condition | AndCondition !Condition !Condition deriving (Show)

deriving instance Show (QueryBuilder a)

-- This hack is to allow Eq instances for models with hasMany relations
instance Eq (TurboHaskell.QueryBuilder.QueryBuilder model) where a == b = True

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: !(Text),
        whereCondition :: !(Maybe Condition),
        orderByClause :: !(Maybe (Text, OrderByDirection)),
        limitClause :: !(Maybe Text)
    }

{-# INLINE buildQuery #-}
buildQuery :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> SQLQuery
buildQuery !queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolVal @(GetTableName model) Proxy
            in SQLQuery { selectFrom = cs tableName, whereCondition = Nothing, orderByClause = Nothing, limitClause = Nothing }
        FilterByQueryBuilder (fieldProxy, operator, value) queryBuilder ->
            let
                query = (buildQuery queryBuilder)
                condition = VarCondition ((fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " " <> compileOperator fieldProxy operator <> " ?") value
            in
                query { whereCondition = Just $ case whereCondition query of Just c -> AndCondition c condition; Nothing -> condition }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { orderByClause = Just (fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection) }
        IncludeQueryBuilder include queryBuilder -> buildQuery queryBuilder
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
            in
                if isSimpleUnion then
                    firstQuery { whereCondition = unionWhere }
                else
                    error "buildQuery: Union of complex queries not supported yet"


class Fetchable fetchable model | fetchable -> model where
    type FetchResult fetchable model
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext) => fetchable -> IO (FetchResult fetchable model)
    fetchOneOrNothing :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext) => fetchable -> IO (Maybe model)
    fetchOne :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext) => fetchable -> IO model

instance Fetchable (QueryBuilder model) model where
    type FetchResult (QueryBuilder model) model = [model]
    {-# INLINE fetch #-}
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext) => QueryBuilder model -> IO [model]
    fetch !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder)
        putStrLn $! tshow (theQuery, theParameters)
        TurboHaskell.ModelSupport.sqlQuery (Query $ cs theQuery) theParameters

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: TurboHaskell.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO (Maybe model)
    fetchOneOrNothing !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder) { limitClause = Just "LIMIT 1"}
        putStrLn $! tshow (theQuery, theParameters)
        results <- TurboHaskell.ModelSupport.sqlQuery (Query $ cs theQuery) theParameters
        return $ listToMaybe results

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: TurboHaskell.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO model
    fetchOne !queryBuilder = do
        maybeModel <- fetchOneOrNothing queryBuilder
        return $ case maybeModel of
            Just model -> model
            Nothing -> error "Cannot find model"

{-# INLINE genericFetchId #-}
genericFetchId :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => value -> IO [model]
genericFetchId !id = query @model |> filterWhere (#id, id) |> fetch
{-# INLINE genericfetchIdOneOrNothing #-}
genericfetchIdOneOrNothing :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => value -> IO (Maybe model)
genericfetchIdOneOrNothing !id = query @model |> filterWhere (#id, id) |> fetchOneOrNothing
{-# INLINE genericFetchIdOne #-}
genericFetchIdOne :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => value -> IO model
genericFetchIdOne !id = query @model |> filterWhere (#id, id) |> fetchOne

{-# INLINE genericFetchIds #-}
genericFetchIds :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => [value] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIn (#id, ids) |> fetch
{-# INLINE genericfetchIdsOneOrNothing #-}
genericfetchIdsOneOrNothing :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => [value] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIn (#id, ids) |> fetchOneOrNothing
{-# INLINE genericFetchIdsOne #-}
genericFetchIdsOne :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: TurboHaskell.ModelSupport.ModelContext, ToField value, EqOrIsOperator value, HasField' "id" model value) => [value] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIn (#id, ids) |> fetchOne

toSQL :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> (Text, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
toSQL' !sqlQuery@SQLQuery { selectFrom, orderByClause, limitClause } =
        (theQuery, theParams)
    where
        !theQuery =
            "SELECT " <> selectors <> " FROM "
            <> fromClause
            <> whereConditions' <> " "
            <> orderByClause' <> " "
            <> limitClause'

        selectors :: Text
        selectors = selectFrom <> ".*"
        fromClause :: Text
        fromClause = selectFrom
        !theParams =
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

{-# INLINE compileConditionQuery #-}
compileConditionQuery :: Condition -> Text
compileConditionQuery (VarCondition var _) =  var
compileConditionQuery (OrCondition a b) =  "(" <> compileConditionQuery a <> ") OR (" <> compileConditionQuery b <> ")"
compileConditionQuery (AndCondition a b) =  "(" <> compileConditionQuery a <> ") AND (" <> compileConditionQuery b <> ")"

{-# INLINE compileConditionArgs #-}
compileConditionArgs :: Condition -> [Action]
compileConditionArgs (VarCondition _ arg) = [arg]
compileConditionArgs (OrCondition a b) = compileConditionArgs a <> compileConditionArgs b
compileConditionArgs (AndCondition a b) = compileConditionArgs a <> compileConditionArgs b

-- Helper to deal with `some_field IS NULL` vs `some_field = 'some value'`
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp

{-# INLINE filterWhere #-}
filterWhere :: forall name model value. (KnownSymbol name, ToField value, HasField' name model value, EqOrIsOperator value) => (Proxy name, value) -> QueryBuilder model -> QueryBuilder model
filterWhere !(name, value) = FilterByQueryBuilder (name, toEqOrIsOperator value, toField value)


{-# INLINE filterWhereIn #-}
filterWhereIn :: forall name model value. (KnownSymbol name, ToField value, HasField' name model value) => (Proxy name, [value]) -> QueryBuilder model -> QueryBuilder model
filterWhereIn !(name, value) = FilterByQueryBuilder (name, InOp, toField $ In value)

data FilterWhereTag

data OrderByTag

{-# INLINE orderBy #-}
orderBy :: KnownSymbol name => Proxy name -> QueryBuilder model -> QueryBuilder model
orderBy !name = OrderByQueryBuilder (name, Asc)

{-# INLINE orderByDesc #-}
orderByDesc :: KnownSymbol name => Proxy name -> QueryBuilder model -> QueryBuilder model
orderByDesc !name = OrderByQueryBuilder (name, Desc)

data IncludeTag
include :: forall name model fieldType relatedModel. (KnownSymbol name, KnownSymbol (GetTableName model), HasField' name model fieldType, relatedModel ~ GetModelById fieldType) => KnownSymbol name => Proxy name -> QueryBuilder model -> QueryBuilder (TurboHaskell.ModelSupport.Include name model)
include !name = IncludeQueryBuilder (name, relatedQueryBuilder)
    where
        relatedQueryBuilder = query @relatedModel

{-# INLINE findBy #-}
findBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

{-# INLINE findMaybeBy #-}
findMaybeBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

--findById :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), HasField "id" value model, ToField value) => value -> QueryBuilder model -> IO model
--findById :: (?modelContext :: ModelContext, PG.FromRow model, ToField value, KnownSymbol (GetTableName model), HasField "id" value model) => value -> QueryBuilder model -> IO model
{-# INLINE findById #-}
findById !value !queryBuilder = queryBuilder |> filterWhere (Proxy @"id", value) |> fetchOne

--findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), KnownSymbol name, ToField value, HasField name value model) => Proxy name -> value -> QueryBuilder model -> IO [model]
{-# INLINE findManyBy #-}
findManyBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #templateId id

{-# INLINE queryUnion #-}
queryUnion :: QueryBuilder model -> QueryBuilder model -> QueryBuilder model
queryUnion = UnionQueryBuilder

{-# INLINE queryOr #-}
queryOr :: (qb ~ QueryBuilder model) => (qb -> qb) -> (qb -> qb) -> qb -> qb
queryOr a b queryBuilder = (a queryBuilder) `UnionQueryBuilder` (b queryBuilder)

instance (model ~ GetModelById (Id' model'), HasField' "id" model id, id ~ Id' model') => Fetchable (Id' model') model where
    type FetchResult (Id' model') model = model
    fetch = genericFetchIdOne
    fetchOneOrNothing = genericfetchIdOneOrNothing
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' model'), HasField' "id" model id, id ~ Id' model') => Fetchable (Maybe (Id' model')) model where
    type FetchResult (Maybe (Id' model')) model = [model]
    fetch (Just a) = genericFetchId a
    fetchOneOrNothing Nothing = return Nothing
    fetchOneOrNothing (Just a) = genericfetchIdOneOrNothing a
    fetchOne (Just a) = genericFetchIdOne a

instance (model ~ GetModelById (Id' model'), value ~ Id' model', HasField' "id" model value) => Fetchable [Id' model'] model where
    type FetchResult [Id' model'] model = [model]
    fetch = genericFetchIds
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    fetchOne = genericFetchIdsOne