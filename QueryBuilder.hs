{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances #-}

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
import Foundation.ModelSupport (ModelFieldValue, GetTableName)
import Model.Generated.Types
import qualified Foundation.ModelSupport
import Foundation.NameSupport (fieldNameToColumnName)
queryBuilder :: forall model. QueryBuilder model
queryBuilder = NewQueryBuilder

data QueryBuilder model where
    NewQueryBuilder :: QueryBuilder model
    FilterByQueryBuilder :: KnownSymbol field => (Proxy field, Action) -> QueryBuilder model -> QueryBuilder model
    OrderByQueryBuilder :: KnownSymbol field => (Proxy field, OrderByDirection) -> QueryBuilder model -> QueryBuilder model

deriving instance Show (QueryBuilder a)

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: Text,
        whereConditions :: [(Text, Action)],
        orderByClause :: Maybe (Text, OrderByDirection)
    }

buildQuery :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> SQLQuery
buildQuery queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolVal @(GetTableName model) Proxy
            in SQLQuery { selectFrom = cs tableName, whereConditions = [], orderByClause = Nothing }
        FilterByQueryBuilder (fieldProxy, value) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { whereConditions = (whereConditions query) <> [((fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " = ?", value)] }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = (buildQuery queryBuilder)
            in query { orderByClause = Just (fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection) }
        otherwise -> error $ show otherwise

fetch :: (?modelContext :: Foundation.ModelSupport.ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO [model]
fetch queryBuilder =
    let (theQuery, theParameters) = toSQL queryBuilder
    in Foundation.ModelSupport.query (Query $ cs theQuery) theParameters

toSQL :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> (Text, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
toSQL' SQLQuery { selectFrom, whereConditions, orderByClause } =
        ("SELECT * FROM " <> selectFrom <> whereConditions' <> " " <> orderByClause', map snd whereConditions)
    where
        whereConditions' = if length whereConditions == 0 then mempty else " WHERE " <> intercalate " AND " (map fst whereConditions)
        orderByClause' =
            case orderByClause of
                Just (column, direction) -> " ORDER BY " <> column <> (if direction == Desc then " DESC" else mempty)
                Nothing -> mempty

instance forall name model f value. (KnownSymbol name) => IsLabel name ((Proxy OrderByTag -> QueryBuilder model -> OrderByDirection -> QueryBuilder model)) where
    fromLabel _ queryBuilder orderByDirection = OrderByQueryBuilder (Proxy @name, orderByDirection) queryBuilder

instance forall name model f value. (KnownSymbol name, ToField (ModelFieldValue model name), f ~ (Proxy FilterWhereTag -> QueryBuilder model -> value -> QueryBuilder model), value ~ ModelFieldValue model name) => IsLabel name f where
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

infixl 9 `isEq`
a `isEq` b = filterWhere (a, b)
