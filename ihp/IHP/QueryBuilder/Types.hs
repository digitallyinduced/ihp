{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Types
Description: Core data types for the QueryBuilder
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.QueryBuilder.Types
( -- * Core Types
  QueryBuilder (..)
, SQLQuery (..)
, Condition (..)
, Join (..)
, OrderByClause (..)
, OrderByDirection (..)
, FilterOperator (..)
, MatchSensitivity (..)
  -- * Type-level Join Tracking
, NoJoins
, EmptyModelList
, ConsModelList
, ModelList
, IsJoined
  -- * QueryBuilder Wrappers
, JoinQueryBuilderWrapper (..)
, NoJoinQueryBuilderWrapper (..)
, LabeledQueryBuilderWrapper (..)
  -- * Type Classes
, HasQueryBuilder (..)
, DefaultScope (..)
, EqOrIsOperator (..)
, FilterPrimaryKey (..)
) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField (Action(..))
import IHP.ModelSupport
import qualified Data.ByteString.Builder as Builder
import IHP.HSX.ToHtml
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text.Encoding as Text
import qualified GHC.Generics
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Prelude

-- | Represents whether string matching should be case-sensitive or not
data MatchSensitivity = CaseSensitive | CaseInsensitive deriving (Show, Eq)

-- | Operators used in WHERE clause conditions
data FilterOperator
    = EqOp -- ^ @col = val@
    | NotEqOp -- ^ @col != val@
    | InOp -- ^ @col IN (set)@
    | NotInOp -- ^ @col NOT IN (set)@
    | IsOp -- ^ @col IS val@
    | IsNotOp -- ^ @col IS NOT val@
    | LikeOp !MatchSensitivity -- ^ @col LIKE val@
    | NotLikeOp !MatchSensitivity -- ^ @col NOT LIKE val@
    | MatchesOp !MatchSensitivity -- ^ @col ~ pattern@
    | GreaterThanOp -- ^ @col > val@
    | GreaterThanOrEqualToOp -- ^ @col >= val@
    | LessThanOp -- ^ @col < val@
    | LessThanOrEqualToOp -- ^ @col <= val@
    | SqlOp -- ^ Used by 'filterWhereSql'
    deriving (Show, Eq)

-- | Represents an ORDER BY clause component
data OrderByClause =
    OrderByClause
    { orderByColumn :: !ByteString
    , orderByDirection :: !OrderByDirection }
    deriving (Show, Eq, GHC.Generics.Generic, DeepSeq.NFData)

-- | Type-level marker indicating no joins are allowed
data NoJoins

-- | Type-level empty list for tracking joined tables
data EmptyModelList

-- | Type-level cons cell for tracking joined tables
data ConsModelList model models

-- | Type class to represent the true list type EmptyModelList ConsModelList.
class ModelList a

instance ModelList EmptyModelList
instance ModelList (ConsModelList model models)

-- | Type class to query containment in the type-level list.
class IsJoined a b

instance (ModelList b) => IsJoined a (ConsModelList a b)
instance {-# OVERLAPPABLE #-} (ModelList b, IsJoined a b) => IsJoined a (ConsModelList c b)

-- | Class to generalise over different QueryBuilder-providing types. The actual query builder can be extracted with 'getQueryBuilder' and injected with 'injectQueryBuilder'. Also assigns a join register to a queryBuilderProvider.
class HasQueryBuilder queryBuilderProvider joinRegister | queryBuilderProvider -> joinRegister where
    getQueryBuilder :: queryBuilderProvider table -> QueryBuilder table
    injectQueryBuilder :: QueryBuilder table -> queryBuilderProvider table
    getQueryIndex :: queryBuilderProvider table -> Maybe ByteString
    getQueryIndex _ = Nothing
    {-# INLINABLE getQueryIndex #-}

-- | Wrapper for QueryBuilders resulting from joins. Associates a joinRegister type.
newtype JoinQueryBuilderWrapper joinRegister table = JoinQueryBuilderWrapper (QueryBuilder table)

-- | Wrapper for QueryBuilder that must not joins, e.g. queryUnion.
newtype NoJoinQueryBuilderWrapper table = NoJoinQueryBuilderWrapper (QueryBuilder table)

-- | Wrapper for QueryBuilders with indexed results.
newtype LabeledQueryBuilderWrapper foreignTable indexColumn indexValue table = LabeledQueryBuilderWrapper (QueryBuilder table)

-- | QueryBuilders have query builders and the join register is empty.
instance HasQueryBuilder QueryBuilder EmptyModelList where
    getQueryBuilder = id
    {-# INLINE getQueryBuilder #-}
    injectQueryBuilder = id
    {-# INLINE injectQueryBuilder #-}

-- | JoinQueryBuilderWrappers have query builders
instance HasQueryBuilder (JoinQueryBuilderWrapper joinRegister) joinRegister where
    getQueryBuilder (JoinQueryBuilderWrapper queryBuilder) = queryBuilder
    {-# INLINABLE getQueryBuilder #-}
    injectQueryBuilder = JoinQueryBuilderWrapper
    {-# INLINABLE injectQueryBuilder #-}

-- | NoJoinQueryBuilderWrapper have query builders and the join register does not allow any joins
instance HasQueryBuilder NoJoinQueryBuilderWrapper NoJoins where
    getQueryBuilder (NoJoinQueryBuilderWrapper queryBuilder) = queryBuilder
    {-# INLINABLE getQueryBuilder #-}
    injectQueryBuilder  = NoJoinQueryBuilderWrapper
    {-# INLINABLE injectQueryBuilder #-}

instance (KnownSymbol foreignTable, foreignModel ~ GetModelByTableName foreignTable , KnownSymbol indexColumn, HasField indexColumn foreignModel indexValue) => HasQueryBuilder (LabeledQueryBuilderWrapper foreignTable indexColumn indexValue) NoJoins where
    getQueryBuilder (LabeledQueryBuilderWrapper queryBuilder) = queryBuilder
    {-# INLINABLE getQueryBuilder #-}
    injectQueryBuilder = LabeledQueryBuilderWrapper
    {-# INLINABLE injectQueryBuilder #-}
    getQueryIndex _ = Just $ symbolToByteString @foreignTable <> "." <> (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @indexColumn)
    {-# INLINABLE getQueryIndex #-}

-- | The main QueryBuilder data type, representing different query operations
data QueryBuilder (table :: Symbol) =
    NewQueryBuilder { selectFrom :: !ByteString, columns :: ![ByteString] }
    | DistinctQueryBuilder   { queryBuilder :: !(QueryBuilder table) }
    | DistinctOnQueryBuilder { queryBuilder :: !(QueryBuilder table), distinctOnColumn :: !ByteString }
    | FilterByQueryBuilder   { queryBuilder :: !(QueryBuilder table), queryFilter :: !(ByteString, FilterOperator, Action, Snippet), applyLeft :: !(Maybe ByteString), applyRight :: !(Maybe ByteString) }
    | OrderByQueryBuilder    { queryBuilder :: !(QueryBuilder table), queryOrderByClause :: !OrderByClause }
    | LimitQueryBuilder      { queryBuilder :: !(QueryBuilder table), queryLimit :: !Int }
    | OffsetQueryBuilder     { queryBuilder :: !(QueryBuilder table), queryOffset :: !Int }
    | UnionQueryBuilder      { firstQueryBuilder :: !(QueryBuilder table), secondQueryBuilder :: !(QueryBuilder table) }
    | JoinQueryBuilder       { queryBuilder :: !(QueryBuilder table), joinData :: Join}

-- | Represents a WHERE condition
-- Stores templates for both backends: pg-simple template (with IN/NOT IN), hasql template (with = ANY/<> ALL)
-- Also stores both Action (for postgresql-simple) and Snippet (for hasql)
data Condition = VarCondition !ByteString !ByteString !Action !Snippet | OrCondition !Condition !Condition | AndCondition !Condition !Condition
--                             ^pgTemplate ^hasqlTemplate

-- | Snippet doesn't have Eq/Show instances, so we provide approximate ones for QueryBuilder
instance Eq Snippet where
    _ == _ = True  -- Approximate equality for deriving purposes

instance Show Snippet where
    showsPrec _ _ = Prelude.showString "<Snippet>"

deriving instance Show Condition
deriving instance Eq Condition
deriving instance Show (QueryBuilder table)
deriving instance Eq (QueryBuilder table)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol table => ToHtml (QueryBuilder table) where
    toHtml queryBuilder = toHtml (toSQLQueryBuilder queryBuilder)
      where
        -- Inline SQL generation for ToHtml to avoid circular imports
        toSQLQueryBuilder :: QueryBuilder table -> ByteString
        toSQLQueryBuilder qb = "QueryBuilder<" <> symbolToByteString @table <> ">"

-- | Represents a JOIN clause
data Join = Join { table :: ByteString, tableJoinColumn :: ByteString, otherJoinColumn :: ByteString }
    deriving (Show, Eq)

-- | ORDER BY direction
data OrderByDirection = Asc | Desc deriving (Eq, Show, GHC.Generics.Generic, DeepSeq.NFData)

-- | Represents a complete SQL query after building
data SQLQuery = SQLQuery
    { queryIndex :: !(Maybe ByteString)
    , selectFrom :: !ByteString
    , distinctClause :: !(Maybe ByteString)
    , distinctOnClause :: !(Maybe ByteString)
    , whereCondition :: !(Maybe Condition)
    , joins :: ![Join]
    , orderByClause :: ![OrderByClause]
    , limitClause :: !(Maybe ByteString)
    , offsetClause :: !(Maybe ByteString)
    , columns :: ![ByteString]
    } deriving (Show, Eq)

-- | Needed for the 'Eq Action' instance used in tests
deriving instance Eq Action

-- | Need for the 'Eq Builder.Builder' instance used in tests
instance Eq Builder.Builder where
    a == b = (Builder.toLazyByteString a) == (Builder.toLazyByteString b)

instance SetField "queryIndex" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { queryIndex = value }
instance SetField "selectFrom" SQLQuery ByteString where setField value sqlQuery = sqlQuery { selectFrom = value }
instance SetField "distinctClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctClause = value }
instance SetField "distinctOnClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctOnClause = value }
instance SetField "whereCondition" SQLQuery (Maybe Condition) where setField value sqlQuery = sqlQuery { whereCondition = value }
instance SetField "orderByClause" SQLQuery [OrderByClause] where setField value sqlQuery = sqlQuery { orderByClause = value }
instance SetField "limitClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { limitClause = value }
instance SetField "offsetClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { offsetClause = value }

-- | Type class for default scoping of queries
class DefaultScope table where
    defaultScope :: QueryBuilder table -> QueryBuilder table

instance {-# OVERLAPPABLE #-} DefaultScope table where
    {-# INLINE defaultScope #-}
    defaultScope queryBuilder = queryBuilder

instance Table (GetModelByTableName table) => Default (QueryBuilder table) where
    {-# INLINE def #-}
    def = NewQueryBuilder { selectFrom = tableNameByteString @(GetModelByTableName table), columns = columnNames @(GetModelByTableName table) }

-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp

-- | Type class for filtering by primary key
class FilterPrimaryKey table where
    filterWhereId :: Id' table -> QueryBuilder table -> QueryBuilder table
