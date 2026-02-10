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
import IHP.ModelSupport
import IHP.HSX.ToHtml
import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics
import qualified Hasql.DynamicStatements.Snippet as Snippet
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
    { orderByColumn :: !Text
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
    getQueryIndex :: queryBuilderProvider table -> Maybe Text
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
    getQueryIndex _ = Just $ symbolToText @foreignTable <> "." <> fieldNameToColumnName (symbolToText @indexColumn)
    {-# INLINABLE getQueryIndex #-}

-- | The main QueryBuilder data type, representing different query operations
data QueryBuilder (table :: Symbol) =
    NewQueryBuilder { selectFrom :: !Text, columns :: ![Text] }
    | DistinctQueryBuilder   { queryBuilder :: !(QueryBuilder table) }
    | DistinctOnQueryBuilder { queryBuilder :: !(QueryBuilder table), distinctOnColumn :: !Text }
    | FilterByQueryBuilder   { queryBuilder :: !(QueryBuilder table), queryFilter :: !(Text, FilterOperator, Snippet), applyLeft :: !(Maybe Text), applyRight :: !(Maybe Text) }
    | OrderByQueryBuilder    { queryBuilder :: !(QueryBuilder table), queryOrderByClause :: !OrderByClause }
    | LimitQueryBuilder      { queryBuilder :: !(QueryBuilder table), queryLimit :: !Int }
    | OffsetQueryBuilder     { queryBuilder :: !(QueryBuilder table), queryOffset :: !Int }
    | UnionQueryBuilder      { firstQueryBuilder :: !(QueryBuilder table), secondQueryBuilder :: !(QueryBuilder table) }
    | JoinQueryBuilder       { queryBuilder :: !(QueryBuilder table), joinData :: Join}

-- | Represents a WHERE condition
data Condition = VarCondition !Text !Snippet | OrCondition !Condition !Condition | AndCondition !Condition !Condition
--                             ^hasqlTemplate

-- | Snippet doesn't have a Show instance, so we provide one for debugging QueryBuilder
instance Show Snippet where
    showsPrec _ _ = Prelude.showString "<Snippet>"

-- | Snippet is an opaque type with no Eq instance. We compare snippets by their
-- rendered SQL template via 'Snippet.toSql'. This only compares the SQL structure
-- (e.g. @col = $1@), not the parameter values.
snippetEq :: Snippet -> Snippet -> Bool
snippetEq a b = Snippet.toSql a == Snippet.toSql b

-- | Returns a numeric tag for each Condition constructor.
-- Pattern match is exhaustive so adding a constructor triggers -Wincomplete-patterns.
conditionTag :: Condition -> Int
conditionTag VarCondition {} = 0
conditionTag OrCondition {} = 1
conditionTag AndCondition {} = 2

instance Eq Condition where
    (VarCondition h1 s1) == (VarCondition h2 s2) = h1 == h2 && snippetEq s1 s2
    (OrCondition l1 r1) == (OrCondition l2 r2) = l1 == l2 && r1 == r2
    (AndCondition l1 r1) == (AndCondition l2 r2) = l1 == l2 && r1 == r2
    a == b = conditionTag a == conditionTag b

-- | Returns a numeric tag for each QueryBuilder constructor.
-- Pattern match is exhaustive so adding a constructor triggers -Wincomplete-patterns.
queryBuilderTag :: QueryBuilder table -> Int
queryBuilderTag NewQueryBuilder {} = 0
queryBuilderTag DistinctQueryBuilder {} = 1
queryBuilderTag DistinctOnQueryBuilder {} = 2
queryBuilderTag FilterByQueryBuilder {} = 3
queryBuilderTag OrderByQueryBuilder {} = 4
queryBuilderTag LimitQueryBuilder {} = 5
queryBuilderTag OffsetQueryBuilder {} = 6
queryBuilderTag UnionQueryBuilder {} = 7
queryBuilderTag JoinQueryBuilder {} = 8

instance Eq (QueryBuilder table) where
    (NewQueryBuilder s1 c1) == (NewQueryBuilder s2 c2) = s1 == s2 && c1 == c2
    (DistinctQueryBuilder q1) == (DistinctQueryBuilder q2) = q1 == q2
    (DistinctOnQueryBuilder q1 d1) == (DistinctOnQueryBuilder q2 d2) = q1 == q2 && d1 == d2
    (FilterByQueryBuilder q1 (b1, op1, sn1) l1 r1) == (FilterByQueryBuilder q2 (b2, op2, sn2) l2 r2) = q1 == q2 && b1 == b2 && op1 == op2 && snippetEq sn1 sn2 && l1 == l2 && r1 == r2
    (OrderByQueryBuilder q1 o1) == (OrderByQueryBuilder q2 o2) = q1 == q2 && o1 == o2
    (LimitQueryBuilder q1 l1) == (LimitQueryBuilder q2 l2) = q1 == q2 && l1 == l2
    (OffsetQueryBuilder q1 o1) == (OffsetQueryBuilder q2 o2) = q1 == q2 && o1 == o2
    (UnionQueryBuilder f1 s1) == (UnionQueryBuilder f2 s2) = f1 == f2 && s1 == s2
    (JoinQueryBuilder q1 j1) == (JoinQueryBuilder q2 j2) = q1 == q2 && j1 == j2
    a == b = queryBuilderTag a == queryBuilderTag b

deriving instance Show Condition
deriving instance Show (QueryBuilder table)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol table => ToHtml (QueryBuilder table) where
    toHtml queryBuilder = toHtml (toSQLQueryBuilder queryBuilder)
      where
        -- Inline SQL generation for ToHtml to avoid circular imports
        toSQLQueryBuilder :: QueryBuilder table -> Text
        toSQLQueryBuilder qb = "QueryBuilder<" <> symbolToText @table <> ">"

-- | Represents a JOIN clause
data Join = Join { table :: Text, tableJoinColumn :: Text, otherJoinColumn :: Text }
    deriving (Show, Eq)

-- | ORDER BY direction
data OrderByDirection = Asc | Desc deriving (Eq, Show, GHC.Generics.Generic, DeepSeq.NFData)

-- | Represents a complete SQL query after building
data SQLQuery = SQLQuery
    { queryIndex :: !(Maybe Text)
    , selectFrom :: !Text
    , distinctClause :: !(Maybe Text)
    , distinctOnClause :: !(Maybe Text)
    , whereCondition :: !(Maybe Condition)
    , joins :: ![Join]
    , orderByClause :: ![OrderByClause]
    , limitClause :: !(Maybe Int)
    , offsetClause :: !(Maybe Int)
    , columns :: ![Text]
    } deriving (Show)


instance SetField "queryIndex" SQLQuery (Maybe Text) where setField value sqlQuery = sqlQuery { queryIndex = value }
instance SetField "selectFrom" SQLQuery Text where setField value sqlQuery = sqlQuery { selectFrom = value }
instance SetField "distinctClause" SQLQuery (Maybe Text) where setField value sqlQuery = sqlQuery { distinctClause = value }
instance SetField "distinctOnClause" SQLQuery (Maybe Text) where setField value sqlQuery = sqlQuery { distinctOnClause = value }
instance SetField "whereCondition" SQLQuery (Maybe Condition) where setField value sqlQuery = sqlQuery { whereCondition = value }
instance SetField "orderByClause" SQLQuery [OrderByClause] where setField value sqlQuery = sqlQuery { orderByClause = value }
instance SetField "limitClause" SQLQuery (Maybe Int) where setField value sqlQuery = sqlQuery { limitClause = value }
instance SetField "offsetClause" SQLQuery (Maybe Int) where setField value sqlQuery = sqlQuery { offsetClause = value }

-- | Type class for default scoping of queries
class DefaultScope table where
    defaultScope :: QueryBuilder table -> QueryBuilder table

instance {-# OVERLAPPABLE #-} DefaultScope table where
    {-# INLINE defaultScope #-}
    defaultScope queryBuilder = queryBuilder

instance Table (GetModelByTableName table) => Default (QueryBuilder table) where
    {-# INLINE def #-}
    def = NewQueryBuilder { selectFrom = tableName @(GetModelByTableName table), columns = columnNames @(GetModelByTableName table) }

-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp

-- | Type class for filtering by primary key
class FilterPrimaryKey table where
    filterWhereId :: Id' table -> QueryBuilder table -> QueryBuilder table
