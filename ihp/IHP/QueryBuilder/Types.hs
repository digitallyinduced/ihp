{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass, MagicHash #-}
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
, ConditionValue (..)
, Join (..)
, OrderByClause (..)
, OrderByDirection (..)
, FilterOperator (..)
, MatchSensitivity (..)
  -- * Helpers
, addCondition
, qualifyAndJoinColumns
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
  -- * Param value extraction
, getParamPrinterText
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.HSX.ToHtml
import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics
import qualified Hasql.Encoders as Encoders
import qualified Prelude
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.DList as DList
import GHC.Exts (Any, Int#)
import qualified Data.Text as Text

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

-- | The QueryBuilder is a flat newtype over SQLQuery. Each combinator directly
-- modifies fields of the underlying SQLQuery, avoiding any recursive tree traversal.
newtype QueryBuilder (table :: Symbol) = QueryBuilder { unQueryBuilder :: SQLQuery }

-- | Add a WHERE condition to a QueryBuilder, ANDing with any existing condition.
addCondition :: Condition -> QueryBuilder table -> QueryBuilder table
addCondition condition (QueryBuilder sq) = QueryBuilder $ sq
    { whereCondition = case whereCondition sq of
        Nothing -> Just condition
        Just existing -> Just (AndCondition existing condition)
    }
{-# INLINE addCondition #-}

-- | A condition value: either a parameterized encoder or a literal SQL fragment.
data ConditionValue
    = Param !(Encoders.Params ())   -- ^ Parameterized value: compiler assigns $N
    | Literal !Text                 -- ^ Raw SQL text (for filterWhereSql, NULL comparisons, etc.)

instance Show ConditionValue where
    showsPrec _ (Param _) = Prelude.showString "<Param>"
    showsPrec _ (Literal t) = Prelude.showString "Literal " . Prelude.shows t

instance Eq ConditionValue where
    Literal a == Literal b = a == b
    _ == _ = False -- Params cannot be compared for equality

-- | Mirror of hasql's internal @Params@ record (5 fields in hasql 1.10.x).
-- Only the 5th field (printer) is accessed.
--
-- Fields use @Int#@ and @Any@ to match compiled layouts where
-- GHCi would otherwise ignore @{-# UNPACK #-}@ pragmas.
data ParamsMirror a = ParamsMirror
    Int#                       -- size (unboxed, matching compiled hasql)
    Any                        -- unknownTypes
    Any                        -- columnsMetadata
    Any                        -- serializer
    (a -> DList.DList Text)    -- printer

-- | Extract the text representation of parameter values from an 'Encoders.Params'.
--
-- Uses 'unsafeCoerce' to access the internal @printer@ field of the @Params@
-- record to get the human-readable text form of encoded values.
getParamPrinterText :: Encoders.Params () -> [Text]
getParamPrinterText p = DList.toList (printer ())
    where ParamsMirror _ _ _ _ printer = unsafeCoerce p

-- | Represents a WHERE condition
data Condition
    = ColumnCondition !Text !FilterOperator !ConditionValue !(Maybe Text) !(Maybe Text)
    --                ^col  ^op             ^value           ^applyLeft    ^applyRight
    | OrCondition !Condition !Condition
    | AndCondition !Condition !Condition

-- | Returns a numeric tag for each Condition constructor.
-- Pattern match is exhaustive so adding a constructor triggers -Wincomplete-patterns.
conditionTag :: Condition -> Int
conditionTag ColumnCondition {} = 0
conditionTag OrCondition {} = 1
conditionTag AndCondition {} = 2

instance Eq Condition where
    (ColumnCondition c1 o1 s1 al1 ar1) == (ColumnCondition c2 o2 s2 al2 ar2) = c1 == c2 && o1 == o2 && s1 == s2 && al1 == al2 && ar1 == ar2
    (OrCondition l1 r1) == (OrCondition l2 r2) = l1 == l2 && r1 == r2
    (AndCondition l1 r1) == (AndCondition l2 r2) = l1 == l2 && r1 == r2
    a == b = conditionTag a == conditionTag b

deriving instance Show Condition

instance Show (QueryBuilder table) where
    show (QueryBuilder sq) = "QueryBuilder " ++ Prelude.show sq

instance Eq (QueryBuilder table) where
    (QueryBuilder sq1) == (QueryBuilder sq2) = sqlQueryEq sq1 sq2

-- | Compare two SQLQuery values. Uses snippetEq for comparing Condition fields.
sqlQueryEq :: SQLQuery -> SQLQuery -> Bool
sqlQueryEq a b =
    selectFrom a == selectFrom b
    && columnsSql a == columnsSql b
    && distinctClause a == distinctClause b
    && distinctOnClause a == distinctOnClause b
    && condEq (whereCondition a) (whereCondition b)
    && joins a == joins b
    && orderByClause a == orderByClause b
    && limitClause a == limitClause b
    && offsetClause a == offsetClause b
  where
    condEq Nothing Nothing = True
    condEq (Just c1) (Just c2) = c1 == c2
    condEq _ _ = False

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
    , distinctClause :: !Bool
    , distinctOnClause :: !(Maybe Text)
    , whereCondition :: !(Maybe Condition)
    , joins :: ![Join]
    , orderByClause :: ![OrderByClause]
    , limitClause :: !(Maybe Int)
    , offsetClause :: !(Maybe Int)
    , columns :: ![Text]
    , columnsSql :: !Text
    -- ^ Pre-computed qualified column selector, e.g. @"users.id, users.name"@.
    -- Built once at query construction time so the compiler avoids re-qualifying
    -- and re-joining the column list on every compilation.
    } deriving (Show)


instance SetField "queryIndex" SQLQuery (Maybe Text) where setField value sqlQuery = sqlQuery { queryIndex = value }
instance SetField "selectFrom" SQLQuery Text where setField value sqlQuery = sqlQuery { selectFrom = value }
instance SetField "distinctClause" SQLQuery Bool where setField value sqlQuery = sqlQuery { distinctClause = value }
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
    def = let tn = tableName @(GetModelByTableName table)
              cols = columnNames @(GetModelByTableName table)
          in QueryBuilder SQLQuery
        { queryIndex = Nothing
        , selectFrom = tn
        , columns = cols
        , columnsSql = qualifyAndJoinColumns tn cols
        , distinctClause = False
        , distinctOnClause = Nothing
        , whereCondition = Nothing
        , joins = []
        , orderByClause = []
        , limitClause = Nothing
        , offsetClause = Nothing
        }

-- | Pre-compute the qualified, comma-separated column selector.
-- E.g. @qualifyAndJoinColumns "users" ["id", "name"] = "users.id, users.name"@
--
-- Intentionally NOINLINE: call sites (query, def) are lifted to CAFs, so
-- this is evaluated once per table type. NOINLINE prevents GHC from inlining
-- the map/intercalate into every use site of the resulting SQLQuery.
qualifyAndJoinColumns :: Text -> [Text] -> Text
qualifyAndJoinColumns tableName columns =
    Text.intercalate ", " (map (\c -> tableName <> "." <> c) columns)
{-# NOINLINE qualifyAndJoinColumns #-}

-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp

-- | Type class for filtering by primary key
class FilterPrimaryKey table where
    filterWhereId :: Id' table -> QueryBuilder table -> QueryBuilder table
