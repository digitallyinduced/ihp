{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder
Description:  Tool to build simple sql queries
Copyright: (c) digitally induced GmbH, 2020

QueryBuilder is mainly used for doing simple `SELECT` sql queries. It allows dynamic
creation of sql queries in a type safe way.

For more complex sql queries, use 'IHP.ModelSupport.sqlQuery'.
-}
module IHP.QueryBuilder
( query
, filterWhere
, QueryBuilder
, In (In)
, orderBy
, orderByAsc
, orderByDesc
, limit
, offset
, queryUnion
, queryOr
, DefaultScope (..)
, filterWhereIn
, filterWhereNotIn
, EqOrIsOperator
, filterWhereSql
, FilterPrimaryKey (..)
, distinctOn
, distinct
, toSQL
, toSQL'
, buildQuery
)
where

import IHP.Prelude
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.OverloadedLabels
import IHP.ModelSupport
import qualified Data.ByteString.Builder as Builder
import IHP.HtmlSupport.ToHtml
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text.Encoding as Text

-- | Represent's a @SELECT * FROM ..@ query. It's the starting point to build a query.
-- Used together with the other functions to compose a sql query.
--
-- Example:
--
-- > toSQL (query @User)
-- > -- Returns: ("SELECT id, firstname, lastname FROM users", [])
--
-- Example: Fetching all users
--
-- > allUsers <- query @User |> fetch
-- > -- Runs a 'SELECT * FROM users' query
--
-- You can use it together with 'filterWhere':
--
-- > activeUsers :: [User] <-
-- >    query @User
-- >     |> filterWhere (#active, True)
-- >     |> fetch
query :: forall model table. (table ~ GetTableName model) => DefaultScope table => QueryBuilder table
query = (defaultScope @table) NewQueryBuilder
{-# INLINE query #-}

class DefaultScope table where
    defaultScope :: QueryBuilder table -> QueryBuilder table

instance {-# OVERLAPPABLE #-} DefaultScope table where
    {-# INLINE defaultScope #-}
    defaultScope queryBuilder = queryBuilder

instance Default (QueryBuilder table) where
    {-# INLINE def #-}
    def = NewQueryBuilder

data FilterOperator = EqOp | InOp | NotInOp | IsOp | SqlOp deriving (Show, Eq)


{-# INLINE compileOperator #-}
compileOperator EqOp = "="
compileOperator InOp = "IN"
compileOperator NotInOp = "NOT IN"
compileOperator IsOp = "IS"
compileOperator SqlOp = ""

data OrderByClause =
    OrderByClause
    { orderByColumn :: !ByteString
    , orderByDirection :: !OrderByDirection }
    deriving (Show, Eq)

data QueryBuilder (table :: Symbol) =
    NewQueryBuilder
    | DistinctQueryBuilder   { queryBuilder :: !(QueryBuilder table) }
    | DistinctOnQueryBuilder { queryBuilder :: !(QueryBuilder table), distinctOnColumn :: !ByteString }
    | FilterByQueryBuilder   { queryBuilder :: !(QueryBuilder table), queryFilter :: !(ByteString, FilterOperator, Action) }
    | OrderByQueryBuilder    { queryBuilder :: !(QueryBuilder table), queryOrderByClause :: !OrderByClause }
    | LimitQueryBuilder      { queryBuilder :: !(QueryBuilder table), queryLimit :: !Int }
    | OffsetQueryBuilder     { queryBuilder :: !(QueryBuilder table), queryOffset :: !Int }
    | UnionQueryBuilder      { firstQueryBuilder :: !(QueryBuilder table), secondQueryBuilder :: !(QueryBuilder table) }
    deriving (Show, Eq)

data Condition = VarCondition !ByteString !Action | OrCondition !Condition !Condition | AndCondition !Condition !Condition deriving (Show, Eq)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol table => ToHtml (QueryBuilder table) where
    toHtml queryBuilder = toHtml (toSQL queryBuilder)

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery
    { selectFrom :: !ByteString
    , distinctClause :: !(Maybe ByteString)
    , distinctOnClause :: !(Maybe ByteString)
    , whereCondition :: !(Maybe Condition)
    , orderByClause :: ![OrderByClause]
    , limitClause :: !(Maybe ByteString)
    , offsetClause :: !(Maybe ByteString)
    } deriving (Show, Eq)

-- | Needed for the 'Eq QueryBuilder' instance
deriving instance Eq Action

-- | Need for the 'Eq QueryBuilder' instance
--
-- You likely wonder: Why do we need the 'Eq SQLQuery' instance if this causes so much trouble?
-- This has to do with how has-many and belongs-to relations are models by the SchemaCompiler
--
-- E.g. given a table users and a table posts. Each Post belongs to a user. The schema compiler will
-- add a field 'posts :: QueryBuilder "posts"' with the default value @query |> filterWhere (#userId, get #id self)@ to all users by default.
-- 
-- This is needed to support syntax like this:
-- 
-- > user
-- >     |> get #posts
-- >     |> fetch
--
instance Eq Builder.Builder where
    a == b = (Builder.toLazyByteString a) == (Builder.toLazyByteString b)

instance SetField "selectFrom" SQLQuery ByteString where setField value sqlQuery = sqlQuery { selectFrom = value }
instance SetField "distinctClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctClause = value }
instance SetField "distinctOnClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctOnClause = value }
instance SetField "whereCondition" SQLQuery (Maybe Condition) where setField value sqlQuery = sqlQuery { whereCondition = value }
instance SetField "orderByClause" SQLQuery [OrderByClause] where setField value sqlQuery = sqlQuery { orderByClause = value }
instance SetField "limitClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { limitClause = value }
instance SetField "offsetClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { offsetClause = value }

{-# INLINE buildQuery #-}
buildQuery :: forall table. (KnownSymbol table) => QueryBuilder table -> SQLQuery
buildQuery NewQueryBuilder =
    let tableName = symbolToByteString @table
    in SQLQuery
            { selectFrom = tableName
            , distinctClause = Nothing
            , distinctOnClause = Nothing
            , whereCondition = Nothing
            , orderByClause = []
            , limitClause = Nothing
            , offsetClause = Nothing
            }
buildQuery DistinctQueryBuilder { queryBuilder } = queryBuilder
        |> buildQuery
        |> setJust #distinctClause "DISTINCT"
buildQuery DistinctOnQueryBuilder { queryBuilder, distinctOnColumn } = queryBuilder
        |> buildQuery
        |> setJust #distinctOnClause ("DISTINCT ON (" <> distinctOnColumn <> ")")
buildQuery FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, operator, value) } =
            let
                condition = VarCondition (columnName <> " " <> compileOperator operator <> " ?") value
            in
                queryBuilder
                    |> buildQuery
                    |> modify #whereCondition \case
                            Just c -> Just (AndCondition c condition)
                            Nothing -> Just condition
buildQuery OrderByQueryBuilder { queryBuilder, queryOrderByClause } = queryBuilder
        |> buildQuery
        |> modify #orderByClause (\value -> value <> [queryOrderByClause] ) -- although adding to the end of a list is bad form, these lists are very short
buildQuery LimitQueryBuilder { queryBuilder, queryLimit } =
                queryBuilder
                |> buildQuery
                |> setJust #limitClause (
                        (Builder.byteString "LIMIT " <> Builder.intDec queryLimit)
                        |> Builder.toLazyByteString
                        |> LByteString.toStrict
                    )
buildQuery OffsetQueryBuilder { queryBuilder, queryOffset } = queryBuilder
        |> buildQuery
        |> setJust #offsetClause (
                (Builder.byteString "OFFSET " <> Builder.intDec queryOffset)
                |> Builder.toLazyByteString
                |> LByteString.toStrict
            )
buildQuery UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder } =
            let
                firstQuery = buildQuery firstQueryBuilder
                secondQuery = buildQuery secondQueryBuilder
                isSimpleQuery query = null (orderByClause query) && isNothing (limitClause query) && isNothing (offsetClause query)
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

-- | Transforms a @query @@User |> ..@ expression into a SQL Query. Returns a tuple with the sql query template and it's placeholder values.
--
-- __Example:__ Get the sql query that is represented by a QueryBuilder
--
-- >>> let postsQuery = query @Post |> filterWhere (#public, True)
-- >>> toSQL postsQuery
-- ("SELECT posts.* FROM posts WHERE public = ?", [Plain "true"])
toSQL :: (KnownSymbol table) => QueryBuilder table -> (ByteString, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
{-# INLINE toSQL #-}

toSQL' :: SQLQuery -> (ByteString, [Action])
toSQL' sqlQuery@SQLQuery { selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause } =
        (DeepSeq.force theQuery, theParams)
    where
        !theQuery =
            ByteString.intercalate " " $
                catMaybes
                    [ Just "SELECT"
                    , distinctClause
                    , distinctOnClause
                    , Just selectors
                    , Just "FROM"
                    , Just fromClause
                    , whereConditions'
                    , orderByClause'
                    , limitClause
                    , offsetClause
                    ]

        selectors :: ByteString
        selectors = selectFrom <> ".*"

        fromClause :: ByteString
        fromClause = selectFrom

        !theParams =
            case whereCondition sqlQuery of
                Just condition -> compileConditionArgs condition
                Nothing -> mempty

        toQualifiedName unqualifiedName = selectFrom <> "." <> unqualifiedName

        whereConditions' = case whereCondition sqlQuery of
                Just condition -> Just $ "WHERE " <> compileConditionQuery condition
                Nothing -> Nothing

        orderByClause' :: Maybe ByteString
        orderByClause' = case orderByClause of
                [] -> Nothing
                xs -> Just ("ORDER BY " <> ByteString.intercalate "," ((map (\OrderByClause { orderByColumn, orderByDirection } -> orderByColumn <> (if orderByDirection == Desc then " DESC" else mempty)) xs)))

{-# INLINE toSQL' #-}

{-# INLINE compileConditionQuery #-}
compileConditionQuery :: Condition -> ByteString
compileConditionQuery (VarCondition var _) =  var
compileConditionQuery (OrCondition a b) =  "(" <> compileConditionQuery a <> ") OR (" <> compileConditionQuery b <> ")"
compileConditionQuery (AndCondition a b) =  "(" <> compileConditionQuery a <> ") AND (" <> compileConditionQuery b <> ")"

{-# INLINE compileConditionArgs #-}
compileConditionArgs :: Condition -> [Action]
compileConditionArgs (VarCondition _ arg) = [arg]
compileConditionArgs (OrCondition a b) = compileConditionArgs a <> compileConditionArgs b
compileConditionArgs (AndCondition a b) = compileConditionArgs a <> compileConditionArgs b

-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp


class FilterPrimaryKey table where
    filterWhereId :: Id' table -> QueryBuilder table -> QueryBuilder table

-- | Adds a simple @WHERE x = y@ condition to the query.
--
-- __Example:__ Only show projects where @active@ is @True@.
--
-- > activeProjects <- query @Project
-- >     |> filterWhere (#active, True)
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE active = True
--
-- __Example:__ Find book with title @Learn you a Haskell@.
--
-- > book <- query @Book
-- >     |> filterWhere (#title, "Learn you a Haskell")
-- >     |> fetchOne
-- > -- SELECT * FROM books WHERE name = 'Learn you a Haskell' LIMIT 1
--
--
-- __Example:__ Find active projects owned by the current user.
--
-- > projects <- query @User
-- >     |> filterWhere (#active, True)
-- >     |> filterWhere (#currentUserId, currentUserId)
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE active = true AND current_user_id = '..'
--
--
-- For dynamic conditions (e.g. involving @NOW()@), see 'filterWhereSql'.
--
-- For @WHERE x IN (a, b, c)@ conditions, take a look at 'filterWhereIn' and 'filterWhereNotIn'.
--
-- When your condition is too complex, use a raw sql query with 'IHP.ModelSupport.sqlQuery'.
filterWhere :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table) => (Proxy name, value) -> QueryBuilder table -> QueryBuilder table
filterWhere (name, value) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, toField value) }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhere #-}

filterWhereIn :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, [value]) -> QueryBuilder table -> QueryBuilder table
filterWhereIn (name, value) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, InOp, toField (In value)) }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhereIn #-}

filterWhereNotIn :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, [value]) -> QueryBuilder table -> QueryBuilder table
filterWhereNotIn (_, []) queryBuilder = queryBuilder -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotIn (name, value) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, NotInOp, toField (In value)) }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhereNotIn #-}

-- | Allows to add a custom raw sql where condition
--
-- If your query cannot be represented with 'filterWhereSql', take a look at 'IHP.ModelSupport.sqlQuery'.
--
-- __Example:__ Fetching all projects created in the last 24 hours.
-- > latestProjects <- query @Project
-- >     |> filterWhereSql (#startedAt, "< current_timestamp - interval '1 day'")
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE started_at < current_timestamp - interval '1 day'
filterWhereSql :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, ByteString) -> QueryBuilder table -> QueryBuilder table
filterWhereSql (name, sqlCondition) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, SqlOp, Plain (Builder.byteString sqlCondition)) }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhereSql #-}

-- | Adds an @ORDER BY .. ASC@ to your query.
--
-- Use 'orderByDesc' for descending order.
--
-- __Example:__ Fetch the 10 oldest books.
--
-- > query @Book
-- >     |> orderBy #createdAt
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM books LIMIT 10 ORDER BY created_at ASC
orderByAsc :: forall name model table value. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByAsc !name queryBuilder = OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Asc } }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE orderByAsc #-}

-- | Adds an @ORDER BY .. DESC@ to your query.
--
-- Use 'orderBy' for ascending order.
--
-- __Example:__ Fetch the 10 newest projects (ordered by creation time).
--
-- > query @Project
-- >     |> orderBy #createdAt
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM projects LIMIT 10 ORDER BY created_at DESC
orderByDesc :: forall name model table value. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByDesc !name queryBuilder = OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Desc } }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE orderByDesc #-}

-- | Alias for 'orderByAsc'
orderBy :: (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderBy !name = orderByAsc name
{-# INLINE orderBy #-}

-- | Adds an @LIMIT ..@ to your query.
--
--
-- __Example:__ Fetch 10 posts
--
-- > query @Post
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM posts LIMIT 10
limit :: Int -> QueryBuilder model -> QueryBuilder model
limit !queryLimit queryBuilder = LimitQueryBuilder { queryBuilder, queryLimit }
{-# INLINE limit #-}

-- | Adds an @OFFSET ..@ to your query. Most often used together with @LIMIT...@
--
--
-- __Example:__ Fetch posts 10-20
--
-- > query @Post
-- >     |> limit 10
-- >     |> offset 10
-- >     |> fetch
-- > -- SELECT * FROM posts LIMIT 10 OFFSET 10
offset :: Int -> QueryBuilder model -> QueryBuilder model
offset !queryOffset queryBuilder = OffsetQueryBuilder { queryBuilder, queryOffset }
{-# INLINE offset #-}

-- | Merges the results of two query builders.
--
-- Take a look at ‘queryOr'  as well, as this might be a bit shorter.
--
-- __Example:__ Return all pages owned by the user or owned by the users team.
--
-- > let userPages = query @Page |> filterWhere (#ownerId, currentUserId)
-- > let teamPages = query @Page |> filterWhere (#teamId, currentTeamId)
-- > pages <- queryUnion userPages teamPages |> fetch
-- > -- (SELECT * FROM pages WHERE owner_id = '..') UNION (SELECT * FROM pages WHERE team_id = '..')
queryUnion :: QueryBuilder model -> QueryBuilder model -> QueryBuilder model
queryUnion firstQueryBuilder secondQueryBuilder = UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder }
{-# INLINE queryUnion #-}


-- | Adds an @a OR b@ condition
--
-- __Example:__ Return all pages owned by the user or public.
--
-- > query @Page
-- >     |> queryOr
-- >         (filterWhere (#createdBy, currentUserId))
-- >         (filterWhere (#public, True))
-- >     |> fetch
-- > -- SELECT * FROM pages WHERE created_by = '..' OR public = True
queryOr :: (qb ~ QueryBuilder model) => (qb -> qb) -> (qb -> qb) -> qb -> qb
queryOr firstQuery secondQuery queryBuilder = UnionQueryBuilder { firstQueryBuilder = firstQuery queryBuilder, secondQueryBuilder = secondQuery queryBuilder }
{-# INLINE queryOr #-}

-- | Adds an @DISTINCT to your query.
--
-- Use 'distinct' to remove all duplicate rows from the result
--
-- __Example:__ Fetch distinct books
--
-- > query @Book
-- >     |> distinct
-- >     |> fetch
-- > -- SELECT DISTINCT * FROM books
distinct :: QueryBuilder table -> QueryBuilder table
distinct = DistinctQueryBuilder
{-# INLINE distinct #-}

-- | Adds an @DISTINCT ON .. to your query.
--
-- Use 'distinctOn' to return a single row for each distinct value provided.
--
-- __Example:__ Fetch one book for each categoryId field
--
-- > query @Book
-- >     |> distinctOn #categoryId
-- >     |> fetch
-- > -- SELECT DISTINCT ON (category_id) * FROM books
distinctOn :: forall name model value table. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
distinctOn !name queryBuilder = DistinctOnQueryBuilder { distinctOnColumn = columnName, queryBuilder }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE distinctOn #-}
