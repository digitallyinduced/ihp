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
, findManyBy
, findMaybeBy
, filterWhere
, QueryBuilder
, findBy
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
, genericFetchId
, genericfetchIdOneOrNothing
, genericFetchIdOne
, Fetchable (..)
, genericFetchIds
, genericfetchIdsOneOrNothing
, genericFetchIdsOne
, EqOrIsOperator
, fetchCount
, filterWhereSql
, fetchExists
, FilterPrimaryKey (..)
, distinctOn
, distinct
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
import qualified Data.ByteString.Builder as ByteStringBuilder
import IHP.HtmlSupport.ToHtml
import qualified Data.ByteString.Char8 as ByteString
import qualified Control.DeepSeq as DeepSeq

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
compileOperator _ EqOp = "="
compileOperator _ InOp = "IN"
compileOperator _ NotInOp = "NOT IN"
compileOperator _ IsOp = "IS"
compileOperator _ SqlOp = ""

data QueryBuilder (table :: Symbol) where
    NewQueryBuilder :: QueryBuilder table
    DistinctQueryBuilder :: QueryBuilder table -> QueryBuilder table
    DistinctOnQueryBuilder :: KnownSymbol field => !(Proxy field) -> !(QueryBuilder table) -> QueryBuilder table
    FilterByQueryBuilder :: (KnownSymbol field) => !(Proxy field, FilterOperator, Action) -> !(QueryBuilder table) -> QueryBuilder table
    OrderByQueryBuilder :: KnownSymbol field => !(Proxy field, OrderByDirection) -> !(QueryBuilder table) -> QueryBuilder table
    LimitQueryBuilder :: Int -> !(QueryBuilder table) -> QueryBuilder table
    OffsetQueryBuilder :: Int -> !(QueryBuilder table) -> QueryBuilder table
    UnionQueryBuilder :: !(QueryBuilder table) -> !(QueryBuilder table) -> QueryBuilder table

data Condition = VarCondition !ByteString !Action | OrCondition !Condition !Condition | AndCondition !Condition !Condition deriving (Show)

deriving instance Show (QueryBuilder a)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol table => ToHtml (QueryBuilder table) where
    toHtml queryBuilder = toHtml (toSQL queryBuilder)

-- | This hack is to allow Eq instances for models with hasMany relations
instance Eq (IHP.QueryBuilder.QueryBuilder table) where a == b = True

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: !ByteString,
        distinctClause :: !(Maybe ByteString),
        distinctOnClause :: !(Maybe ByteString),
        whereCondition :: !(Maybe Condition),
        orderByClause :: !([(ByteString, OrderByDirection)]),
        limitClause :: !(Maybe ByteString),
        offsetClause :: !(Maybe ByteString)
    }

{-# INLINE buildQuery #-}
buildQuery :: forall table. (KnownSymbol table) => QueryBuilder table -> SQLQuery
buildQuery !queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolToByteString @table
            in SQLQuery { selectFrom = cs tableName, distinctClause = Nothing, distinctOnClause = Nothing, whereCondition = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing }
        DistinctQueryBuilder queryBuilder -> (buildQuery queryBuilder) { distinctClause = Just "DISTINCT" }
        DistinctOnQueryBuilder fieldProxy queryBuilder -> (buildQuery queryBuilder) { distinctOnClause = Just ("DISTINCT ON (" <> (cs $ symbolVal fieldProxy) <> ")") }
        FilterByQueryBuilder (fieldProxy, operator, value) queryBuilder ->
            let
                query = buildQuery queryBuilder
                condition = VarCondition ((cs $ fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " " <> compileOperator fieldProxy operator <> " ?") value
            in
                query { whereCondition = Just $ case whereCondition query of Just c -> AndCondition c condition; Nothing -> condition }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = buildQuery queryBuilder
            in query { orderByClause = (orderByClause query) ++ [(cs $ fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection)] } -- although adding to the end of a list is bad form, these lists are very short
        LimitQueryBuilder limit queryBuilder -> (buildQuery queryBuilder) { limitClause = Just ("LIMIT " <> cs (show limit)) }
        OffsetQueryBuilder offset queryBuilder -> (buildQuery queryBuilder) { offsetClause = Just ("OFFSET " <> cs (show offset)) }
        UnionQueryBuilder firstQueryBuilder secondQueryBuilder ->
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


class Fetchable fetchable model | fetchable -> model where
    type FetchResult fetchable model
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO (FetchResult fetchable model)
    fetchOneOrNothing :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO (Maybe model)
    fetchOne :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO model

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (QueryBuilder table) model where
    type FetchResult (QueryBuilder table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => QueryBuilder table -> IO [model]
    fetch !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder)
        logQuery theQuery theParameters
        trackTableRead (tableNameByteString @model)
        sqlQuery (Query $ cs theQuery) theParameters

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder table -> IO (Maybe model)
    fetchOneOrNothing !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder) { limitClause = Just "LIMIT 1"}
        logQuery theQuery theParameters
        trackTableRead (tableNameByteString @model)
        results <- sqlQuery (Query $ cs theQuery) theParameters
        pure $ listToMaybe results

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder table -> IO model
    fetchOne !queryBuilder = do
        maybeModel <- fetchOneOrNothing queryBuilder
        case maybeModel of
            Just model -> pure model
            Nothing -> throwIO RecordNotFoundException { queryAndParams = toSQL queryBuilder }

-- | Returns the count of records selected by the query builder.
--
-- __Example:__ Counting all users.
--
-- > allUsersCount <- query @User |> fetchCount -- SELECT COUNT(*) FROM users
--
--
-- __Example:__ Counting all active projects
--
-- >     activeProjectsCount <- query @Project
-- >         |> filterWhere (#isActive, True)
-- >         |> fetchCount
-- >     -- SELECT COUNT(*) FROM projects WHERE is_active = true
fetchCount :: forall table. (?modelContext :: ModelContext, KnownSymbol table) => QueryBuilder table -> IO Int
fetchCount !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT COUNT(*) FROM (" <> theQuery' <> ") AS _count_values"
    logQuery theQuery theParameters
    trackTableRead (symbolToByteString @table)
    [PG.Only count] <- sqlQuery (Query $! cs theQuery) theParameters
    pure count
{-# INLINE fetchCount #-}

-- | Checks whether the query has any results.
--
-- Returns @True@ when there is at least one row matching the conditions of the query. Returns @False@ otherwise.
--
-- __Example:__ Checking whether there are unread messages
--
-- >     hasUnreadMessages <- query @Message
-- >         |> filterWhere (#isUnread, True)
-- >         |> fetchExists
-- >     -- SELECT EXISTS (SELECT * FROM messages WHERE is_unread = true)
fetchExists :: forall table. (?modelContext :: ModelContext, KnownSymbol table) => QueryBuilder table -> IO Bool
fetchExists !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT EXISTS (" <> theQuery' <> ") AS _exists_values"
    logQuery theQuery theParameters
    trackTableRead (symbolToByteString @table)
    [PG.Only exists] <- sqlQuery (Query $! cs theQuery) theParameters
    pure exists
{-# INLINE fetchExists #-}

{-# INLINE genericFetchId #-}
genericFetchId :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO [model]
genericFetchId !id = query @model |> filterWhereId id |> fetch

{-# INLINE genericfetchIdOneOrNothing #-}
genericfetchIdOneOrNothing :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO (Maybe model)
genericfetchIdOneOrNothing !id = query @model |> filterWhereId id |> fetchOneOrNothing

{-# INLINE genericFetchIdOne #-}
genericFetchIdOne :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO model
genericFetchIdOne !id = query @model |> filterWhereId id |> fetchOne

{-# INLINE genericFetchIds #-}
genericFetchIds :: forall table model value. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIn (#id, ids) |> fetch

{-# INLINE genericfetchIdsOneOrNothing #-}
genericfetchIdsOneOrNothing :: forall model value table. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIn (#id, ids) |> fetchOneOrNothing

{-# INLINE genericFetchIdsOne #-}
genericFetchIdsOne :: forall model value table. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIn (#id, ids) |> fetchOne

toSQL :: forall table. (KnownSymbol table) => QueryBuilder table -> (ByteString, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
{-# INLINE toSQL #-}

toSQL' sqlQuery@SQLQuery { selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause } =
        (DeepSeq.force theQuery, theParams)
    where
        !theQuery =
            "SELECT "
            <> distinctClause' <> distinctOnClause' <> " "
            <> selectors <> " FROM "
            <> fromClause
            <> whereConditions' <> " "
            <> orderByClause' <> " "
            <> limitClause'
            <> offsetClause'

        selectors :: ByteString
        selectors = selectFrom <> ".*"
        fromClause :: ByteString
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
                [] -> mempty
                xs -> " ORDER BY " <> ByteString.intercalate "," ((map (\(column,direction) -> column <> (if direction == Desc then " DESC" else mempty)) xs))
        distinctClause' = fromMaybe "" distinctClause
        distinctOnClause' = fromMaybe "" distinctOnClause
        limitClause' = fromMaybe "" limitClause
        offsetClause' = fromMaybe "" offsetClause
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
filterWhere (name, value) = FilterByQueryBuilder (name, toEqOrIsOperator value, toField value)
{-# INLINE filterWhere #-}

filterWhereIn :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, [value]) -> QueryBuilder table -> QueryBuilder table
filterWhereIn (name, value) = FilterByQueryBuilder (name, InOp, toField (In value))
{-# INLINE filterWhereIn #-}

filterWhereNotIn :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, [value]) -> QueryBuilder table -> QueryBuilder table
filterWhereNotIn (_, []) = id -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotIn (name, value) = FilterByQueryBuilder (name, NotInOp, toField (In value))
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
{-# INLINE filterWhereSql #-}
filterWhereSql :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table) => (Proxy name, ByteString) -> QueryBuilder table -> QueryBuilder table
filterWhereSql (name, sqlCondition) = FilterByQueryBuilder (name, SqlOp, Plain (ByteStringBuilder.byteString sqlCondition))

data FilterWhereTag

data OrderByTag

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
orderByAsc :: (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByAsc !name = OrderByQueryBuilder (name, Asc)
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
orderByDesc :: (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByDesc !name = OrderByQueryBuilder (name, Desc)
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
limit !limit = LimitQueryBuilder limit
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
offset !offset = OffsetQueryBuilder offset
{-# INLINE offset #-}

{-# INLINE findBy #-}
findBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

{-# INLINE findMaybeBy #-}
findMaybeBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

--findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), KnownSymbol name, ToField value, HasField name value model) => Proxy name -> value -> QueryBuilder model -> IO [model]
{-# INLINE findManyBy #-}
findManyBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #templateId id

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
queryUnion = UnionQueryBuilder
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
queryOr a b queryBuilder = a queryBuilder `UnionQueryBuilder` b queryBuilder
{-# INLINE queryOr #-}

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Id' table) model where
    type FetchResult (Id' table) model = model
    {-# INLINE fetch #-}
    fetch = genericFetchIdOne
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Maybe (Id' table)) model where
    type FetchResult (Maybe (Id' table)) model = [model]
    {-# INLINE fetch #-}
    fetch (Just a) = genericFetchId a
    fetch Nothing = pure []
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing Nothing = pure Nothing
    fetchOneOrNothing (Just a) = genericfetchIdOneOrNothing a
    {-# INLINE fetchOne #-}
    fetchOne (Just a) = genericFetchIdOne a
    fetchOne Nothing = error "Fetchable (Maybe Id): Failed to fetch because given id is 'Nothing', 'Just id' was expected"

instance (model ~ GetModelById (Id' table), value ~ Id' table, HasField "id" model value, ToField (PrimaryKey table), GetModelByTableName (GetTableName model) ~ model) => Fetchable [Id' table] model where
    type FetchResult [Id' table] model = [model]
    {-# INLINE fetch #-}
    fetch = genericFetchIds
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdsOne

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
distinctOn :: (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table) => Proxy name -> QueryBuilder table -> QueryBuilder table
distinctOn !name = DistinctOnQueryBuilder name
{-# INLINE distinctOn #-}
