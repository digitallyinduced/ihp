{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs #-}
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
  , include
  , genericFetchIds
  , genericfetchIdsOneOrNothing
  , genericFetchIdsOne
  , EqOrIsOperator
  , fetchCount
  , filterWhereSql
  , fetchExists
  , FilterPrimaryKey (..)
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

-- | Represent's a @SELECT * FROM ..@ query. It's the starting point to build a query.
-- Used togehter with the other functions to compose a sql query.
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
query :: forall model. DefaultScope model => QueryBuilder model
query = defaultScope NewQueryBuilder
{-# INLINE query #-}

class DefaultScope model where
    defaultScope :: QueryBuilder model -> QueryBuilder model

instance {-# OVERLAPPABLE #-} DefaultScope model where
    {-# INLINE defaultScope #-}
    defaultScope queryBuilder = queryBuilder

instance Default (QueryBuilder model) where
    {-# INLINE def #-}
    def = NewQueryBuilder

data FilterOperator = EqOp | InOp | NotInOp | IsOp | SqlOp deriving (Show, Eq)


{-# INLINE compileOperator #-}
compileOperator _ EqOp = "="
compileOperator _ InOp = "IN"
compileOperator _ NotInOp = "NOT IN"
compileOperator _ IsOp = "IS"
compileOperator _ SqlOp = ""

data QueryBuilder model where
    NewQueryBuilder :: QueryBuilder model
    FilterByQueryBuilder :: (KnownSymbol field) => !(Proxy field, FilterOperator, Action) -> !(QueryBuilder model) -> QueryBuilder model
    OrderByQueryBuilder :: KnownSymbol field => !(Proxy field, OrderByDirection) -> !(QueryBuilder model) -> QueryBuilder model
    LimitQueryBuilder :: Int -> !(QueryBuilder model) -> QueryBuilder model
    OffsetQueryBuilder :: Int -> !(QueryBuilder model) -> QueryBuilder model
    IncludeQueryBuilder :: (KnownSymbol field, KnownSymbol (GetTableName model)) => !(Proxy field, QueryBuilder relatedModel) -> !(QueryBuilder model) -> QueryBuilder (Include field model)
    UnionQueryBuilder :: !(QueryBuilder model) -> !(QueryBuilder model) -> QueryBuilder model

data Condition = VarCondition !Text !Action | OrCondition !Condition !Condition | AndCondition !Condition !Condition deriving (Show)

deriving instance Show (QueryBuilder a)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol (GetTableName a) => ToHtml (QueryBuilder a) where
    toHtml queryBuilder = toHtml (toSQL queryBuilder)

-- | This hack is to allow Eq instances for models with hasMany relations
instance Eq (IHP.QueryBuilder.QueryBuilder model) where a == b = True

data OrderByDirection = Asc | Desc deriving (Eq, Show)
data SQLQuery = SQLQuery {
        selectFrom :: !Text,
        whereCondition :: !(Maybe Condition),
        orderByClause :: !([(Text, OrderByDirection)]),
        limitClause :: !(Maybe Text),
        offsetClause :: !(Maybe Text)
    }

{-# INLINE buildQuery #-}
buildQuery :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> SQLQuery
buildQuery !queryBuilder =
    case queryBuilder of
        NewQueryBuilder ->
            let tableName = symbolVal @(GetTableName model) Proxy
            in SQLQuery { selectFrom = cs tableName, whereCondition = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing }
        FilterByQueryBuilder (fieldProxy, operator, value) queryBuilder ->
            let
                query = buildQuery queryBuilder
                condition = VarCondition ((fieldNameToColumnName . cs $ symbolVal fieldProxy) <> " " <> compileOperator fieldProxy operator <> " ?") value
            in
                query { whereCondition = Just $ case whereCondition query of Just c -> AndCondition c condition; Nothing -> condition }
        OrderByQueryBuilder (fieldProxy, orderByDirection) queryBuilder ->
            let query = buildQuery queryBuilder
            in query { orderByClause = (orderByClause query) ++ [(fieldNameToColumnName . cs $ symbolVal fieldProxy, orderByDirection)] } -- although adding to the end of a list is bad form, these lists are very short
        LimitQueryBuilder limit queryBuilder -> (buildQuery queryBuilder) { limitClause = Just ("LIMIT " <> tshow limit) }
        OffsetQueryBuilder offset queryBuilder -> (buildQuery queryBuilder) { offsetClause = Just ("OFFSET " <> tshow offset) }
        IncludeQueryBuilder include queryBuilder -> buildQuery queryBuilder
        UnionQueryBuilder firstQueryBuilder secondQueryBuilder ->
            let
                firstQuery = buildQuery firstQueryBuilder
                secondQuery = buildQuery secondQueryBuilder
                isSimpleQuery query = null (orderByClause query) && isNothing (limitClause query)
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

instance Fetchable (QueryBuilder model) model where
    type FetchResult (QueryBuilder model) model = [model]
    {-# INLINE fetch #-}
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => QueryBuilder model -> IO [model]
    fetch !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder)
        logQuery theQuery theParameters
        trackTableRead (tableName @model)
        sqlQuery (Query $ cs theQuery) theParameters

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO (Maybe model)
    fetchOneOrNothing !queryBuilder = do
        let !(theQuery, theParameters) = toSQL' (buildQuery queryBuilder) { limitClause = Just "LIMIT 1"}
        logQuery theQuery theParameters
        trackTableRead (tableName @model)
        results <- sqlQuery (Query $ cs theQuery) theParameters
        pure $ listToMaybe results

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO model
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
fetchCount :: forall model. (?modelContext :: ModelContext, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO Int
fetchCount !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT COUNT(*) FROM (" <> theQuery' <> ") AS _count_values"
    logQuery theQuery theParameters
    trackTableRead (tableName @model)
    [PG.Only count] <- sqlQuery (Query $! cs theQuery) theParameters
    pure count
{-# INLINE fetchCount #-}

-- | Checks whether the query has any results.
--
-- Returns @True@ when there is atleast one row matching the conditions of the query. Returns @False@ otherwise.
--
-- __Example:__ Checking whether there are unread messages
--
-- >     hasUnreadMessages <- query @Message
-- >         |> filterWhere (#isUnread, True)
-- >         |> fetchExists
-- >     -- SELECT EXISTS (SELECT * FROM messages WHERE is_unread = true)
fetchExists :: forall model. (?modelContext :: ModelContext, KnownSymbol (GetTableName model)) => QueryBuilder model -> IO Bool
fetchExists !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT EXISTS (" <> theQuery' <> ") AS _exists_values"
    logQuery theQuery theParameters
    trackTableRead (tableName @model)
    [PG.Only exists] <- sqlQuery (Query $! cs theQuery) theParameters
    pure exists
{-# INLINE fetchExists #-}

{-# INLINE genericFetchId #-}
genericFetchId :: forall model. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey model) => Id model -> IO [model]
genericFetchId !id = query @model |> filterWhereId id |> fetch
{-# INLINE genericfetchIdOneOrNothing #-}
genericfetchIdOneOrNothing :: forall model. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey model) => Id model -> IO (Maybe model)
genericfetchIdOneOrNothing !id = query @model |> filterWhereId id |> fetchOneOrNothing
{-# INLINE genericFetchIdOne #-}
genericFetchIdOne :: forall model. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey model) => Id model -> IO model
genericFetchIdOne !id = query @model |> filterWhereId id |> fetchOne

{-# INLINE genericFetchIds #-}
genericFetchIds :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value) => [value] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIn (#id, ids) |> fetch
{-# INLINE genericfetchIdsOneOrNothing #-}
genericfetchIdsOneOrNothing :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value) => [value] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIn (#id, ids) |> fetchOneOrNothing
{-# INLINE genericFetchIdsOne #-}
genericFetchIdsOne :: forall model value. (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value) => [value] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIn (#id, ids) |> fetchOne

toSQL :: forall model. (KnownSymbol (GetTableName model)) => QueryBuilder model -> (Text, [Action])
toSQL queryBuilder = toSQL' (buildQuery queryBuilder)
toSQL' sqlQuery@SQLQuery { selectFrom, orderByClause, limitClause, offsetClause } =
        (theQuery, theParams)
    where
        !theQuery =
            "SELECT " <> selectors <> " FROM "
            <> fromClause
            <> whereConditions' <> " "
            <> orderByClause' <> " "
            <> limitClause'
            <> offsetClause'

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
                [] -> mempty
                xs -> " ORDER BY " <> intercalate "," ((map (\(column,direction) -> column <> (if direction == Desc then " DESC" else mempty)) xs))
        limitClause' = fromMaybe "" limitClause
        offsetClause' = fromMaybe "" offsetClause

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

-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp


class FilterPrimaryKey model where
    filterWhereId :: Id model -> QueryBuilder model -> QueryBuilder model

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
filterWhere :: forall name model value. (KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value) => (Proxy name, value) -> QueryBuilder model -> QueryBuilder model
filterWhere (name, value) = FilterByQueryBuilder (name, toEqOrIsOperator value, toField value)
{-# INLINE filterWhere #-}

filterWhereIn :: forall name model value. (KnownSymbol name, ToField value, HasField name model value) => (Proxy name, [value]) -> QueryBuilder model -> QueryBuilder model
filterWhereIn (name, value) = FilterByQueryBuilder (name, InOp, toField (In value))
{-# INLINE filterWhereIn #-}

filterWhereNotIn :: forall name model value. (KnownSymbol name, ToField value, HasField name model value) => (Proxy name, [value]) -> QueryBuilder model -> QueryBuilder model
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
filterWhereSql :: forall name model value. (KnownSymbol name, ToField value, HasField name model value) => (Proxy name, ByteString) -> QueryBuilder model -> QueryBuilder model
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
orderByAsc :: (KnownSymbol name, HasField name model value) => Proxy name -> QueryBuilder model -> QueryBuilder model
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
orderByDesc :: (KnownSymbol name, HasField name model value) => Proxy name -> QueryBuilder model -> QueryBuilder model
orderByDesc !name = OrderByQueryBuilder (name, Desc)
{-# INLINE orderByDesc #-}

-- | Alias for 'orderByAsc'
orderBy :: (KnownSymbol name, HasField name model value) => Proxy name -> QueryBuilder model -> QueryBuilder model
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


data IncludeTag
include :: forall name model fieldType relatedModel. (KnownSymbol name, KnownSymbol (GetTableName model), HasField name model fieldType, relatedModel ~ GetModelById fieldType) => KnownSymbol name => Proxy name -> QueryBuilder model -> QueryBuilder (Include name model)
include !name = IncludeQueryBuilder (name, relatedQueryBuilder)
    where
        relatedQueryBuilder = query @relatedModel

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

instance (model ~ GetModelById (Id' model'), GetTableName model ~ model', FilterPrimaryKey model) => Fetchable (Id' model') model where
    type FetchResult (Id' model') model = model
    {-# INLINE fetch #-}
    fetch = genericFetchIdOne
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' model'), GetTableName model ~ model', FilterPrimaryKey model) => Fetchable (Maybe (Id' model')) model where
    type FetchResult (Maybe (Id' model')) model = [model]
    {-# INLINE fetch #-}
    fetch (Just a) = genericFetchId a
    fetch Nothing = pure []
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing Nothing = pure Nothing
    fetchOneOrNothing (Just a) = genericfetchIdOneOrNothing a
    {-# INLINE fetchOne #-}
    fetchOne (Just a) = genericFetchIdOne a
    fetchOne Nothing = error "Fetchable (Maybe Id): Failed to fetch because given id is 'Nothing', 'Just id' was expected"

instance (model ~ GetModelById (Id' model'), value ~ Id' model', HasField "id" model value, ToField (PrimaryKey model')) => Fetchable [Id' model'] model where
    type FetchResult [Id' model'] model = [model]
    {-# INLINE fetch #-}
    fetch = genericFetchIds
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdsOne