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
, QueryBuilder (..)
, In (In)
, orderBy
, orderByAsc
, orderByDesc
, limit
, offset
, queryUnion
, queryOr
, DefaultScope (..)
, filterWhere
, filterWhereCaseInsensitive
, filterWhereNot
, filterWhereIn
, filterWhereNotIn
, filterWhereLike
, filterWhereILike
, filterWhereMatches
, filterWhereIMatches
, filterWhereJoinedTable
, filterWhereNotJoinedTable
, filterWhereInJoinedTable
, filterWhereNotInJoinedTable
, filterWhereLikeJoinedTable
, filterWhereILikeJoinedTable
, filterWhereMatchesJoinedTable
, filterWhereIMatchesJoinedTable
, labelResults
, EqOrIsOperator
, filterWhereSql
, FilterPrimaryKey (..)
, distinctOn
, distinct
, toSQL
, toSQL'
, buildQuery
, SQLQuery (..)
, OrderByClause (..)
, OrderByDirection (..)
, Condition (..)
, innerJoin
, innerJoinThirdTable
, HasQueryBuilder
, JoinQueryBuilderWrapper
, NoJoinQueryBuilderWrapper
, LabeledQueryBuilderWrapper
, getQueryBuilder
, NoJoins
)
where
import qualified Prelude
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
import IHP.HSX.ToHtml
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text.Encoding as Text
import Debug.Trace

class DefaultScope table where
    defaultScope :: QueryBuilder table -> QueryBuilder table

instance {-# OVERLAPPABLE #-} DefaultScope table where
    {-# INLINE defaultScope #-}
    defaultScope queryBuilder = queryBuilder

instance Default (QueryBuilder table) where
    {-# INLINE def #-}
    def = NewQueryBuilder

data MatchSensitivity = CaseSensitive | CaseInsensitive deriving (Show, Eq)

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
    | SqlOp -- ^ Used by 'filterWhereSql'
    deriving (Show, Eq)


compileOperator :: FilterOperator -> ByteString
compileOperator EqOp = "="
compileOperator NotEqOp = "!="
compileOperator InOp = "IN"
compileOperator NotInOp = "NOT IN"
compileOperator IsOp = "IS"
compileOperator IsNotOp = "IS NOT"
compileOperator (LikeOp CaseSensitive) = "LIKE"
compileOperator (LikeOp CaseInsensitive) = "ILIKE"
compileOperator (NotLikeOp CaseSensitive) = "NOT LIKE"
compileOperator (NotLikeOp CaseInsensitive) = "NOT ILIKE"
compileOperator (MatchesOp CaseSensitive) = " ~ "
compileOperator (MatchesOp CaseInsensitive) = " ~* "
compileOperator SqlOp = ""
{-# INLINE compileOperator #-}

-- | Returns the "NOT" version of an operator
--
-- >>> negateFilterOperator EqOp
-- NotEqOp
--
negateFilterOperator :: FilterOperator -> FilterOperator
negateFilterOperator EqOp = NotEqOp
negateFilterOperator InOp = NotInOp
negateFilterOperator IsOp = IsNotOp
negateFilterOperator (LikeOp matchSensitivity) = (NotLikeOp matchSensitivity)
negateFilterOperator (MatchesOp matchSensitivity) = error "not supported"
negateFilterOperator SqlOp = SqlOp

data OrderByClause =
    OrderByClause
    { orderByColumn :: !ByteString
    , orderByDirection :: !OrderByDirection }
    deriving (Show, Eq)

-- Types implementing a type level list to record joined tables. EmptyModelList and ConsModelList correspond to the data constructors [] and :. NoJoins is like the empty List but cannot be extended.
data NoJoins
data EmptyModelList
data ConsModelList model models

-- Type class to represent the true list type EmptyModelList ConsModelList.
class ModelList a

instance ModelList EmptyModelList
instance ModelList (ConsModelList model models)

-- Typeclass to quer containment in the type-level list.
class IsJoined a b

instance (ModelList b) => IsJoined a (ConsModelList a b)
instance {-# OVERLAPPABLE #-} (ModelList b, IsJoined a b) => IsJoined a (ConsModelList c b)

-- Class to generalise over different QueryBuilder-providing types. The actual query builder can be extracted with 'getQueryBuilder' and injected with 'injectQueryBuilder'. Also assigns a join reqister to a queryBilderProvider. 
class HasQueryBuilder queryBuilderProvider joinRegister | queryBuilderProvider -> joinRegister where
    getQueryBuilder :: queryBuilderProvider table -> QueryBuilder table
    injectQueryBuilder :: QueryBuilder table -> queryBuilderProvider table
    getQueryIndex :: queryBuilderProvider table -> Maybe ByteString
    getQueryIndex _ = Nothing 

-- Wrapper for QueryBuilders resulting from joins. Associates a joinRegister type.
newtype JoinQueryBuilderWrapper joinRegister table = JoinQueryBuilderWrapper (QueryBuilder table)

-- Wrapper for QueryBuilder that must not joins, e.g. queryUnion.
newtype NoJoinQueryBuilderWrapper table = NoJoinQueryBuilderWrapper (QueryBuilder table)

-- Wrapper for QueryBuilders with indexed results.
newtype LabeledQueryBuilderWrapper foreignTable indexColumn indexValue table = LabeledQueryBuilderWrapper (QueryBuilder table)

-- QueryBuilders have query builders and the join register is empty.
instance HasQueryBuilder QueryBuilder EmptyModelList where
    getQueryBuilder = id
    injectQueryBuilder = id

-- JoinQueryBuilderWrappers have query builders
instance HasQueryBuilder (JoinQueryBuilderWrapper joinRegister) joinRegister where
    getQueryBuilder (JoinQueryBuilderWrapper queryBuilder) = queryBuilder
    injectQueryBuilder = JoinQueryBuilderWrapper 

-- NoJoinQueryBuilderWrapper have query builders and the join register does not allow any joins
instance HasQueryBuilder NoJoinQueryBuilderWrapper NoJoins where
    getQueryBuilder (NoJoinQueryBuilderWrapper queryBuilder) = queryBuilder
    injectQueryBuilder  = NoJoinQueryBuilderWrapper 

instance (KnownSymbol foreignTable, foreignModel ~ GetModelByTableName foreignTable , KnownSymbol indexColumn, HasField indexColumn foreignModel indexValue) => HasQueryBuilder (LabeledQueryBuilderWrapper foreignTable indexColumn indexValue) NoJoins where
    getQueryBuilder (LabeledQueryBuilderWrapper queryBuilder) = queryBuilder
    injectQueryBuilder = LabeledQueryBuilderWrapper
    getQueryIndex _ = Just $ symbolToByteString @foreignTable <> "." <> (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @indexColumn)


data QueryBuilder (table :: Symbol) =
    NewQueryBuilder
    | DistinctQueryBuilder   { queryBuilder :: !(QueryBuilder table) }
    | DistinctOnQueryBuilder { queryBuilder :: !(QueryBuilder table), distinctOnColumn :: !ByteString }
    | FilterByQueryBuilder   { queryBuilder :: !(QueryBuilder table), queryFilter :: !(ByteString, FilterOperator, Action), applyLeft :: !(Maybe ByteString), applyRight :: !(Maybe ByteString) }
    | OrderByQueryBuilder    { queryBuilder :: !(QueryBuilder table), queryOrderByClause :: !OrderByClause }
    | LimitQueryBuilder      { queryBuilder :: !(QueryBuilder table), queryLimit :: !Int }
    | OffsetQueryBuilder     { queryBuilder :: !(QueryBuilder table), queryOffset :: !Int }
    | UnionQueryBuilder      { firstQueryBuilder :: !(QueryBuilder table), secondQueryBuilder :: !(QueryBuilder table) }
    | JoinQueryBuilder       { queryBuilder :: !(QueryBuilder table), joinData :: Join}
    deriving (Show, Eq)

data Condition = VarCondition !ByteString !Action | OrCondition !Condition !Condition | AndCondition !Condition !Condition deriving (Show, Eq)

-- | Display QueryBuilder's as their sql query inside HSX
instance KnownSymbol table => ToHtml (QueryBuilder table) where
    toHtml queryBuilder = toHtml (toSQL queryBuilder)

data Join = Join { table :: ByteString, tableJoinColumn :: ByteString, otherJoinColumn :: ByteString }
    deriving (Show, Eq)

data OrderByDirection = Asc | Desc deriving (Eq, Show)
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

instance SetField "queryIndex" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { queryIndex = value }
instance SetField "selectFrom" SQLQuery ByteString where setField value sqlQuery = sqlQuery { selectFrom = value }
instance SetField "distinctClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctClause = value }
instance SetField "distinctOnClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { distinctOnClause = value }
instance SetField "whereCondition" SQLQuery (Maybe Condition) where setField value sqlQuery = sqlQuery { whereCondition = value }
instance SetField "orderByClause" SQLQuery [OrderByClause] where setField value sqlQuery = sqlQuery { orderByClause = value }
instance SetField "limitClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { limitClause = value }
instance SetField "offsetClause" SQLQuery (Maybe ByteString) where setField value sqlQuery = sqlQuery { offsetClause = value }



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

{-# INLINE buildQuery #-}
buildQuery :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> SQLQuery
buildQuery queryBuilderProvider = buildQueryHelper $ getQueryBuilder queryBuilderProvider
    where
    buildQueryHelper NewQueryBuilder =
        let tableName = symbolToByteString @table
        in SQLQuery
            {     queryIndex = trace (Prelude.show $ getQueryIndex queryBuilderProvider) getQueryIndex queryBuilderProvider 
                , selectFrom = tableName
                , distinctClause = Nothing
                , distinctOnClause = Nothing
                , whereCondition = Nothing
                , joins = []
                , orderByClause = []
                , limitClause = Nothing
                , offsetClause = Nothing
                }
    buildQueryHelper DistinctQueryBuilder { queryBuilder } = queryBuilder
            |> buildQueryHelper
            |> setJust #distinctClause "DISTINCT"
    buildQueryHelper DistinctOnQueryBuilder { queryBuilder, distinctOnColumn } = queryBuilder
            |> buildQueryHelper
            |> setJust #distinctOnClause ("DISTINCT ON (" <> distinctOnColumn <> ")")
    buildQueryHelper FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, operator, value), applyLeft, applyRight } =
                let
                    applyFn fn value = case fn of
                            Just fn -> fn <> "(" <> value <> ")"
                            Nothing -> value

                    condition = VarCondition (applyFn applyLeft columnName <> " " <> compileOperator operator <> " " <> applyFn applyRight "?") value
                in
                    queryBuilder
                        |> buildQueryHelper
                        |> modify #whereCondition \case
                                Just c -> Just (AndCondition c condition)
                                Nothing -> Just condition
    buildQueryHelper OrderByQueryBuilder { queryBuilder, queryOrderByClause } = queryBuilder
            |> buildQueryHelper
            |> modify #orderByClause (\value -> value <> [queryOrderByClause] ) -- although adding to the end of a list is bad form, these lists are very short
    buildQueryHelper LimitQueryBuilder { queryBuilder, queryLimit } =
                    queryBuilder
                    |> buildQueryHelper
                    |> setJust #limitClause (
                            (Builder.byteString "LIMIT " <> Builder.intDec queryLimit)
                            |> Builder.toLazyByteString
                            |> LByteString.toStrict
                        )
    buildQueryHelper OffsetQueryBuilder { queryBuilder, queryOffset } = queryBuilder
            |> buildQueryHelper
            |> setJust #offsetClause (
                    (Builder.byteString "OFFSET " <> Builder.intDec queryOffset)
                    |> Builder.toLazyByteString
                    |> LByteString.toStrict
                )
    buildQueryHelper UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder } =
                let
                    firstQuery = buildQueryHelper firstQueryBuilder
                    secondQuery = buildQueryHelper secondQueryBuilder
                    isSimpleQuery query = null (orderByClause query) && isNothing (limitClause query) && isNothing (offsetClause query) && null (joins query)
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
    
    buildQueryHelper JoinQueryBuilder { queryBuilder, joinData } =
        let 
            firstQuery = buildQueryHelper queryBuilder
         in firstQuery { joins = joinData:joins firstQuery }
    
-- | Transforms a @query @@User |> ..@ expression into a SQL Query. Returns a tuple with the sql query template and it's placeholder values.
--
-- __Example:__ Get the sql query that is represented by a QueryBuilder
--
-- >>> let postsQuery = query @Post |> filterWhere (#public, True)
-- >>> toSQL postsQuery
-- ("SELECT posts.* FROM posts WHERE public = ?", [Plain "true"])
toSQL :: (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> (ByteString, [Action])
toSQL queryBuilderProvider = toSQL' (buildQuery queryBuilderProvider)
{-# INLINE toSQL #-}

toSQL' :: SQLQuery -> (ByteString, [Action])
toSQL' sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause } =
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
                    , joinClause
                    , whereConditions'
                    , orderByClause'
                    , limitClause
                    , offsetClause
                    ]

        selectors :: ByteString
        selectors = ByteString.intercalate ", " $ catMaybes [queryIndex , Just (selectFrom <> ".*")]

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
        joinClause :: Maybe ByteString
        joinClause = buildJoinClause $ reverse $ joins sqlQuery
        buildJoinClause :: [Join] -> Maybe ByteString
        buildJoinClause [] = Nothing
        buildJoinClause (joinClause:joinClauses) = Just $ "INNER JOIN " <> table joinClause <> " ON " <> tableJoinColumn joinClause <> " = " <>table joinClause <> "." <> otherJoinColumn joinClause <> maybe "" (" " <>) (buildJoinClause joinClauses)


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
-- > projects <- query @Project
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
-- For @WHERE x LIKE a@ or @WHERE x ~ a@  conditions, see 'filterWhereLike' and 'filterWhereMatches' respectively.
-- For case-insensitive versions of these operators, see 'filterWhereILike' and 'filterWhereIMatches'.
--
-- When your condition is too complex, use a raw sql query with 'IHP.ModelSupport.sqlQuery'.
filterWhere :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhere (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhere #-}

-- | Like 'filterWhere', but takes a type argument specifying the table which holds the column that is to be compared. The column must have been joined before using 'innerJoin' or 'innerJoinThirdTable'. Example: 
--
-- __Example:__ get posts by user Tom.
--
-- > tomPosts <- query @Post
-- >                    |> innerJoin @User (#createdBy, #id)
-- >                    |> filterWhereJoinedTable @User (#name, "Tom" :: Text)
-- >                    |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name = 'Tom'
--
filterWhereJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereJoinedTable #-}

-- | Like 'filterWhere' but negates the condition.
--
-- __Example:__ Only show projects created by other users.
--
-- > activeProjects <- query @Project
-- >     |> filterWhereNot (#userId, currentUserId)
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE user_id != '23d5ea33-b28e-4f0a-99b3-77a3564a2546'
--
filterWhereNot :: forall name table model value. (KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table) => (Proxy name, value) -> QueryBuilder table -> QueryBuilder table
filterWhereNot (name, value) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, negateFilterOperator (toEqOrIsOperator value), toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhereNot #-}

-- | Like 'filterWhereNotJoinedTable' but negates the condition.
--
-- __Example:__ Only show projects not created by user Tom.
--
-- > tomPosts <- query @Post
-- >                    |> innerJoin @User (#createdBy, #id)
-- >                    |> filterWhereNotJoinedTable @User (#name, "Tom" :: Text)
-- >                    |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name = 'Tom'
--
filterWhereNotJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, negateFilterOperator (toEqOrIsOperator value), toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotJoinedTable #-}
-- | Adds a @WHERE x IN (y)@ condition to the query.
--
-- __Example:__ Only show projects where @status@ is @Draft@ or @Active@.
--
-- > visibleProjects <- query @Project
-- >     |> filterWhereIn (#status, [Draft, Active])
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE status IN ('draft', 'active')
--
-- For negation use 'filterWhereNotIn'
--
filterWhereIn :: forall name table model value queryBuilderProvider (joinRegister :: *). (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIn (name, value) queryBuilderProvider =
        case head nullValues of
            Nothing -> injectQueryBuilder whereInQuery -- All values non null
            Just nullValue ->
                let
                    isNullValueExpr = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, IsOp, toField nullValue), applyLeft = Nothing, applyRight = Nothing }
                in
                    case head nonNullValues of
                        Just nonNullValue -> -- Some non null values, some null values
                            injectQueryBuilder $ UnionQueryBuilder
                                (injectQueryBuilder whereInQuery)
                                (injectQueryBuilder isNullValueExpr)
                        Nothing -> injectQueryBuilder isNullValueExpr -- All values null
    where
        -- Only NOT NULL values can be compares inside the IN expression, NULL values have to be compares using a manual appended IS expression
        -- https://github.com/digitallyinduced/ihp/issues/906
        --
        (nonNullValues, nullValues) = value |> partition (\v -> toEqOrIsOperator v == EqOp)

        whereInQuery = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, InOp, toField (In nonNullValues)), applyLeft = Nothing, applyRight = Nothing }

        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIn #-}

-- | Like 'filterWhereIn', but takes a type argument specifying the table which holds the column that is compared. The table needs to have been joined before using 'innerJoin' or 'innerJoinThirdTable'.
-- 
-- __Example:__ get posts by Tom and Tim.
--
-- > tomOrTimPosts <- query @Post 
-- >    |> innerJoin @User (#createdBy, #id) 
-- >    |> filterWhereInJoinedTable @User (#name, ["Tom","Tim"]) 
-- >    |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name IN ('Tom', 'Tim') 
--
filterWhereInJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereInJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, InOp, toField (In value)), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereInJoinedTable #-}


-- | Adds a @WHERE x NOT IN (y)@ condition to the query.
--
-- __Example:__ Only show projects where @status@ is not @Archived@
--
-- > visibleProjects <- query @Project
-- >     |> filterWhereNotIn (#status, [Archived])
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE status NOT IN ('archived')
--
-- The inclusive version of this function is called 'filterWhereIn'.
--
filterWhereNotIn :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereNotIn (_, []) queryBuilder = queryBuilder -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotIn (name, value) queryBuilderProvider =
        case head nullValues of
            Nothing -> injectQueryBuilder whereNotInQuery -- All values non null
            Just nullValue ->
                case head nonNullValues of
                    Just nonNullValue -> injectQueryBuilder FilterByQueryBuilder { queryBuilder = whereNotInQuery, queryFilter = (columnName, IsNotOp, toField nullValue), applyLeft = Nothing, applyRight = Nothing } -- Some non null values, some null values
                    Nothing -> injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, IsNotOp, toField nullValue), applyLeft = Nothing, applyRight = Nothing } -- All values null
    where
        -- Only NOT NULL values can be compares inside the IN expression, NULL values have to be compares using a manual appended IS expression
        -- https://github.com/digitallyinduced/ihp/issues/906
        --
        (nonNullValues, nullValues) = value |> partition (\v -> toEqOrIsOperator v == EqOp)

        whereNotInQuery = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, NotInOp, toField (In nonNullValues)), applyLeft = Nothing, applyRight = Nothing }

        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotIn #-}

-- | Like 'filterWhereNotIn', but takes a type argument specifying the table which holds the column that is compared. The table needs to have been joined before using 'innerJoin' or 'innerJoinThirdTable'.
--
-- __Example:__ get posts by users not named Tom or Tim.
--
-- > notTomOrTimPosts <- query @Post
-- >    |> innerJoin @User (#createdBy, #id)
-- >    |> filterWhereNotInJoinedTable @User (#name, ["Tom","Tim"])
-- >    |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name NOT IN ('Tom', 'Tim')
filterWhereNotInJoinedTable :: forall model name table  value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotInJoinedTable (_, []) queryBuilderProvider = queryBuilderProvider -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotInJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, NotInOp, toField (In value)), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotInJoinedTable #-}


-- | Adds a @WHERE x LIKE y@ condition to the query.
--
-- __Example:__ Find titles matching search term.
--
-- > articles <- query @Article
-- >     |> filterWhereLike (#title, "%" <> searchTerm <> "%")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title LIKE '%..%'
filterWhereLike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLike (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseSensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLike #-}

-- | Like 'filterWhereLik'e, but takes a type argument specifying the table which holds the column that is compared. The table needs to have been joined before using 'innerJoin' or 'innerJoinThirdTable'.
--
-- __Example:__ Serach for Posts by users whose name contains "olaf" (case insensitive)
--
-- > olafPosts <- query @Post  
-- >                |> innerJoin @User (#createdBy, #id)  
-- >                |> filterWhereLikeJoinedTable @User (#name, "%Olaf%")
-- >                |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name LIKE '%Olaf%'
filterWhereLikeJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol name, KnownSymbol table, table ~ GetTableName model, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereLikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseSensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLikeJoinedTable #-}


-- | Adds a @WHERE x ILIKE y@ condition to the query. Case-insensitive version of 'filterWhereLike'.
--
-- __Example:__ Find titles matching search term.
--
-- > articles <- query @Article
-- >     |> filterWhereILike (#title, "%" <> searchTerm <> "%")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title ILIKE '%..%'
filterWhereILike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereILike (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseInsensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereILike #-}

-- | Like 'filterWhereILike'; case-insensitive version of filterWhereLikeJoinedTable, takes a type argument specifying the table which holds the column that is compared. The table needs to have been joined before using 'innerJoin' or 'innerJoinThirdTable'.
--
-- __Example:__ Serach for Posts by users whose name contains "olaf" (case insensitive)
--
-- > olafPosts <- 
-- >    query @Post  
--      |> innerJoin @User (#createdBy, #id) 
--      |> filterWhereILikeJoinedTable @User (#name, "%Olaf%")
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name ILIKE '%Olaf%' 
filterWhereILikeJoinedTable :: forall model table name table' model' value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, model' ~ GetModelByTableName table', HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereILikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseInsensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereILikeJoinedTable #-}


-- | Adds a @WHERE x ~ y@ condition to the query.
--
-- __Example:__ Find names with titles in front.
--
-- > articles <- query @User
-- >     |> filterWhereMatches (#name, "^(M(rs|r|iss)|Dr|Sir). ")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title ~ '^(M(rs|r|iss)|Dr|Sir). '
filterWhereMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereMatches (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseSensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereMatches #-}

-- | Adds a @WHERE x ~ y@ condition to the query, where the column x is held by a joined table.
--
-- __Example:__ Find Posts by people with names with titles in front.
--
-- > articles <- query @Post
-- >     |> innerJoin @User (#createdBy, #id)
-- >     |> filterWhereMatchesJoinedTable (#title, "^(M(rs|r|iss|s)|Dr|Sir). ")
-- >     |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.title ~ '^(M(rs|r|iss|s)|Dr|Sir). '

filterWhereMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseSensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereMatchesJoinedTable #-}


-- | Adds a @WHERE x ~* y@ condition to the query. Case-insensitive version of 'filterWhereMatches'.
filterWhereIMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIMatches (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseInsensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIMatches #-}

-- | Case-insensitive version of 'filterWhereMatchesJoinedTable'
filterWhereIMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, ToField value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereIMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseInsensitive, toField value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIMatchesJoinedTable #-}


-- | Allows to add a custom raw sql where condition
--
-- If your query cannot be represented with 'filterWhereSql', take a look at 'IHP.ModelSupport.sqlQuery'.
--
-- __Example:__ Fetching all projects created in the last 24 hours.
--
-- > latestProjects <- query @Project
-- >     |> filterWhereSql (#startedAt, "< current_timestamp - interval '1 day'")
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE started_at < current_timestamp - interval '1 day'
--
filterWhereSql :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, ByteString) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereSql (name, sqlCondition) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, SqlOp, Plain (Builder.byteString sqlCondition)), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereSql #-}

-- | Adds a @WHERE LOWER(x) = LOWER(y)@ condition to the query.
--
-- __Example:__ Get a user by an email address, ignoring case
--
-- > user <- query @User
-- >     |> filterWhereCaseInsensitive (#email, "marc@digitallyinduced.com")
-- >     |> fetchOne
-- > -- SELECT * FROM users WHERE LOWER(email) = 'marc@digitallyinduced.com'
--
-- For high performance it's best to have an index for @LOWER(field)@ in your Schema.sql
--
-- >>> CREATE UNIQUE INDEX users_email_index ON users ((LOWER(email)));
--
filterWhereCaseInsensitive :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol name, ToField value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereCaseInsensitive (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, toField value), applyLeft = Just "LOWER", applyRight = Just "LOWER" }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereCaseInsensitive #-}

-- | Joins a table to an existing QueryBuilder (or something holding a QueryBuilder) on the specified columns. Example:
-- >    query @Posts 
-- > |> innerJoin @Users (#author, #id)
-- > -- SELECT users.* FROM users INNER JOIN posts ON users.id = posts.author ...
innerJoin :: forall model' table' name' value' model table name value queryBuilderProvider joinRegister.
                            (
                                KnownSymbol name, 
                                KnownSymbol table,
                                HasField name model value,
                                KnownSymbol name', 
                                KnownSymbol table',
                                HasQueryBuilder queryBuilderProvider joinRegister,
                                ModelList joinRegister,
                                HasField name' model' value', 
                                value ~ value',
                                model ~ GetModelByTableName table,
                                table' ~ GetTableName model'
                            ) => (Proxy name, Proxy name') -> queryBuilderProvider table -> JoinQueryBuilderWrapper (ConsModelList model' joinRegister) table 
innerJoin (name, name') queryBuilderProvider = injectQueryBuilder $ JoinQueryBuilder (getQueryBuilder queryBuilderProvider) $ Join joinTableName leftJoinColumn rightJoinColumn 
    where 
        baseTableName = symbolToByteString @table
        joinTableName = symbolToByteString @table'
        leftJoinColumn = baseTableName <> "." <> (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @name)
        rightJoinColumn = (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @name')
{-# INLINE innerJoin #-}

-- | Index the values from a table with values of a field from a table joined by 'innerJoin' or 'innerJoinThirdTable'. Useful to get, e.g., the tags to a set of posts in such a way that the assignment of tags to posts is preserved.
--
--
-- __Example:__ Fetch a list of all comments, each paired with the id of the post it belongs to.
--
-- > labeledTags <-
-- >  query @Tag
-- >     |> innerJoin @Tagging (#id, #tagId)
-- >     |> innerJoinThirdTable @Post @Tagging (#id, #postId)
-- >     |> labelResults @Post #id
-- >     |> fetch
-- > -- SELECT posts.id, tags.* FROM comments INNER JOIN taggings ON tags.id = taggings.tagId INNER JOIN posts ON posts.id = taggings.postId 
-- 
-- labeledTags is then a list of type ['LabeledData' (Id' "posts") Tag] such that "LabeledData postId tag" is contained in that list if "tag" is a tag of the post with id postId.
--
labelResults :: forall foreignModel baseModel foreignTable baseTable name value queryBuilderProvider joinRegister.
                (
                    KnownSymbol foreignTable,
                    KnownSymbol baseTable,
                    foreignTable ~ GetTableName foreignModel, 
                    baseModel ~ GetModelByTableName baseTable,
                    HasField name foreignModel value,
                    HasQueryBuilder queryBuilderProvider joinRegister,
                    KnownSymbol name,
                    IsJoined foreignModel joinRegister
                ) => Proxy name -> queryBuilderProvider baseTable -> LabeledQueryBuilderWrapper foreignTable name value baseTable
labelResults name queryBuilderProvider = LabeledQueryBuilderWrapper $ getQueryBuilder queryBuilderProvider
                    
-- | Joins a table on a column held by a previously joined table. Example:
-- > query @Posts 
-- > |> innerJoin @Users (#author, #id)
-- > |> innerJoinThirdTable @City @Users (#id, #homeTown)
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.author = users.id INNER JOIN cities ON user.home_town = cities.id
--
innerJoinThirdTable :: forall model model' name name' value value' table table' baseTable baseModel queryBuilderProvider joinRegister.
                        ( 
                            KnownSymbol name,
                            KnownSymbol table,
                            HasField name model value,
                            KnownSymbol name',
                            KnownSymbol table',
                            HasQueryBuilder queryBuilderProvider joinRegister,
                            ModelList joinRegister,
                            HasField name' model' value',
                            value ~ value',
                            table ~ GetTableName model,
                            table' ~ GetTableName model',
                            baseModel ~ GetModelByTableName baseTable 
                        ) => (Proxy name, Proxy name') -> queryBuilderProvider baseTable -> JoinQueryBuilderWrapper (ConsModelList model joinRegister) baseTable
innerJoinThirdTable (name, name') queryBuilderProvider = injectQueryBuilder $ JoinQueryBuilder (getQueryBuilder queryBuilderProvider) $ Join joinTableName leftJoinColumn rightJoinColumn
     where 
        baseTableName = symbolToByteString @table'
        joinTableName = symbolToByteString @table
        leftJoinColumn = baseTableName <> "." <> (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @name')
        rightJoinColumn = (Text.encodeUtf8 . fieldNameToColumnName) (symbolToText @name)
{-# INLINE innerJoinThirdTable #-}
                       


-- | Adds an @ORDER BY .. ASC@ to your query.
--
-- Use 'orderByDesc' for descending order.
--
-- __Example:__ Fetch the 10 oldest books.
--
-- > query @Book
-- >     |> orderBy #createdAt -- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM books LIMIT 10 ORDER BY created_at ASC
orderByAsc :: forall name model table value queryBuilderProvider joinRegister. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
orderByAsc !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Asc } }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
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
orderByDesc :: forall name model table value queryBuilderProvider joinRegister. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
orderByDesc !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Desc } }
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE orderByDesc #-}

-- | Alias for 'orderByAsc'
orderBy :: (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
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
limit :: (HasQueryBuilder queryBuilderProvider joinRegister) => Int -> queryBuilderProvider model -> queryBuilderProvider model
limit !queryLimit queryBuilderProvider = injectQueryBuilder LimitQueryBuilder { queryBuilder, queryLimit }
    where
        queryBuilder = getQueryBuilder queryBuilderProvider
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
offset :: (HasQueryBuilder queryBuilderProvider joinRegister) => Int -> queryBuilderProvider model -> queryBuilderProvider model
offset !queryOffset queryBuilderProvider = injectQueryBuilder OffsetQueryBuilder { queryBuilder, queryOffset }
    where
        queryBuilder = getQueryBuilder queryBuilderProvider
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
queryUnion :: (HasQueryBuilder queryBuilderProvider joinRegister, HasQueryBuilder r joinRegister') => queryBuilderProvider model -> r model -> NoJoinQueryBuilderWrapper model
queryUnion firstQueryBuilderProvider secondQueryBuilderProvider = NoJoinQueryBuilderWrapper (UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder })
    where
        firstQueryBuilder = getQueryBuilder firstQueryBuilderProvider
        secondQueryBuilder = getQueryBuilder secondQueryBuilderProvider

    
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
queryOr :: (HasQueryBuilder queryBuilderProvider joinRegister, HasQueryBuilder queryBUilderProvider'' joinRegister'', HasQueryBuilder queryBuilderProvider''' joinRegister''') => (queryBuilderProvider model -> queryBuilderProvider''' model) -> (queryBuilderProvider model -> queryBUilderProvider'' model) -> queryBuilderProvider model -> NoJoinQueryBuilderWrapper model 
queryOr firstQuery secondQuery queryBuilder = NoJoinQueryBuilderWrapper 
    (UnionQueryBuilder { 
        firstQueryBuilder = getQueryBuilder $ firstQuery queryBuilder, 
        secondQueryBuilder = getQueryBuilder $ secondQuery queryBuilder}
    )
{-# INLINE queryOr #-}

-- | Adds a @DISTINCT@ to your query.
--
-- Use 'distinct' to remove all duplicate rows from the result
--
-- __Example:__ Fetch distinct books
--
-- > query @Book
-- >     |> distinct
-- >     |> fetch
-- > -- SELECT DISTINCT * FROM books
distinct :: (HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> queryBuilderProvider table
distinct = injectQueryBuilder . DistinctQueryBuilder . getQueryBuilder
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
distinctOn :: forall name model value table queryBuilderProvider joinRegister. (KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
distinctOn !name queryBuilderProvider = injectQueryBuilder DistinctOnQueryBuilder { distinctOnColumn = columnName, queryBuilder = getQueryBuilder queryBuilderProvider}
    where
        columnName = Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE distinctOn #-}



-- | Helper to deal with @some_field IS NULL@ and @some_field = 'some value'@
class EqOrIsOperator value where toEqOrIsOperator :: value -> FilterOperator
instance {-# OVERLAPS #-} EqOrIsOperator (Maybe something) where toEqOrIsOperator Nothing = IsOp; toEqOrIsOperator (Just _) = EqOp
instance {-# OVERLAPPABLE #-} EqOrIsOperator otherwise where toEqOrIsOperator _ = EqOp
