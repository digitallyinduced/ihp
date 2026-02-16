{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Filter
Description: WHERE clause filtering functions for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides all the filterWhere* functions for building WHERE clauses.
-}
module IHP.QueryBuilder.Filter
( filterWhere
, filterWhereCaseInsensitive
, filterWhereNot
, filterWhereIn
, filterWhereInCaseInsensitive
, filterWhereIdIn
, filterWhereNotIn
, filterWhereLike
, filterWhereILike
, filterWhereMatches
, filterWhereIMatches
, filterWherePast
, filterWhereFuture
, filterWhereGreaterThan
, filterWhereLarger
, filterWhereGreaterThanOrEqualTo
, filterWhereAtLeast
, filterWhereLessThan
, filterWhereSmaller
, filterWhereLessThanOrEqualTo
, filterWhereAtMost
, filterWhereSql
  -- * Joined table variants
, filterWhereJoinedTable
, filterWhereCaseInsensitiveJoinedTable
, filterWhereNotJoinedTable
, filterWhereInJoinedTable
, filterWhereNotInJoinedTable
, filterWhereLikeJoinedTable
, filterWhereILikeJoinedTable
, filterWhereMatchesJoinedTable
, filterWhereIMatchesJoinedTable
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (negateFilterOperator)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder)
import IHP.Hasql.Encoders () -- Import for DefaultParamEncoder instances
import IHP.QueryBuilder.Compiler (qualifiedColumnName)

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
filterWhere :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhere (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (toEqOrIsOperator value) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (toEqOrIsOperator value) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereJoinedTable #-}

-- | Like 'filterWhereJoinedTable', but adds a @WHERE LOWER(x) = LOWER(y)@ condition to the query. Example:
--
-- __Example:__ get posts by user Tom, ignoring case.
--
-- > tomPosts <- query @Post
-- >                    |> innerJoin @User (#createdBy, #id)
-- >                    |> filterWhereCaseInsensitiveJoinedTable @User (#name, "Tom" :: Text)
-- >                    |> fetch
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE LOWER(users.name) = LOWER('Tom')
--
filterWhereCaseInsensitiveJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereCaseInsensitiveJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (toEqOrIsOperator value) (Snippet.param value) (Just "LOWER") (Just "LOWER")
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereCaseInsensitiveJoinedTable #-}

-- | Like 'filterWhere' but negates the condition.
--
-- __Example:__ Only show projects created by other users.
--
-- > activeProjects <- query @Project
-- >     |> filterWhereNot (#userId, currentUserId)
-- >     |> fetch
-- > -- SELECT * FROM projects WHERE user_id != '23d5ea33-b28e-4f0a-99b3-77a3564a2546'
--
filterWhereNot :: forall name table model value. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, Table model) => (Proxy name, value) -> QueryBuilder table -> QueryBuilder table
filterWhereNot (name, value) queryBuilder = addCondition condition queryBuilder
    where
        condition = ColumnCondition columnName (negateFilterOperator (toEqOrIsOperator value)) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereNotJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (negateFilterOperator (toEqOrIsOperator value)) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereIn :: forall name table model value queryBuilderProvider (joinRegister :: Type). (KnownSymbol table, KnownSymbol name, DefaultParamEncoder [value], DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value, Table model) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIn (name, value) queryBuilderProvider =
        case head nullValues of
            Nothing -> injectQueryBuilder $ addCondition inCondition qb -- All values non null
            Just nullValue ->
                let
                    isNullCondition = ColumnCondition columnName IsOp (Snippet.param nullValue) Nothing Nothing
                in
                    case head nonNullValues of
                        Just nonNullValue -> -- Some non null values, some null values
                            injectQueryBuilder $ addCondition (OrCondition inCondition isNullCondition) qb
                        Nothing -> injectQueryBuilder $ addCondition isNullCondition qb -- All values null
    where
        -- Only NOT NULL values can be compares inside the IN expression, NULL values have to be compares using a manual appended IS expression
        -- https://github.com/digitallyinduced/ihp/issues/906
        --
        (nonNullValues, nullValues) = value |> partition (\v -> toEqOrIsOperator v == EqOp)

        inCondition = ColumnCondition columnName InOp (Snippet.param nonNullValues) Nothing Nothing

        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        qb = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIn #-}

-- Like 'filterWhereIn', but case insensitive.
--
-- __Example:__ Only show users where @email@ is @User1@example.com@ or @User2@example.com@.
--
-- > users <- query @User
-- >     |> filterWhereInCaseInsensitive (#email, ['User1@example.com', 'User2@example.com'])
-- >     |> fetch
-- > -- SELECT * FROM users WHERE LOWER(email) IN ('user1@example.com', 'user2@example.com')
--
filterWhereInCaseInsensitive :: forall name table model value queryBuilderProvider (joinRegister :: Type). ( KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value, Table model) => (Proxy name, [Text]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereInCaseInsensitive (name, values) queryBuilderProvider =
        case head nullValues of
            Nothing -> injectQueryBuilder $ addCondition inCondition qb
            Just nullValue ->
                let
                    isNullCondition = ColumnCondition columnName IsOp (Snippet.param nullValue) Nothing Nothing
                in
                    case head nonNullValues of
                        Just _ ->
                            injectQueryBuilder $ addCondition (OrCondition inCondition isNullCondition) qb
                        Nothing -> injectQueryBuilder $ addCondition isNullCondition qb
    where
        (nonNullValues, nullValues) = values |> partition (\v -> toEqOrIsOperator v == EqOp)

        lowerValues = map toLower nonNullValues

        inCondition = ColumnCondition lowerColumnName InOp (Snippet.param lowerValues) Nothing Nothing

        lowerColumnName = "LOWER(" <> columnName <> ")"
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        qb = getQueryBuilder queryBuilderProvider

{-# INLINE filterWhereInCaseInsensitive #-}

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
filterWhereInJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder [value], HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereInJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName InOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereNotIn :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder [value], DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereNotIn (_, []) queryBuilder = queryBuilder -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotIn (name, value) queryBuilderProvider =
        case head nullValues of
            Nothing -> injectQueryBuilder $ addCondition notInCondition qb -- All values non null
            Just nullValue ->
                case head nonNullValues of
                    Just nonNullValue -> injectQueryBuilder $ addCondition (ColumnCondition columnName IsNotOp (Snippet.param nullValue) Nothing Nothing) (addCondition notInCondition qb) -- Some non null values, some null values
                    Nothing -> injectQueryBuilder $ addCondition (ColumnCondition columnName IsNotOp (Snippet.param nullValue) Nothing Nothing) qb -- All values null
    where
        -- Only NOT NULL values can be compares inside the IN expression, NULL values have to be compares using a manual appended IS expression
        -- https://github.com/digitallyinduced/ihp/issues/906
        --
        (nonNullValues, nullValues) = value |> partition (\v -> toEqOrIsOperator v == EqOp)

        notInCondition = ColumnCondition columnName NotInOp (Snippet.param nonNullValues) Nothing Nothing

        columnName = qualifiedColumnName (symbolToText @table) (symbolToText @name)
        qb = getQueryBuilder queryBuilderProvider
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
filterWhereNotInJoinedTable :: forall model name table  value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder [value], HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotInJoinedTable (_, []) queryBuilderProvider = queryBuilderProvider -- Handle empty case by ignoring query part: `WHERE x NOT IN ()`
filterWhereNotInJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName NotInOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereNotInJoinedTable #-}


-- | Adds a @WHERE x LIKE y@ condition to the query.
--
-- __Example:__ Find titles matching search term.
--
-- > articles <- query @Article
-- >     |> filterWhereLike (#title, "%" <> searchTerm <> "%")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title LIKE '%..%'
filterWhereLike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLike (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (LikeOp CaseSensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereLikeJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol name, KnownSymbol table, table ~ GetTableName model, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereLikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (LikeOp CaseSensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereLikeJoinedTable #-}


-- | Adds a @WHERE x ILIKE y@ condition to the query. Case-insensitive version of 'filterWhereLike'.
--
-- __Example:__ Find titles matching search term.
--
-- > articles <- query @Article
-- >     |> filterWhereILike (#title, "%" <> searchTerm <> "%")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title ILIKE '%..%'
filterWhereILike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereILike (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (LikeOp CaseInsensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereILikeJoinedTable :: forall model table name table' model' value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, model' ~ GetModelByTableName table', HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereILikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (LikeOp CaseInsensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereILikeJoinedTable #-}


-- | Adds a @WHERE x ~ y@ condition to the query.
--
-- __Example:__ Find names with titles in front.
--
-- > articles <- query @User
-- >     |> filterWhereMatches (#name, "^(M(rs|r|iss)|Dr|Sir). ")
-- >     |> fetch
-- > -- SELECT * FROM articles WHERE title ~ '^(M(rs|r|iss)|Dr|Sir). '
filterWhereMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereMatches (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (MatchesOp CaseSensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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

filterWhereMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (MatchesOp CaseSensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereMatchesJoinedTable #-}


-- | Adds a @WHERE x ~* y@ condition to the query. Case-insensitive version of 'filterWhereMatches'.
filterWhereIMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIMatches (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (MatchesOp CaseInsensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereIMatches #-}

-- | Case-insensitive version of 'filterWhereMatchesJoinedTable'
filterWhereIMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereIMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (MatchesOp CaseInsensitive) (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereIMatchesJoinedTable #-}


-- | Filter all rows by whether a field is in the past, determined by comparing 'NOW()' to the field's value.
--
-- Opposite of 'filterWhereFuture'
--
-- __Example:__ Fetch all posts scheduled for the past.
--
-- > publicPosts <- query @Post
-- >     |> filterWherePast #scheduledAt
-- >     |> fetch
-- > -- SELECT * FROM posts WHERE scheduled_at <= NOW()
filterWherePast
    :: ( KnownSymbol table
       , KnownSymbol name
       , HasField name (GetModelByTableName table) value
       , HasQueryBuilder queryBuilderProvider joinRegister
       , Table (GetModelByTableName table)
       )
    => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
filterWherePast name = filterWhereSql (name, "<= NOW()")
{-# INLINE filterWherePast #-}

-- | Filter all rows by whether a field is in the future, determined by comparing 'NOW()' to the field's value.
--
-- Opposite of 'filterWherePast'
--
-- __Example:__ Fetch all posts scheduled for the future.
--
-- > hiddenPosts <- query @Post
-- >     |> filterWhereFuture #scheduledAt
-- >     |> fetch
-- > -- SELECT * FROM posts WHERE scheduled_at > NOW()
filterWhereFuture
    :: ( KnownSymbol table
       , KnownSymbol name
       , HasField name (GetModelByTableName table) value
       , HasQueryBuilder queryBuilderProvider joinRegister
       , Table (GetModelByTableName table)
       )
    => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereFuture name = filterWhereSql (name, "> NOW()")
{-# INLINE filterWhereFuture #-}

-- | Adds a @WHERE x > y@ condition to the query.
--
-- __Example:__ Find assignments with grade greater than 80.
--
-- > greatAssignments <- query @Assignment
-- >     |> filterWhereGreaterThan (#grade, 80)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade > 80
--
-- See also: 'filterWhereLarger', 'filterWhereGreaterThanOrEqualTo', 'filterWhereAtLeast'
filterWhereGreaterThan :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereGreaterThan (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName GreaterThanOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereGreaterThan #-}

-- | Alias for 'filterWhereGreaterThan'. Adds a @WHERE x > y@ condition to the query.
--
-- __Example:__ Find assignments with grade larger than 80.
--
-- > greatAssignments <- query @Assignment
-- >     |> filterWhereLarger (#grade, 80)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade > 80
filterWhereLarger :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLarger = filterWhereGreaterThan
{-# INLINE filterWhereLarger #-}

-- | Adds a @WHERE x >= y@ condition to the query.
--
-- __Example:__ Find assignments with grade at least 80.
--
-- > greatAssignments <- query @Assignment
-- >     |> filterWhereGreaterThanOrEqualTo (#grade, 80)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade >= 80
--
-- See also: 'filterWhereAtLeast', 'filterWhereGreaterThan', 'filterWhereLarger'
filterWhereGreaterThanOrEqualTo :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereGreaterThanOrEqualTo (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName GreaterThanOrEqualToOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereGreaterThanOrEqualTo #-}

-- | Alias for 'filterWhereGreaterThanOrEqualTo'. Adds a @WHERE x >= y@ condition to the query.
--
-- __Example:__ Find assignments with grade at least 80.
--
-- > greatAssignments <- query @Assignment
-- >     |> filterWhereAtLeast (#grade, 80)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade >= 80
filterWhereAtLeast :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereAtLeast = filterWhereGreaterThanOrEqualTo
{-# INLINE filterWhereAtLeast #-}

-- | Adds a @WHERE x < y@ condition to the query.
--
-- __Example:__ Find assignments with grade less than 60.
--
-- > poorAssignments <- query @Assignment
-- >     |> filterWhereLessThan (#grade, 60)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade < 60
--
-- See also: 'filterWhereSmaller', 'filterWhereLessThanOrEqualTo', 'filterWhereAtMost'
filterWhereLessThan :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLessThan (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName LessThanOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereLessThan #-}

-- | Alias for 'filterWhereLessThan'. Adds a @WHERE x < y@ condition to the query.
--
-- __Example:__ Find assignments with grade smaller than 60.
--
-- > poorAssignments <- query @Assignment
-- >     |> filterWhereSmaller (#grade, 60)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade < 60
filterWhereSmaller :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereSmaller = filterWhereLessThan
{-# INLINE filterWhereSmaller #-}

-- | Adds a @WHERE x <= y@ condition to the query.
--
-- __Example:__ Find assignments with grade at most 60.
--
-- > poorAssignments <- query @Assignment
-- >     |> filterWhereLessThanOrEqualTo (#grade, 60)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade <= 60
--
-- See also: 'filterWhereAtMost', 'filterWhereLessThan', 'filterWhereSmaller'
filterWhereLessThanOrEqualTo :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLessThanOrEqualTo (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName LessThanOrEqualToOp (Snippet.param value) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereLessThanOrEqualTo #-}

-- | Alias for 'filterWhereLessThanOrEqualTo'. Adds a @WHERE x <= y@ condition to the query.
--
-- __Example:__ Find assignments with grade at most 60.
--
-- > poorAssignments <- query @Assignment
-- >     |> filterWhereAtMost (#grade, 60)
-- >     |> fetch
-- > -- SELECT * FROM assignments WHERE grade <= 60
filterWhereAtMost :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereAtMost = filterWhereLessThanOrEqualTo
{-# INLINE filterWhereAtMost #-}


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
filterWhereSql :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, Text) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereSql (name, sqlCondition) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName SqlOp (Snippet.sql sqlCondition) Nothing Nothing
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
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
filterWhereCaseInsensitive :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereCaseInsensitive (name, value) queryBuilderProvider = injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
    where
        condition = ColumnCondition columnName (toEqOrIsOperator value) (Snippet.param value) (Just "LOWER") (Just "LOWER")
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE filterWhereCaseInsensitive #-}


filterWhereIdIn :: forall table model queryBuilderProvider (joinRegister :: Type). (KnownSymbol table, Table model, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIdIn values queryBuilderProvider =
    -- We don't need to treat null values differently here, because primary keys imply not-null
    -- Extract the raw primary key values from the Id wrappers
    let
        rawPrimaryKeys = map (\(Id pk) -> pk) values
        condition = ColumnCondition (primaryKeyConditionColumnSelector @model) InOp (Snippet.param rawPrimaryKeys) Nothing Nothing
     in
        injectQueryBuilder $ addCondition condition (getQueryBuilder queryBuilderProvider)
{-# INLINE filterWhereIdIn #-}
