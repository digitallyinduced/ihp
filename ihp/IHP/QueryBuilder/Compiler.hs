{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Compiler
Description: SQL compilation for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides functions to compile a QueryBuilder into SQL.
-}
module IHP.QueryBuilder.Compiler
( query
, buildQuery
, toSQL
, toSQL'
, compileConditionQuery
, compileConditionArgs
, compileOperator
, negateFilterOperator
) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Control.DeepSeq as DeepSeq

-- | Compiles a 'FilterOperator' to its SQL representation
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
compileOperator GreaterThanOp = ">"
compileOperator GreaterThanOrEqualToOp = ">="
compileOperator LessThanOp = "<"
compileOperator LessThanOrEqualToOp = "<="
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
negateFilterOperator GreaterThanOp = LessThanOrEqualToOp
negateFilterOperator GreaterThanOrEqualToOp = LessThanOp
negateFilterOperator LessThanOp = GreaterThanOrEqualToOp
negateFilterOperator LessThanOrEqualToOp = GreaterThanOp
negateFilterOperator SqlOp = SqlOp

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
query :: forall model table. (table ~ GetTableName model, Table model) => DefaultScope table => QueryBuilder table
query = (defaultScope @table) NewQueryBuilder { selectFrom = tableNameByteString @model, columns = columnNames @model }
{-# INLINE query #-}

{-# INLINE buildQuery #-}
buildQuery :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> SQLQuery
buildQuery queryBuilderProvider = buildQueryHelper $ getQueryBuilder queryBuilderProvider
    where
    buildQueryHelper NewQueryBuilder { selectFrom, columns } =
        SQLQuery
            {     queryIndex = getQueryIndex queryBuilderProvider
                , selectFrom = selectFrom
                , distinctClause = Nothing
                , distinctOnClause = Nothing
                , whereCondition = Nothing
                , joins = []
                , orderByClause = []
                , limitClause = Nothing
                , offsetClause = Nothing
                , columns
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
toSQL' sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
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
        selectors = ByteString.intercalate ", " $ (catMaybes [queryIndex]) <> selectFromWithColumns
            where
                -- Generates a string like: `posts.id, posts.title, posts.body`
                selectFromWithColumns :: [ByteString]
                selectFromWithColumns =
                    columns
                    |> map (\column -> selectFrom <> "." <> column)
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
