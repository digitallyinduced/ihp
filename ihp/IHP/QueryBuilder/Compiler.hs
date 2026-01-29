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
, toSnippet
, toSnippet'
, compileConditionQuery
, compileConditionArgs
, compileConditionSnippet
, compileOperator
, negateFilterOperator
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import qualified Data.List as List
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Control.DeepSeq as DeepSeq
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet

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
negateFilterOperator :: FilterOperator -> FilterOperator
negateFilterOperator EqOp = NotEqOp
negateFilterOperator NotEqOp = EqOp
negateFilterOperator InOp = NotInOp
negateFilterOperator NotInOp = InOp
negateFilterOperator IsOp = IsNotOp
negateFilterOperator IsNotOp = IsOp
negateFilterOperator (LikeOp matchSensitivity) = (NotLikeOp matchSensitivity)
negateFilterOperator (NotLikeOp matchSensitivity) = (LikeOp matchSensitivity)
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
            |> modify #orderByClause (\value -> value <> [queryOrderByClause] )
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

-- | Transforms a @query @@User |> ..@ expression into a SQL Snippet.
-- Returns a tuple with the sql query template ByteString and a list of Snippet params.
--
-- __Example:__ Get the sql query that is represented by a QueryBuilder
--
-- >>> let postsQuery = query @Post |> filterWhere (#public, True)
-- >>> toSQL postsQuery
-- Returns (ByteString, [Snippet]) pair representing the query
toSQL :: (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> (ByteString, [Snippet])
toSQL queryBuilderProvider = toSQL' (buildQuery queryBuilderProvider)
{-# INLINE toSQL #-}

toSQL' :: SQLQuery -> (ByteString, [Snippet])
toSQL' sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
        (theQuery, theParams)
    where
        !theQuery = LByteString.toStrict (Builder.toLazyByteString queryBuilder)

        queryBuilder =
            Builder.byteString "SELECT"
            <> optionalB distinctClause
            <> optionalB distinctOnClause
            <> Builder.char8 ' ' <> selectorsBuilder
            <> Builder.byteString " FROM"
            <> Builder.char8 ' ' <> Builder.byteString selectFrom
            <> optionalB joinClause
            <> optionalB whereConditions'
            <> optionalB orderByClause'
            <> optionalB limitClause
            <> optionalB offsetClause

        optionalB :: Maybe ByteString -> Builder.Builder
        optionalB Nothing = mempty
        optionalB (Just bs) = Builder.char8 ' ' <> Builder.byteString bs
        {-# INLINE optionalB #-}

        selectorsBuilder :: Builder.Builder
        selectorsBuilder =
            let indexParts = case queryIndex of
                    Just idx -> [Builder.byteString idx]
                    Nothing -> []
                -- Generates a string like: `posts.id, posts.title, posts.body`
                selectFromB = Builder.byteString selectFrom
                columnParts = map (\column -> selectFromB <> Builder.char8 '.' <> Builder.byteString column) columns
            in mconcat $ List.intersperse (Builder.byteString ", ") (indexParts <> columnParts)

        !theParams =
            case whereCondition sqlQuery of
                Just condition -> compileConditionArgs condition
                Nothing -> mempty

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

-- | Compile a SQL query and its parameters into a single Snippet for execution with hasql
toSnippet :: (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> Snippet
toSnippet queryBuilderProvider = toSnippet' (buildQuery queryBuilderProvider)
{-# INLINE toSnippet #-}

-- | Compile a SQLQuery into a Snippet that can be executed directly
toSnippet' :: SQLQuery -> Snippet
toSnippet' SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, whereCondition, joins, orderByClause, limitClause, offsetClause, columns } =
    mconcat $ catMaybes
        [ Just $ Snippet.sql "SELECT"
        , Snippet.sql . (" " <>) . cs <$> distinctClause
        , Snippet.sql . (" " <>) . cs <$> distinctOnClause
        , Just $ Snippet.sql (" " <> cs selectors)
        , Just $ Snippet.sql (" FROM " <> cs selectFrom)
        , Snippet.sql . (" " <>) . cs <$> joinClause
        , whereSnippet
        , Snippet.sql . (" " <>) . cs <$> orderByClause'
        , Snippet.sql . (" " <>) . cs <$> limitClause
        , Snippet.sql . (" " <>) . cs <$> offsetClause
        ]
    where
        selectors :: ByteString
        selectors = ByteString.intercalate ", " $ (catMaybes [queryIndex]) <> selectFromWithColumns
            where
                selectFromWithColumns :: [ByteString]
                selectFromWithColumns =
                    columns
                    |> map (\column -> selectFrom <> "." <> column)

        whereSnippet = case whereCondition of
            Just condition -> Just $ Snippet.sql " WHERE " <> compileConditionSnippet condition
            Nothing -> Nothing

        orderByClause' :: Maybe ByteString
        orderByClause' = case orderByClause of
                [] -> Nothing
                xs -> Just ("ORDER BY " <> ByteString.intercalate "," ((map (\OrderByClause { orderByColumn, orderByDirection } -> orderByColumn <> (if orderByDirection == Desc then " DESC" else mempty)) xs)))

        joinClause :: Maybe ByteString
        joinClause = buildJoinClause $ reverse joins
        buildJoinClause :: [Join] -> Maybe ByteString
        buildJoinClause [] = Nothing
        buildJoinClause (joinClause:joinClauses) = Just $ "INNER JOIN " <> table joinClause <> " ON " <> tableJoinColumn joinClause <> " = " <>table joinClause <> "." <> otherJoinColumn joinClause <> maybe "" (" " <>) (buildJoinClause joinClauses)

{-# INLINE compileConditionQuery #-}
compileConditionQuery :: Condition -> ByteString
compileConditionQuery (VarCondition var _) =  var
compileConditionQuery (OrCondition a b) =  "(" <> compileConditionQuery a <> ") OR (" <> compileConditionQuery b <> ")"
compileConditionQuery (AndCondition a b) =  "(" <> compileConditionQuery a <> ") AND (" <> compileConditionQuery b <> ")"

{-# INLINE compileConditionArgs #-}
compileConditionArgs :: Condition -> [Snippet]
compileConditionArgs (VarCondition _ arg) = [arg]
compileConditionArgs (OrCondition a b) = compileConditionArgs a <> compileConditionArgs b
compileConditionArgs (AndCondition a b) = compileConditionArgs a <> compileConditionArgs b

-- | Compile a Condition directly to a Snippet without going through a ByteString template
{-# INLINE compileConditionSnippet #-}
compileConditionSnippet :: Condition -> Snippet
compileConditionSnippet (VarCondition template param) =
    let parts = ByteString.split '?' template
    in case parts of
        [before, after] -> Snippet.sql (cs before) <> param <> Snippet.sql (cs after)
        [single] -> Snippet.sql (cs single)
        _ -> error "compileConditionSnippet: multiple ? in single VarCondition"
compileConditionSnippet (OrCondition a b) =
    Snippet.sql "(" <> compileConditionSnippet a <> Snippet.sql ") OR ("
    <> compileConditionSnippet b <> Snippet.sql ")"
compileConditionSnippet (AndCondition a b) =
    Snippet.sql "(" <> compileConditionSnippet a <> Snippet.sql ") AND ("
    <> compileConditionSnippet b <> Snippet.sql ")"
