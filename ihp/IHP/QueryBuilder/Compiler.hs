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
, compileConditionSnippets
, compileOperator
, compileOperatorHasql
, negateFilterOperator
, compileJoinClause
, compileSQLQuery
, qualifiedColumnName
) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField (Action(..))
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.NameSupport (fieldNameToColumnName)
import qualified Data.List as List
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Encoding as Text
import Hasql.DynamicStatements.Snippet (Snippet)


-- | Compiles a 'FilterOperator' to its SQL representation for postgresql-simple
--
-- Uses traditional IN/NOT IN syntax with expanded parameters for postgresql-simple.
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

-- | Compiles a 'FilterOperator' to its SQL representation for hasql
--
-- For InOp and NotInOp, uses = ANY(?) and <> ALL(?) with array parameters
-- instead of the traditional IN (?, ?, ?) syntax.
compileOperatorHasql :: FilterOperator -> ByteString
compileOperatorHasql InOp = "= ANY"  -- Uses array parameter: column = ANY(?)
compileOperatorHasql NotInOp = "<> ALL"  -- Uses array parameter: column <> ALL(?)
compileOperatorHasql op = compileOperator op  -- All other operators are the same
{-# INLINE compileOperatorHasql #-}

-- | Returns the "NOT" version of an operator
--
-- >>> negateFilterOperator EqOp
-- NotEqOp
--
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
buildQuery queryBuilderProvider = compileSQLQuery (getQueryIndex queryBuilderProvider) (getQueryBuilder queryBuilderProvider)

-- | Traverse a 'QueryBuilder' tree and produce an 'SQLQuery'.
--
-- Defined at the top level so the recursive case analysis is shared across
-- all inlined call sites of 'buildQuery'. The tiny 'buildQuery' wrapper
-- stays INLINE to resolve the 'HasQueryBuilder' dictionary, then hands off
-- to this shared worker.
compileSQLQuery :: Maybe ByteString -> QueryBuilder table -> SQLQuery
compileSQLQuery qIndex NewQueryBuilder { selectFrom, columns } =
    SQLQuery
        {     queryIndex = qIndex
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
compileSQLQuery qIndex (DistinctQueryBuilder { queryBuilder }) = queryBuilder
        |> compileSQLQuery qIndex
        |> setJust #distinctClause "DISTINCT"
compileSQLQuery qIndex (DistinctOnQueryBuilder { queryBuilder, distinctOnColumn }) = queryBuilder
        |> compileSQLQuery qIndex
        |> setJust #distinctOnClause ("DISTINCT ON (" <> distinctOnColumn <> ")")
compileSQLQuery qIndex (FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, operator, action, snippet), applyLeft, applyRight }) =
            let
                applyFn fn val = case fn of
                        Just fn' -> fn' <> "(" <> val <> ")"
                        Nothing -> val

                -- pg-simple: use standard placeholder, In type expands arrays
                pgSimpleParamPlaceholder = applyFn applyRight "?"
                pgSimpleTemplate = applyFn applyLeft columnName <> " " <> compileOperator operator <> " " <> pgSimpleParamPlaceholder

                -- hasql: use = ANY(?)/(<> ALL(?) for IN/NOT IN with array parameters
                hasqlParamPlaceholder = case operator of
                    InOp -> "(?)"     -- column = ANY(?)
                    NotInOp -> "(?)"  -- column <> ALL(?)
                    _ -> applyFn applyRight "?"
                hasqlTemplate = applyFn applyLeft columnName <> " " <> compileOperatorHasql operator <> " " <> hasqlParamPlaceholder

                condition = VarCondition pgSimpleTemplate hasqlTemplate action snippet
            in
                queryBuilder
                    |> compileSQLQuery qIndex
                    |> modify #whereCondition \case
                            Just c -> Just (AndCondition c condition)
                            Nothing -> Just condition
compileSQLQuery qIndex (OrderByQueryBuilder { queryBuilder, queryOrderByClause }) = queryBuilder
        |> compileSQLQuery qIndex
        |> modify #orderByClause (\value -> value <> [queryOrderByClause] ) -- although adding to the end of a list is bad form, these lists are very short
compileSQLQuery qIndex (LimitQueryBuilder { queryBuilder, queryLimit }) =
                queryBuilder
                |> compileSQLQuery qIndex
                |> setJust #limitClause (
                        (Builder.byteString "LIMIT " <> Builder.intDec queryLimit)
                        |> Builder.toLazyByteString
                        |> LByteString.toStrict
                    )
compileSQLQuery qIndex (OffsetQueryBuilder { queryBuilder, queryOffset }) = queryBuilder
        |> compileSQLQuery qIndex
        |> setJust #offsetClause (
                (Builder.byteString "OFFSET " <> Builder.intDec queryOffset)
                |> Builder.toLazyByteString
                |> LByteString.toStrict
            )
compileSQLQuery qIndex (UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder }) =
            let
                firstQuery = compileSQLQuery qIndex firstQueryBuilder
                secondQuery = compileSQLQuery qIndex secondQueryBuilder
                isSimpleQuery q = null (orderByClause q) && isNothing (limitClause q) && isNothing (offsetClause q) && null (joins q)
                isSimpleUnion = isSimpleQuery firstQuery && isSimpleQuery secondQuery
                unionWhere =
                    case (whereCondition firstQuery, whereCondition secondQuery) of
                        (Nothing, wc) -> wc
                        (wc, Nothing) -> wc
                        (Just firstWhere, Just secondWhere) -> Just $ OrCondition firstWhere secondWhere
            in
                if isSimpleUnion then
                    firstQuery { whereCondition = unionWhere }
                else
                    error "buildQuery: Union of complex queries not supported yet"
compileSQLQuery qIndex (JoinQueryBuilder { queryBuilder, joinData }) =
    let
        firstQuery = compileSQLQuery qIndex queryBuilder
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

        -- Extract Action parameters for postgresql-simple execution
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
        joinClause = compileJoinClause $ reverse $ joins sqlQuery

-- toSQL' takes monomorphic SQLQuery — no specialization benefit from INLINE.
-- Removing INLINE prevents duplicating the SQL compilation logic at every call site.

-- | Compile a list of joins into a SQL JOIN clause.
--
-- Defined at the top level so that it is shared across all inlined call sites
-- of 'toSQL'' instead of being duplicated at each one.
compileJoinClause :: [Join] -> Maybe ByteString
compileJoinClause [] = Nothing
compileJoinClause (j:js) = Just $ "INNER JOIN " <> table j <> " ON " <> tableJoinColumn j <> " = " <> table j <> "." <> otherJoinColumn j <> maybe "" (" " <>) (compileJoinClause js)

{-# INLINE compileConditionQuery #-}
-- | Compile condition to SQL template for postgresql-simple
compileConditionQuery :: Condition -> ByteString
compileConditionQuery (VarCondition pgTemplate _ _ _) =  pgTemplate
compileConditionQuery (OrCondition a b) =  "(" <> compileConditionQuery a <> ") OR (" <> compileConditionQuery b <> ")"
compileConditionQuery (AndCondition a b) =  "(" <> compileConditionQuery a <> ") AND (" <> compileConditionQuery b <> ")"

-- | Extract Action parameters for postgresql-simple
{-# INLINE compileConditionArgs #-}
compileConditionArgs :: Condition -> [Action]
compileConditionArgs (VarCondition _ _ action _) = [action]
compileConditionArgs (OrCondition a b) = compileConditionArgs a <> compileConditionArgs b
compileConditionArgs (AndCondition a b) = compileConditionArgs a <> compileConditionArgs b

-- | Extract Snippet parameters for hasql
{-# INLINE compileConditionSnippets #-}
compileConditionSnippets :: Condition -> [Snippet]
compileConditionSnippets (VarCondition _ _ _ snippet) = [snippet]
compileConditionSnippets (OrCondition a b) = compileConditionSnippets a <> compileConditionSnippets b
compileConditionSnippets (AndCondition a b) = compileConditionSnippets a <> compileConditionSnippets b

-- | Build a qualified column name like @tablename.column_name@ from a table name
-- ByteString and a camelCase field name Text. The field name is converted to
-- snake_case via 'fieldNameToColumnName'.
--
-- This is intentionally NOINLINE: the call sites in filterWhere, orderBy, etc.
-- are always lifted to CAFs (evaluated once at program start), so inlining
-- only duplicates the Text.Inflections parsing logic (~69 Core terms per call
-- site) and ByteString concatenation (~224 Core terms) without any runtime benefit.
qualifiedColumnName :: ByteString -> Text -> ByteString
qualifiedColumnName tableName fieldName = tableName <> "." <> Text.encodeUtf8 (fieldNameToColumnName fieldName)
{-# NOINLINE qualifiedColumnName #-}
