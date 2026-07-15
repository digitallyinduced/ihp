{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}

{-|
Module: IHP.QueryBuilder.HasqlCompiler
Description: Compile QueryBuilder to Hasql Statement
Copyright: (c) digitally induced GmbH, 2025

This module compiles QueryBuilder queries directly to Hasql 'Statement' values
by threading a parameter counter and encoder accumulator through compilation.
-}
module IHP.QueryBuilder.HasqlCompiler
( buildStatement
, buildWrappedStatement
, buildQueryMatchesRowStatement
, buildQueryMatchesRowsStatement
, toSQL
, compileOperator
, CompilerState(..)
, emptyCompilerState
, nextParam
) where

import IHP.Prelude
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Statement as Hasql
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (buildQuery)
import qualified Data.List as List
import qualified Data.Text as Text

-- | Compile context: parameter counter + accumulated encoder.
data CompilerState = CompilerState !Int !(Encoders.Params ())

-- | Initial compile context: counter starts at 1, no params.
emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState 1 conquer
{-# INLINE emptyCompilerState #-}

-- | Assign the next @$N@ placeholder and accumulate the encoder.
nextParam :: Encoders.Params () -> CompilerState -> (Text, CompilerState)
nextParam enc (CompilerState n acc) = ("$" <> tshow n, CompilerState (n + 1) (acc <> enc))
{-# INLINE nextParam #-}

-- | Build a Hasql 'Statement' from a compiled 'SQLQuery' and a result decoder.
buildStatement :: SQLQuery -> Decoders.Result a -> Hasql.Statement () a
buildStatement sqlQuery decoder =
    let (sql, CompilerState _ encoder) = compileQuery emptyCompilerState sqlQuery
    in Hasql.preparable sql encoder decoder

-- | Like 'buildStatement', but wraps the compiled SQL with a prefix and suffix.
-- Used for @SELECT COUNT(*) FROM (inner) AS alias@ patterns.
buildWrappedStatement :: Text -> SQLQuery -> Text -> Decoders.Result a -> Hasql.Statement () a
buildWrappedStatement prefix sqlQuery suffix decoder =
    let (innerSql, CompilerState _ encoder) = compileQuery emptyCompilerState sqlQuery
    in Hasql.preparable (prefix <> innerSql <> suffix) encoder decoder

-- | Build a statement that checks whether a row ID is present in a query result.
--
-- The query builder's parameters are already captured inside its @Params ()@
-- encoder. They are lifted to the statement's 'Text' input and combined with a
-- final, runtime row-ID parameter. This preserves the exact bound parameters of
-- the original query instead of retaining SQL text alone.
buildQueryMatchesRowStatement :: SQLQuery -> Hasql.Statement Text Bool
buildQueryMatchesRowStatement sqlQuery =
    let (innerSql, CompilerState nextParameter queryEncoder) = compileQuery emptyCompilerState sqlQuery
        rowIdEncoder = Encoders.param (Encoders.nonNullable Encoders.text)
        encoder = contramap (const ()) queryEncoder <> rowIdEncoder
        tableType = quoteIdentifierPath sqlQuery.selectFrom
        sql =
            "SELECT EXISTS (SELECT 1 FROM (" <> innerSql
            <> ") AS _ihp_auto_refresh_records WHERE _ihp_auto_refresh_records.id = "
            <> "(jsonb_populate_record(NULL::" <> tableType
            <> ", jsonb_build_object('id', $" <> tshow nextParameter <> "))).id)"
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    in Hasql.preparable sql encoder decoder

-- | Batched variant of 'buildQueryMatchesRowStatement'. The changed IDs arrive
-- as text in the compact notification payload. @jsonb_populate_record@ converts
-- them back to the table's real @id@ type, preserving primary-key index usage.
buildQueryMatchesRowsStatement :: SQLQuery -> Hasql.Statement [Text] Bool
buildQueryMatchesRowsStatement sqlQuery =
    let (innerSql, CompilerState nextParameter queryEncoder) = compileQuery emptyCompilerState sqlQuery
        rowIdsEncoder = Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text)))
        encoder = contramap (const ()) queryEncoder <> rowIdsEncoder
        tableType = quoteIdentifierPath sqlQuery.selectFrom
        sql =
            "SELECT EXISTS (SELECT 1 FROM (" <> innerSql
            <> ") AS _ihp_auto_refresh_records WHERE _ihp_auto_refresh_records.id = ANY (ARRAY("
            <> "SELECT (jsonb_populate_record(NULL::" <> tableType
            <> ", jsonb_build_object('id', _ihp_changed_id))).id "
            <> "FROM unnest($" <> tshow nextParameter <> "::text[]) AS _ihp_changed_id)))"
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
    in Hasql.preparable sql encoder decoder

quoteIdentifierPath :: Text -> Text
quoteIdentifierPath = Text.intercalate "." . map quoteIdentifierText . Text.splitOn "."
  where
    quoteIdentifierText name = "\"" <> Text.replace "\"" "\"\"" name <> "\""

-- | Compile a QueryBuilder to SQL text (for testing / error messages).
-- Discards the encoder.
toSQL :: forall table. KnownSymbol table => QueryBuilder table -> Text
toSQL queryBuilder =
    let (sql, _) = compileQuery emptyCompilerState (buildQuery queryBuilder)
    in sql

-- | Compile a full SQLQuery to SQL text + updated compile context.
--
-- Structured so that the Nothing/empty branches contribute no concatenation;
-- GHC can see through the case alternatives and eliminate dead appends.
compileQuery :: CompilerState -> SQLQuery -> (Text, CompilerState)
compileQuery cc0 SQLQuery { selectFrom, distinctClause, distinctOnClause, whereCondition, orderByClause, limitClause, offsetClause, columnsSql } =
    let -- Build the fixed prefix: SELECT [DISTINCT] [DISTINCT ON (...)] cols FROM table
        selectPart = case distinctClause of
            True -> case distinctOnClause of
                Just col -> "SELECT DISTINCT DISTINCT ON (" <> col <> ") " <> columnsSql <> " FROM " <> selectFrom
                Nothing  -> "SELECT DISTINCT " <> columnsSql <> " FROM " <> selectFrom
            False -> case distinctOnClause of
                Just col -> "SELECT DISTINCT ON (" <> col <> ") " <> columnsSql <> " FROM " <> selectFrom
                Nothing  -> "SELECT " <> columnsSql <> " FROM " <> selectFrom

        -- WHERE: only append when there is a condition
        (withWhere, cc1) = case whereCondition of
            Nothing -> (selectPart, cc0)
            Just condition ->
                let (condText, cc') = compileCondition cc0 condition
                in (selectPart <> " WHERE " <> condText, cc')

        -- ORDER BY: only append when there are clauses
        withOrderBy = case orderByClause of
            [] -> withWhere
            clauses -> withWhere <> " ORDER BY " <> compileOrderByClauses clauses

        -- LIMIT: only append when set
        (withLimit, cc2) = case limitClause of
            Nothing -> (withOrderBy, cc1)
            Just n ->
                let enc = contramap (const n) (Encoders.param (Encoders.nonNullable Encoders.int8))
                    (placeholder, cc') = nextParam enc cc1
                in (withOrderBy <> " LIMIT " <> placeholder, cc')

        -- OFFSET: only append when set
        (result, cc3) = case offsetClause of
            Nothing -> (withLimit, cc2)
            Just n ->
                let enc = contramap (const n) (Encoders.param (Encoders.nonNullable Encoders.int8))
                    (placeholder, cc') = nextParam enc cc2
                in (withLimit <> " OFFSET " <> placeholder, cc')

    in (result, cc3)

compileCondition :: CompilerState -> Condition -> (Text, CompilerState)
compileCondition cc (ColumnCondition column operator value applyLeft applyRight) =
    let applyFn fn txt = case fn of
            Just f -> f <> "(" <> txt <> ")"
            Nothing -> txt
        colText = applyFn applyLeft column
        opText = compileOperator operator
        (valText, cc') = case operator of
            IsOp -> ("NULL", cc)
            IsNotOp -> ("NULL", cc)
            _ -> compileConditionValue cc value
        valWrapped = case operator of
            InOp -> "(" <> valText <> ")"
            NotInOp -> "(" <> valText <> ")"
            SqlOp -> valText
            _ -> applyFn applyRight valText
    in case operator of
        SqlOp -> (colText <> " " <> valWrapped, cc')
        _ -> (colText <> " " <> opText <> " " <> valWrapped, cc')
compileCondition cc (OrCondition a b) =
    let (aText, cc1) = compileCondition cc a
        (bText, cc2) = compileCondition cc1 b
    in ("(" <> aText <> ") OR (" <> bText <> ")", cc2)
compileCondition cc (AndCondition a b) =
    let (aText, cc1) = compileCondition cc a
        (bText, cc2) = compileCondition cc1 b
    in ("(" <> aText <> ") AND (" <> bText <> ")", cc2)

compileConditionValue :: CompilerState -> ConditionValue -> (Text, CompilerState)
compileConditionValue cc (Param enc) = nextParam enc cc
compileConditionValue cc (Literal t) = (t, cc)

compileOrderByClauses :: [OrderByClause] -> Text
compileOrderByClauses clauses = mconcat (List.intersperse "," (map compileOrderByClause clauses))
    where
        compileOrderByClause OrderByClause { orderByColumn, orderByDirection } =
            orderByColumn <> (if orderByDirection == Desc then " DESC" else "")

-- | Compiles a 'FilterOperator' to its SQL representation
compileOperator :: FilterOperator -> Text
compileOperator EqOp = "="
compileOperator NotEqOp = "!="
compileOperator InOp = "= ANY"
compileOperator NotInOp = "<> ALL"
compileOperator IsOp = "IS"
compileOperator IsNotOp = "IS NOT"
compileOperator (LikeOp CaseSensitive) = "LIKE"
compileOperator (LikeOp CaseInsensitive) = "ILIKE"
compileOperator (NotLikeOp CaseSensitive) = "NOT LIKE"
compileOperator (NotLikeOp CaseInsensitive) = "NOT ILIKE"
compileOperator (MatchesOp CaseSensitive) = "~"
compileOperator (MatchesOp CaseInsensitive) = "~*"
compileOperator GreaterThanOp = ">"
compileOperator GreaterThanOrEqualToOp = ">="
compileOperator LessThanOp = "<"
compileOperator LessThanOrEqualToOp = "<="
compileOperator SqlOp = ""
