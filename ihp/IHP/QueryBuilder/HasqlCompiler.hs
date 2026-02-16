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
, toSQL
, compileOperator
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

-- | Compile context: parameter counter + accumulated encoder.
data CC = CC !Int !(Encoders.Params ())

-- | Initial compile context: counter starts at 1, no params.
emptyCC :: CC
emptyCC = CC 1 conquer
{-# INLINE emptyCC #-}

-- | Assign the next @$N@ placeholder and accumulate the encoder.
nextParam :: Encoders.Params () -> CC -> (Text, CC)
nextParam enc (CC n acc) = ("$" <> tshow n, CC (n + 1) (acc <> enc))
{-# INLINE nextParam #-}

-- | Build a Hasql 'Statement' from a compiled 'SQLQuery' and a result decoder.
buildStatement :: SQLQuery -> Decoders.Result a -> Hasql.Statement () a
buildStatement sqlQuery decoder =
    let (sql, CC _ encoder) = compileQuery emptyCC sqlQuery
    in Hasql.preparable sql encoder decoder
{-# INLINE buildStatement #-}

-- | Like 'buildStatement', but wraps the compiled SQL with a prefix and suffix.
-- Used for @SELECT COUNT(*) FROM (inner) AS alias@ patterns.
buildWrappedStatement :: Text -> SQLQuery -> Text -> Decoders.Result a -> Hasql.Statement () a
buildWrappedStatement prefix sqlQuery suffix decoder =
    let (innerSql, CC _ encoder) = compileQuery emptyCC sqlQuery
    in Hasql.preparable (prefix <> innerSql <> suffix) encoder decoder
{-# INLINE buildWrappedStatement #-}

-- | Compile a QueryBuilder to SQL text (for testing / error messages).
-- Discards the encoder.
toSQL :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> Text
toSQL queryBuilderProvider =
    let (sql, _) = compileQuery emptyCC (buildQuery queryBuilderProvider)
    in sql
{-# INLINE toSQL #-}

-- | Compile a full SQLQuery to SQL text + updated compile context.
compileQuery :: CC -> SQLQuery -> (Text, CC)
compileQuery cc0 sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
    let selectPart = "SELECT"
            <> compileDistinct distinctClause
            <> compileDistinctOn distinctOnClause
            <> " " <> compileSelectors queryIndex selectFrom columns
            <> " FROM " <> selectFrom
            <> compileJoins (reverse (joins sqlQuery))
        (wherePart, cc1) = compileWhere cc0 (whereCondition sqlQuery)
        orderByPart = compileOrderBy orderByClause
        (limitPart, cc2) = compileLimit cc1 limitClause
        (offsetPart, cc3) = compileOffset cc2 offsetClause
    in (selectPart <> wherePart <> orderByPart <> limitPart <> offsetPart, cc3)
{-# INLINE compileQuery #-}

compileDistinct :: Bool -> Text
compileDistinct False = ""
compileDistinct True = " DISTINCT"
{-# INLINE compileDistinct #-}

compileDistinctOn :: Maybe Text -> Text
compileDistinctOn Nothing = ""
compileDistinctOn (Just col) = " DISTINCT ON (" <> col <> ")"
{-# INLINE compileDistinctOn #-}

compileSelectors :: Maybe Text -> Text -> [Text] -> Text
compileSelectors queryIndex selectFrom columns =
    let indexParts = case queryIndex of
            Just idx -> [idx]
            Nothing -> []
        columnParts = map (\column -> selectFrom <> "." <> column) columns
    in mconcat $ List.intersperse ", " (indexParts <> columnParts)
{-# INLINE compileSelectors #-}

compileJoins :: [Join] -> Text
compileJoins [] = ""
compileJoins (j:js) = " INNER JOIN " <> table j <> " ON " <> tableJoinColumn j <> " = " <> table j <> "." <> otherJoinColumn j <> compileJoins js
{-# INLINE compileJoins #-}

compileWhere :: CC -> Maybe Condition -> (Text, CC)
compileWhere cc Nothing = ("", cc)
compileWhere cc (Just condition) =
    let (condText, cc') = compileCondition cc condition
    in (" WHERE " <> condText, cc')
{-# INLINE compileWhere #-}

compileCondition :: CC -> Condition -> (Text, CC)
compileCondition cc (ColumnCondition column operator value applyLeft applyRight) =
    let applyFn fn txt = case fn of
            Just f -> f <> "(" <> txt <> ")"
            Nothing -> txt
        colText = applyFn applyLeft column
        opText = compileOperator operator
        (valText, cc') = compileConditionValue cc value
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
{-# INLINE compileCondition #-}

compileConditionValue :: CC -> ConditionValue -> (Text, CC)
compileConditionValue cc (Param enc) = nextParam enc cc
compileConditionValue cc (Literal t) = (t, cc)
{-# INLINE compileConditionValue #-}

compileLimit :: CC -> Maybe Int -> (Text, CC)
compileLimit cc Nothing = ("", cc)
compileLimit cc (Just n) =
    let enc = contramap (const (fromIntegral n :: Int32)) (Encoders.param (Encoders.nonNullable Encoders.int4))
        (placeholder, cc') = nextParam enc cc
    in (" LIMIT " <> placeholder, cc')
{-# INLINE compileLimit #-}

compileOffset :: CC -> Maybe Int -> (Text, CC)
compileOffset cc Nothing = ("", cc)
compileOffset cc (Just n) =
    let enc = contramap (const (fromIntegral n :: Int32)) (Encoders.param (Encoders.nonNullable Encoders.int4))
        (placeholder, cc') = nextParam enc cc
    in (" OFFSET " <> placeholder, cc')
{-# INLINE compileOffset #-}

compileOrderBy :: [OrderByClause] -> Text
compileOrderBy [] = ""
compileOrderBy clauses = " ORDER BY " <> mconcat (List.intersperse "," (map compileOrderByClause clauses))
    where
        compileOrderByClause OrderByClause { orderByColumn, orderByDirection } =
            orderByColumn <> (if orderByDirection == Desc then " DESC" else "")
{-# INLINE compileOrderBy #-}

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
{-# INLINE compileOperator #-}
