{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.Cardinality
    ( inferCardinality
    ) where

import           Data.Foldable                    (toList)
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import qualified Database.PostgreSQL.LibPQ        as PQ
import           IHP.Prelude
import qualified PostgresqlSyntax.Ast             as Ast

import           IHP.TypedSql.Metadata            (ColumnMeta (..), TableMeta (..))
import           IHP.TypedSql.Types               (QueryCardinality (..))

type SourceCardinalityMap = Map.Map Text QueryCardinality

-- | Conservatively infer the maximum number of rows a parsed statement can
-- return. Unknown cases intentionally stay at 'ManyRows'.
inferCardinality :: Map.Map PQ.Oid TableMeta -> Ast.PreparableStmt -> QueryCardinality
inferCardinality tables = \case
    Ast.SelectPreparableStmt selectStmt -> inferSelectStmt tables selectStmt
    Ast.InsertPreparableStmt insertStmt -> inferInsertStmt tables insertStmt
    Ast.UpdatePreparableStmt _ -> ManyRows
    Ast.DeletePreparableStmt _ -> ManyRows
    Ast.CallPreparableStmt _ -> ManyRows

inferInsertStmt :: Map.Map PQ.Oid TableMeta -> Ast.InsertStmt -> QueryCardinality
inferInsertStmt tables (Ast.InsertStmt _with _target insertRest _onConflict maybeReturning) =
    case maybeReturning of
        Nothing -> ManyRows
        Just _ ->
            case insertRest of
                Ast.DefaultValuesInsertRest -> ExactlyOneRow
                Ast.SelectInsertRest _columns _override selectStmt -> inferSelectStmt tables selectStmt

inferSelectStmt :: Map.Map PQ.Oid TableMeta -> Ast.SelectStmt -> QueryCardinality
inferSelectStmt tables = \case
    Left selectNoParens -> inferSelectNoParens tables selectNoParens
    Right selectWithParens -> inferSelectWithParens tables selectWithParens

inferSelectWithParens :: Map.Map PQ.Oid TableMeta -> Ast.SelectWithParens -> QueryCardinality
inferSelectWithParens tables = \case
    Ast.NoParensSelectWithParens selectNoParens -> inferSelectNoParens tables selectNoParens
    Ast.WithParensSelectWithParens inner -> inferSelectWithParens tables inner

inferSelectNoParens :: Map.Map PQ.Oid TableMeta -> Ast.SelectNoParens -> QueryCardinality
inferSelectNoParens tables (Ast.SelectNoParens maybeWith selectClause _sort maybeLimit _lock) =
    let cteCardinality = maybe Map.empty (cteCardinalities tables) maybeWith
    in applyLimit maybeLimit (inferSelectClause tables cteCardinality selectClause)

inferSelectClause :: Map.Map PQ.Oid TableMeta -> SourceCardinalityMap -> Ast.SelectClause -> QueryCardinality
inferSelectClause tables sourceCardinalities = \case
    Left simpleSelect -> inferSimpleSelect tables sourceCardinalities simpleSelect
    Right selectWithParens -> inferSelectWithParens tables selectWithParens

inferSimpleSelect :: Map.Map PQ.Oid TableMeta -> SourceCardinalityMap -> Ast.SimpleSelect -> QueryCardinality
inferSimpleSelect tables sourceCardinalities = \case
    Ast.NormalSimpleSelect maybeTargeting _into maybeFrom maybeWhere maybeGroup maybeHaving _window
        | isNothing maybeGroup && isAggregateTargeting maybeTargeting ->
            if isNothing maybeHaving then ExactlyOneRow else AtMostOneRow
        | isNothing maybeFrom && isNothing maybeWhere && isNothing maybeGroup && isNothing maybeHaving ->
            ExactlyOneRow
        | isNothing maybeGroup && isNothing maybeHaving
        , Just sourceCardinality <- singleSourceCardinality tables sourceCardinalities maybeFrom ->
            applyWhere maybeWhere sourceCardinality
        | provesPrimaryKeyLookup tables maybeFrom maybeWhere ->
            AtMostOneRow
        | otherwise ->
            ManyRows
    Ast.ValuesSimpleSelect values
        | length (toList values) == 1 -> ExactlyOneRow
        | otherwise -> ManyRows
    Ast.TableSimpleSelect _ -> ManyRows
    Ast.BinSimpleSelect _op _left _distinct _right -> ManyRows

cteCardinalities :: Map.Map PQ.Oid TableMeta -> Ast.WithClause -> SourceCardinalityMap
cteCardinalities tables (Ast.WithClause _recursive ctes) =
    ctes
        |> toList
        |> map (\(Ast.CommonTableExpr cteName _cols _materialized stmt) -> (identToText cteName, inferCardinality tables stmt))
        |> Map.fromList

singleSourceCardinality :: Map.Map PQ.Oid TableMeta -> SourceCardinalityMap -> Maybe Ast.FromClause -> Maybe QueryCardinality
singleSourceCardinality tables sourceCardinalities maybeFrom = do
    fromClause <- maybeFrom
    case toList fromClause of
        [tableRef] -> tableRefCardinality tables sourceCardinalities tableRef
        _ -> Nothing

tableRefCardinality :: Map.Map PQ.Oid TableMeta -> SourceCardinalityMap -> Ast.TableRef -> Maybe QueryCardinality
tableRefCardinality tables sourceCardinalities = \case
    Ast.RelationExprTableRef relExpr _alias _sample ->
        Map.lookup (relationExprName relExpr) sourceCardinalities
    Ast.SelectTableRef _lateral subquery _alias ->
        Just (inferSelectWithParens tables subquery)
    _ ->
        Nothing

applyWhere :: Maybe Ast.WhereClause -> QueryCardinality -> QueryCardinality
applyWhere Nothing cardinality = cardinality
applyWhere (Just _) cardinality =
    case cardinality of
        ExactlyOneRow -> AtMostOneRow
        AtMostOneRow -> AtMostOneRow
        ManyRows -> ManyRows

applyLimit :: Maybe Ast.SelectLimit -> QueryCardinality -> QueryCardinality
applyLimit Nothing cardinality = cardinality
applyLimit (Just selectLimit) cardinality
    | limitSelectAtMostOne selectLimit =
        if cardinality == ExactlyOneRow && not (selectLimitHasOffset selectLimit)
            then ExactlyOneRow
            else AtMostOneRow
    | otherwise = cardinality

limitSelectAtMostOne :: Ast.SelectLimit -> Bool
limitSelectAtMostOne = \case
    Ast.LimitOffsetSelectLimit limitClause _offset -> limitClauseAtMostOne limitClause
    Ast.OffsetLimitSelectLimit _offset limitClause -> limitClauseAtMostOne limitClause
    Ast.LimitSelectLimit limitClause -> limitClauseAtMostOne limitClause
    Ast.OffsetSelectLimit _offset -> False

selectLimitHasOffset :: Ast.SelectLimit -> Bool
selectLimitHasOffset = \case
    Ast.LimitOffsetSelectLimit _ _ -> True
    Ast.OffsetLimitSelectLimit _ _ -> True
    Ast.OffsetSelectLimit _ -> True
    Ast.LimitSelectLimit _ -> False

limitClauseAtMostOne :: Ast.LimitClause -> Bool
limitClauseAtMostOne = \case
    Ast.LimitLimitClause (Ast.ExprSelectLimitValue expr) _offset -> exprAtMostOne expr
    Ast.LimitLimitClause Ast.AllSelectLimitValue _offset -> False
    Ast.FetchOnlyLimitClause _withTies maybeValue _rowsOnly ->
        case maybeValue of
            Nothing -> True
            Just (Ast.ExprSelectFetchFirstValue cExpr) -> cExprAtMostOne cExpr
            Just (Ast.NumSelectFetchFirstValue _signed number) -> numericLimitAtMostOne number

exprAtMostOne :: Ast.AExpr -> Bool
exprAtMostOne = \case
    Ast.CExprAExpr cExpr -> cExprAtMostOne cExpr
    Ast.TypecastAExpr expr _ -> exprAtMostOne expr
    Ast.PlusAExpr expr -> exprAtMostOne expr
    _ -> False

cExprAtMostOne :: Ast.CExpr -> Bool
cExprAtMostOne = \case
    Ast.AexprConstCExpr (Ast.IAexprConst value) -> value <= 1
    _ -> False

numericLimitAtMostOne :: Either Int64 Double -> Bool
numericLimitAtMostOne = \case
    Left value -> value <= 1
    Right value -> value <= 1

isAggregateTargeting :: Maybe Ast.Targeting -> Bool
isAggregateTargeting = \case
    Just (Ast.NormalTargeting targets) -> any targetContainsAggregate (toList targets)
    Just (Ast.DistinctTargeting _ targets) -> any targetContainsAggregate (toList targets)
    _ -> False

targetContainsAggregate :: Ast.TargetEl -> Bool
targetContainsAggregate = \case
    Ast.AliasedExprTargetEl expr _ -> exprContainsAggregate expr
    Ast.ImplicitlyAliasedExprTargetEl expr _ -> exprContainsAggregate expr
    Ast.ExprTargetEl expr -> exprContainsAggregate expr
    Ast.AsteriskTargetEl -> False

exprContainsAggregate :: Ast.AExpr -> Bool
exprContainsAggregate = \case
    Ast.CExprAExpr cExpr -> cExprContainsAggregate cExpr
    Ast.TypecastAExpr expr _ -> exprContainsAggregate expr
    Ast.CollateAExpr expr _ -> exprContainsAggregate expr
    Ast.AtTimeZoneAExpr left right -> exprContainsAggregate left || exprContainsAggregate right
    Ast.PlusAExpr expr -> exprContainsAggregate expr
    Ast.MinusAExpr expr -> exprContainsAggregate expr
    Ast.SymbolicBinOpAExpr left _ right -> exprContainsAggregate left || exprContainsAggregate right
    Ast.PrefixQualOpAExpr _ expr -> exprContainsAggregate expr
    Ast.SuffixQualOpAExpr expr _ -> exprContainsAggregate expr
    Ast.AndAExpr left right -> exprContainsAggregate left || exprContainsAggregate right
    Ast.OrAExpr left right -> exprContainsAggregate left || exprContainsAggregate right
    Ast.NotAExpr expr -> exprContainsAggregate expr
    Ast.VerbalExprBinOpAExpr left _ _ right maybeExtra ->
        exprContainsAggregate left || exprContainsAggregate right || maybe False exprContainsAggregate maybeExtra
    Ast.ReversableOpAExpr expr _ op -> exprContainsAggregate expr || reversibleOpContainsAggregate op
    Ast.IsnullAExpr expr -> exprContainsAggregate expr
    Ast.NotnullAExpr expr -> exprContainsAggregate expr
    Ast.OverlapsAExpr _ _ -> False
    Ast.SubqueryAExpr expr _ _ subqueryOrExpr ->
        exprContainsAggregate expr || either (const False) exprContainsAggregate subqueryOrExpr
    Ast.UniqueAExpr _ -> False
    Ast.DefaultAExpr -> False

cExprContainsAggregate :: Ast.CExpr -> Bool
cExprContainsAggregate = \case
    Ast.FuncCExpr (Ast.ApplicationFuncExpr (Ast.FuncApplication funcName _params) _withinGroup _filter overClause) ->
        isNothing overClause && funcNameToText funcName `Set.member` aggregateFunctions
    Ast.FuncCExpr (Ast.SubexprFuncExpr subexpr) -> subexprContainsAggregate subexpr
    Ast.InParensCExpr expr _ -> exprContainsAggregate expr
    Ast.CaseCExpr _ -> False
    Ast.ArrayCExpr (Right _) -> False
    Ast.ExplicitRowCExpr _ -> False
    Ast.ImplicitRowCExpr _ -> False
    _ -> False

subexprContainsAggregate :: Ast.FuncExprCommonSubexpr -> Bool
subexprContainsAggregate = \case
    Ast.CoalesceFuncExprCommonSubexpr args -> any exprContainsAggregate (toList args)
    Ast.GreatestFuncExprCommonSubexpr args -> any exprContainsAggregate (toList args)
    Ast.LeastFuncExprCommonSubexpr args -> any exprContainsAggregate (toList args)
    Ast.NullIfFuncExprCommonSubexpr left right -> exprContainsAggregate left || exprContainsAggregate right
    Ast.CastFuncExprCommonSubexpr expr _ -> exprContainsAggregate expr
    Ast.CollationForFuncExprCommonSubexpr expr -> exprContainsAggregate expr
    Ast.TreatFuncExprCommonSubexpr expr _ -> exprContainsAggregate expr
    _ -> False

reversibleOpContainsAggregate :: Ast.AExprReversableOp -> Bool
reversibleOpContainsAggregate = \case
    Ast.DistinctFromAExprReversableOp expr -> exprContainsAggregate expr
    Ast.BetweenAExprReversableOp _ _ right -> exprContainsAggregate right
    Ast.BetweenSymmetricAExprReversableOp _ right -> exprContainsAggregate right
    Ast.InAExprReversableOp (Ast.ExprListInExpr exprs) -> any exprContainsAggregate (toList exprs)
    Ast.InAExprReversableOp (Ast.SelectInExpr _) -> False
    _ -> False

aggregateFunctions :: Set.Set Text
aggregateFunctions =
    Set.fromList
        [ "array_agg"
        , "avg"
        , "bit_and"
        , "bit_or"
        , "bool_and"
        , "bool_or"
        , "count"
        , "every"
        , "json_agg"
        , "json_object_agg"
        , "jsonb_agg"
        , "jsonb_object_agg"
        , "max"
        , "min"
        , "string_agg"
        , "sum"
        , "xmlagg"
        ]

provesPrimaryKeyLookup :: Map.Map PQ.Oid TableMeta -> Maybe Ast.FromClause -> Maybe Ast.WhereClause -> Bool
provesPrimaryKeyLookup tables maybeFrom maybeWhere =
    let tableByName =
            tables
                |> Map.elems
                |> map (\table@TableMeta { tmName } -> (tmName, table))
                |> Map.fromList
    in case (singleSimpleTableRef maybeFrom, maybeWhere) of
        (Just (tableName, tableQualifier), Just whereExpr) ->
            case Map.lookup tableName tableByName of
                Just tableMeta ->
                    let primaryKeyColumns = primaryKeyColumnNames tableMeta
                        constrainedColumns = equalityConstrainedColumns tableQualifier whereExpr
                    in not (Set.null primaryKeyColumns) && primaryKeyColumns `Set.isSubsetOf` constrainedColumns
                Nothing -> False
        _ -> False

primaryKeyColumnNames :: TableMeta -> Set.Set Text
primaryKeyColumnNames TableMeta { tmColumns, tmPrimaryKeys } =
    tmPrimaryKeys
        |> Set.toList
        |> mapMaybe (\attnum -> cmName <$> Map.lookup attnum tmColumns)
        |> Set.fromList

singleSimpleTableRef :: Maybe Ast.FromClause -> Maybe (Text, Text)
singleSimpleTableRef maybeFrom = do
    fromClause <- maybeFrom
    case toList fromClause of
        [Ast.RelationExprTableRef relExpr maybeAlias _sample] ->
            let tableName = relationExprName relExpr
                qualifier = case maybeAlias of
                    Just (Ast.AliasClause _asKw aliasIdent _cols) -> identToText aliasIdent
                    Nothing -> tableName
            in Just (tableName, qualifier)
        _ -> Nothing

equalityConstrainedColumns :: Text -> Ast.AExpr -> Set.Set Text
equalityConstrainedColumns qualifier = \case
    Ast.AndAExpr left right ->
        Set.union (equalityConstrainedColumns qualifier left) (equalityConstrainedColumns qualifier right)
    Ast.CExprAExpr (Ast.InParensCExpr expr Nothing) ->
        equalityConstrainedColumns qualifier expr
    Ast.SymbolicBinOpAExpr left (Ast.MathSymbolicExprBinOp Ast.EqualsMathOp) right ->
        Set.fromList (mapMaybe (columnRefForQualifier qualifier) [left, right])
    _ -> Set.empty

columnRefForQualifier :: Text -> Ast.AExpr -> Maybe Text
columnRefForQualifier qualifier = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref ident maybeIndirection)) ->
        case maybeIndirection of
            Nothing
                | qualifier == "" -> Just (identToText ident)
                | otherwise -> Just (identToText ident)
            Just indirection ->
                case toList indirection of
                    [Ast.AttrNameIndirectionEl columnIdent]
                        | identToText ident == qualifier -> Just (identToText columnIdent)
                    _ -> Nothing
    _ -> Nothing

relationExprName :: Ast.RelationExpr -> Text
relationExprName = \case
    Ast.SimpleRelationExpr qname _ -> qualifiedNameToText qname
    Ast.OnlyRelationExpr qname _ -> qualifiedNameToText qname

qualifiedNameToText :: Ast.QualifiedName -> Text
qualifiedNameToText = \case
    Ast.SimpleQualifiedName ident -> identToText ident
    Ast.IndirectedQualifiedName schema indirection ->
        case toList indirection of
            [] -> identToText schema
            els -> case List.last els of
                Ast.AttrNameIndirectionEl ident -> identToText ident
                _ -> identToText schema

funcNameToText :: Ast.FuncName -> Text
funcNameToText = \case
    Ast.TypeFuncName ident -> identToText ident
    Ast.IndirectedFuncName _ indirection ->
        case toList indirection of
            [Ast.AttrNameIndirectionEl ident] -> identToText ident
            _ -> ""

identToText :: Ast.Ident -> Text
identToText = \case
    Ast.QuotedIdent text -> text
    Ast.UnquotedIdent text -> Text.toLower text
