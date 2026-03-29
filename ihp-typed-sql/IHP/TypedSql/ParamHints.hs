module IHP.TypedSql.ParamHints
    ( ParamHint (..)
    , extractParamHints
    , extractJoinNullableTables
    , parseSql
    , extractParamHintsFromAst
    , extractJoinNullableTablesFromAst
    , extractNonNullableComputedColumnsFromAst
    , resolveParamHintTypes
    ) where

import           Data.Foldable                (foldMap, toList)
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import qualified Data.String.Conversions     as CS
import qualified Database.PostgreSQL.LibPQ   as PQ
import qualified Language.Haskell.TH         as TH
import           IHP.Prelude

import qualified PostgresqlSyntax.Ast        as Ast
import qualified PostgresqlSyntax.Parsing    as Parsing

import           IHP.TypedSql.Metadata       (ColumnMeta (..), DescribeColumn (..),
                                              PgTypeInfo, TableMeta (..))
import           IHP.TypedSql.TypeMapping    (hsTypeForColumn)

-- | A derived hint about the expected type of a placeholder.
-- The quasiquoter uses this to coerce ${...} to a column-compatible type.
data ParamHint = ParamHint
    { phIndex  :: !Int
    , phTable  :: !Text
    , phColumn :: !Text
    , phArray  :: !Bool
    }
    deriving (Eq, Show)

-- | Parse SQL into an AST. Returns Nothing if parsing fails.
-- Note: postgresql-syntax does not tolerate leading/trailing whitespace,
-- so we strip it before parsing.
parseSql :: String -> Maybe Ast.PreparableStmt
parseSql sql =
    case Parsing.run Parsing.preparableStmt (Text.strip (Text.pack sql)) of
        Left _err -> Nothing
        Right stmt -> Just stmt

-- | Extract parameter hints by parsing SQL and walking the AST.
-- Falls back to empty map if parsing fails.
extractParamHints :: String -> Map.Map Int ParamHint
extractParamHints sql =
    case parseSql sql of
        Nothing -> Map.empty
        Just stmt -> extractParamHintsFromAst stmt

-- | Extract parameter hints from an already-parsed AST.
extractParamHintsFromAst :: Ast.PreparableStmt -> Map.Map Int ParamHint
extractParamHintsFromAst stmt =
    let aliasMap = buildAliasMapFromStmt stmt
        defTable = singleTable aliasMap
    in collectFromStmt aliasMap defTable stmt

-- | Extract table names that are on the nullable side of outer JOINs.
-- LEFT JOIN: right-side tables are nullable.
-- RIGHT JOIN: left-side tables are nullable.
-- FULL [OUTER] JOIN: both sides are nullable.
extractJoinNullableTables :: String -> Set.Set Text
extractJoinNullableTables sql =
    case parseSql sql of
        Nothing -> Set.empty
        Just stmt -> extractJoinNullableTablesFromAst stmt

-- | Extract nullable tables from an already-parsed AST.
extractJoinNullableTablesFromAst :: Ast.PreparableStmt -> Set.Set Text
extractJoinNullableTablesFromAst = nullableTablesFromStmt

nullableTablesFromStmt :: Ast.PreparableStmt -> Set.Set Text
nullableTablesFromStmt = \case
    Ast.SelectPreparableStmt selectStmt -> nullableTablesFromSelectStmt selectStmt
    _ -> Set.empty

nullableTablesFromSelectStmt :: Ast.SelectStmt -> Set.Set Text
nullableTablesFromSelectStmt (Left (Ast.SelectNoParens _with selectClause _sort _limit _lock)) =
    nullableTablesFromSelectClause selectClause
nullableTablesFromSelectStmt (Right _) = Set.empty

nullableTablesFromSelectClause :: Ast.SelectClause -> Set.Set Text
nullableTablesFromSelectClause (Left simpleSelect) = nullableTablesFromSimpleSelect simpleSelect
nullableTablesFromSelectClause (Right _) = Set.empty

nullableTablesFromSimpleSelect :: Ast.SimpleSelect -> Set.Set Text
nullableTablesFromSimpleSelect = \case
    Ast.NormalSimpleSelect _targeting _into maybeFrom _where _group _having _window ->
        case maybeFrom of
            Just fromClause -> foldMap nullableTablesFromTableRef (toList fromClause)
            Nothing -> Set.empty
    Ast.BinSimpleSelect _op left _distinct right ->
        Set.union (nullableTablesFromSelectClause left) (nullableTablesFromSelectClause right)
    _ -> Set.empty

nullableTablesFromTableRef :: Ast.TableRef -> Set.Set Text
nullableTablesFromTableRef = \case
    Ast.JoinTableRef joinedTable _alias -> nullableTablesFromJoinedTable joinedTable
    _ -> Set.empty

nullableTablesFromJoinedTable :: Ast.JoinedTable -> Set.Set Text
nullableTablesFromJoinedTable = \case
    Ast.InParensJoinedTable inner -> nullableTablesFromJoinedTable inner
    Ast.MethJoinedTable meth left right ->
        let nested = Set.union (nullableTablesFromTableRef left) (nullableTablesFromTableRef right)
        in case joinTypeFromMeth meth of
            Just (Ast.LeftJoinType _)  -> Set.union nested (tableNamesFromTableRef right)
            Just (Ast.RightJoinType _) -> Set.union nested (tableNamesFromTableRef left)
            Just (Ast.FullJoinType _)  -> Set.union nested (Set.union (tableNamesFromTableRef left) (tableNamesFromTableRef right))
            _ -> nested

joinTypeFromMeth :: Ast.JoinMeth -> Maybe Ast.JoinType
joinTypeFromMeth = \case
    Ast.QualJoinMeth maybeJoinType _ -> maybeJoinType
    Ast.NaturalJoinMeth maybeJoinType -> maybeJoinType
    Ast.CrossJoinMeth -> Nothing

-- | Collect all resolved table names from a TableRef.
tableNamesFromTableRef :: Ast.TableRef -> Set.Set Text
tableNamesFromTableRef = Set.fromList . Map.elems . buildAliasMapFromTableRef

-- | Get the text from an Ident.
-- Unquoted identifiers are folded to lowercase (PostgreSQL convention).
-- Quoted identifiers preserve case (they are case-sensitive in PostgreSQL).
identToText :: Ast.Ident -> Text
identToText (Ast.QuotedIdent t) = t
identToText (Ast.UnquotedIdent t) = Text.toLower t

-- | Extract table name from a QualifiedName, ignoring schema prefix.
qualifiedNameToText :: Ast.QualifiedName -> Text
qualifiedNameToText (Ast.SimpleQualifiedName ident) = identToText ident
qualifiedNameToText (Ast.IndirectedQualifiedName _schema indirection) =
    -- schema.table -> take the last attr name
    case toList indirection of
        [] -> identToText _schema
        els -> case List.last els of
            Ast.AttrNameIndirectionEl ident -> identToText ident
            _ -> identToText _schema

-- | If the alias map has exactly one distinct table, return it.
singleTable :: Map.Map Text Text -> Maybe Text
singleTable aliases =
    case Set.toList (Set.fromList (Map.elems aliases)) of
        [table] -> Just table
        _ -> Nothing

----------------------------------------------------------------------
-- Alias map building
----------------------------------------------------------------------

buildAliasMapFromStmt :: Ast.PreparableStmt -> Map.Map Text Text
buildAliasMapFromStmt = \case
    Ast.SelectPreparableStmt selectStmt -> buildAliasMapFromSelectStmt selectStmt
    Ast.UpdatePreparableStmt (Ast.UpdateStmt _with (Ast.RelationExprOptAlias relExpr maybeAlias) _setClauses maybeFrom _where _ret) ->
        let tableName = relationExprName relExpr
            base = Map.singleton tableName tableName
            withAlias = case maybeAlias of
                Just (_, aliasIdent) -> Map.insert (identToText aliasIdent) tableName base
                Nothing -> base
        in case maybeFrom of
            Just fromClause -> Map.union withAlias (buildAliasMapFromFrom fromClause)
            Nothing -> withAlias
    Ast.DeletePreparableStmt (Ast.DeleteStmt _with (Ast.RelationExprOptAlias relExpr maybeAlias) _using _where _ret) ->
        let tableName = relationExprName relExpr
            base = Map.singleton tableName tableName
        in case maybeAlias of
            Just (_, aliasIdent) -> Map.insert (identToText aliasIdent) tableName base
            Nothing -> base
    Ast.InsertPreparableStmt (Ast.InsertStmt _with (Ast.InsertTarget qname _alias) _rest _onConflict _ret) ->
        let tableName = qualifiedNameToText qname
        in Map.singleton tableName tableName
    Ast.CallPreparableStmt _ -> Map.empty

buildAliasMapFromSelectStmt :: Ast.SelectStmt -> Map.Map Text Text
buildAliasMapFromSelectStmt (Left (Ast.SelectNoParens maybeWith selectClause _sort _limit _lock)) =
    let withMap = case maybeWith of
            Just (Ast.WithClause _recursive ctes) -> foldMap buildAliasMapFromCte (toList ctes)
            Nothing -> Map.empty
    in Map.union withMap (buildAliasMapFromSelectClause selectClause)
buildAliasMapFromSelectStmt (Right _parens) = Map.empty

buildAliasMapFromCte :: Ast.CommonTableExpr -> Map.Map Text Text
buildAliasMapFromCte (Ast.CommonTableExpr _name _cols _mat innerStmt) =
    buildAliasMapFromStmt innerStmt

buildAliasMapFromSelectClause :: Ast.SelectClause -> Map.Map Text Text
buildAliasMapFromSelectClause (Left simpleSelect) = buildAliasMapFromSimpleSelect simpleSelect
buildAliasMapFromSelectClause (Right _parens) = Map.empty

buildAliasMapFromSimpleSelect :: Ast.SimpleSelect -> Map.Map Text Text
buildAliasMapFromSimpleSelect = \case
    Ast.NormalSimpleSelect _targeting _into maybeFrom _where _group _having _window ->
        case maybeFrom of
            Just fromClause -> buildAliasMapFromFrom fromClause
            Nothing -> Map.empty
    Ast.BinSimpleSelect _op left _distinct right ->
        Map.union (buildAliasMapFromSelectClause left) (buildAliasMapFromSelectClause right)
    Ast.TableSimpleSelect relExpr ->
        let name = relationExprName relExpr
        in Map.singleton name name
    Ast.ValuesSimpleSelect _ -> Map.empty

buildAliasMapFromFrom :: Ast.FromClause -> Map.Map Text Text
buildAliasMapFromFrom = foldMap buildAliasMapFromTableRef . toList

buildAliasMapFromTableRef :: Ast.TableRef -> Map.Map Text Text
buildAliasMapFromTableRef = \case
    Ast.RelationExprTableRef relExpr maybeAlias _sample ->
        let tableName = relationExprName relExpr
            base = Map.singleton tableName tableName
        in case maybeAlias of
            Just (Ast.AliasClause _asKw aliasIdent _cols) ->
                Map.insert (identToText aliasIdent) tableName base
            Nothing -> base
    Ast.JoinTableRef joinedTable maybeAlias ->
        let joinMap = buildAliasMapFromJoinedTable joinedTable
        in case maybeAlias of
            Just _ -> joinMap  -- parenthesized join with alias, just pass through
            Nothing -> joinMap
    Ast.SelectTableRef _lateral _subquery _alias -> Map.empty
    Ast.FuncTableRef _lateral _funcTable _alias -> Map.empty

buildAliasMapFromJoinedTable :: Ast.JoinedTable -> Map.Map Text Text
buildAliasMapFromJoinedTable = \case
    Ast.InParensJoinedTable inner -> buildAliasMapFromJoinedTable inner
    Ast.MethJoinedTable _meth left right ->
        Map.union (buildAliasMapFromTableRef left) (buildAliasMapFromTableRef right)

relationExprName :: Ast.RelationExpr -> Text
relationExprName (Ast.SimpleRelationExpr qname _) = qualifiedNameToText qname
relationExprName (Ast.OnlyRelationExpr qname _) = qualifiedNameToText qname

----------------------------------------------------------------------
-- Parameter hint collection from expressions
----------------------------------------------------------------------

collectFromStmt :: Map.Map Text Text -> Maybe Text -> Ast.PreparableStmt -> Map.Map Int ParamHint
collectFromStmt aliasMap defTable = \case
    Ast.SelectPreparableStmt selectStmt ->
        collectFromSelectStmt aliasMap defTable selectStmt
    Ast.UpdatePreparableStmt (Ast.UpdateStmt _with (Ast.RelationExprOptAlias relExpr _) setClauses _from maybeWhere _ret) ->
        let updateTable = relationExprName relExpr
            setHints = foldMap (collectFromSetClause aliasMap defTable updateTable) (toList setClauses)
            whereHints = case maybeWhere of
                Just (Ast.ExprWhereOrCurrentClause expr) -> collectFromAExpr aliasMap defTable expr
                _ -> Map.empty
        in mergeHintMaps setHints whereHints
    Ast.DeletePreparableStmt (Ast.DeleteStmt _with _rel _using maybeWhere _ret) ->
        case maybeWhere of
            Just (Ast.ExprWhereOrCurrentClause expr) -> collectFromAExpr aliasMap defTable expr
            _ -> Map.empty
    Ast.InsertPreparableStmt (Ast.InsertStmt _with _target _rest maybeOnConflict _ret) ->
        case maybeOnConflict of
            Just (Ast.OnConflict _confExpr (Ast.UpdateOnConflictDo setClauses maybeWhere)) ->
                let insertTable = case _target of
                        Ast.InsertTarget qname _ -> qualifiedNameToText qname
                    setHints = foldMap (collectFromSetClause aliasMap defTable insertTable) (toList setClauses)
                    whereHints = case maybeWhere of
                        Just whereExpr -> collectFromAExpr aliasMap defTable whereExpr
                        Nothing -> Map.empty
                in mergeHintMaps setHints whereHints
            _ -> Map.empty
    Ast.CallPreparableStmt _ -> Map.empty

collectFromSelectStmt :: Map.Map Text Text -> Maybe Text -> Ast.SelectStmt -> Map.Map Int ParamHint
collectFromSelectStmt aliasMap defTable (Left (Ast.SelectNoParens _with selectClause _sort _limit _lock)) =
    collectFromSelectClause aliasMap defTable selectClause
collectFromSelectStmt _ _ (Right _) = Map.empty

collectFromSelectClause :: Map.Map Text Text -> Maybe Text -> Ast.SelectClause -> Map.Map Int ParamHint
collectFromSelectClause aliasMap defTable (Left simpleSelect) =
    collectFromSimpleSelect aliasMap defTable simpleSelect
collectFromSelectClause _ _ (Right _) = Map.empty

collectFromSimpleSelect :: Map.Map Text Text -> Maybe Text -> Ast.SimpleSelect -> Map.Map Int ParamHint
collectFromSimpleSelect aliasMap defTable = \case
    Ast.NormalSimpleSelect _targeting _into _from maybeWhere _group _having _window ->
        case maybeWhere of
            Just whereExpr -> collectFromAExpr aliasMap defTable whereExpr
            Nothing -> Map.empty
    Ast.BinSimpleSelect _op left _distinct right ->
        mergeHintMaps
            (collectFromSelectClause aliasMap defTable left)
            (collectFromSelectClause aliasMap defTable right)
    _ -> Map.empty

collectFromSetClause :: Map.Map Text Text -> Maybe Text -> Text -> Ast.SetClause -> Map.Map Int ParamHint
collectFromSetClause aliasMap defTable updateTable = \case
    Ast.TargetSetClause (Ast.SetTarget colIdent _indirection) expr ->
        let colName = identToText colIdent
        in case extractParam expr of
            Just paramIndex ->
                Map.singleton paramIndex ParamHint
                    { phIndex = paramIndex
                    , phTable = updateTable
                    , phColumn = colName
                    , phArray = False
                    }
            Nothing -> Map.empty
    Ast.TargetListSetClause _ _ -> Map.empty

-- | Extract parameter hints from an AExpr, looking for patterns like:
-- column = $N, $N = column, column IN ($N), column = ANY($N)
collectFromAExpr :: Map.Map Text Text -> Maybe Text -> Ast.AExpr -> Map.Map Int ParamHint
collectFromAExpr aliasMap defTable = \case
    -- column = $N  or  $N = column
    Ast.SymbolicBinOpAExpr left (Ast.MathSymbolicExprBinOp Ast.EqualsMathOp) right ->
        mergeHintMaps
            (matchColEqParam aliasMap defTable left right False)
            (matchColEqParam aliasMap defTable right left False)
    -- AND / OR: recurse both sides
    Ast.AndAExpr left right ->
        mergeHintMaps
            (collectFromAExpr aliasMap defTable left)
            (collectFromAExpr aliasMap defTable right)
    Ast.OrAExpr left right ->
        mergeHintMaps
            (collectFromAExpr aliasMap defTable left)
            (collectFromAExpr aliasMap defTable right)
    -- NOT: recurse
    Ast.NotAExpr inner -> collectFromAExpr aliasMap defTable inner
    -- column IN ($N, ...) - reversable op
    Ast.ReversableOpAExpr colExpr _negated (Ast.InAExprReversableOp (Ast.ExprListInExpr exprs)) ->
        case resolveColumnRef aliasMap defTable colExpr of
            Just (tableName, colName) ->
                foldMap (\expr -> case extractParam expr of
                    Just n ->
                        Map.singleton n ParamHint
                            { phIndex = n
                            , phTable = tableName
                            , phColumn = colName
                            , phArray = True
                            }
                    Nothing -> Map.empty
                    ) (toList exprs)
            Nothing -> Map.empty
    -- column = ANY($N) pattern via SubqueryAExpr
    Ast.SubqueryAExpr colExpr (Ast.AllSubqueryOp (Ast.MathAllOp Ast.EqualsMathOp)) Ast.AnySubType (Right paramExpr) ->
        case resolveColumnRef aliasMap defTable colExpr of
            Just (tableName, colName) ->
                case extractParam paramExpr of
                    Just n -> Map.singleton n ParamHint
                        { phIndex = n
                        , phTable = tableName
                        , phColumn = colName
                        , phArray = True
                        }
                    Nothing -> Map.empty
            Nothing -> Map.empty
    -- Parenthesized expression
    Ast.CExprAExpr (Ast.InParensCExpr inner _) -> collectFromAExpr aliasMap defTable inner
    _ -> Map.empty

-- | Try to match: colExpr is a column ref and paramExpr is $N
matchColEqParam :: Map.Map Text Text -> Maybe Text -> Ast.AExpr -> Ast.AExpr -> Bool -> Map.Map Int ParamHint
matchColEqParam aliasMap defTable colExpr paramExpr isArray =
    case (resolveColumnRef aliasMap defTable colExpr, extractParam paramExpr) of
        (Just (tableName, colName), Just n) ->
            Map.singleton n ParamHint
                { phIndex = n
                , phTable = tableName
                , phColumn = colName
                , phArray = isArray
                }
        _ -> Map.empty

-- | Resolve a column reference expression to (table, column).
resolveColumnRef :: Map.Map Text Text -> Maybe Text -> Ast.AExpr -> Maybe (Text, Text)
resolveColumnRef aliasMap defTable = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref colId maybeIndirection)) ->
        case maybeIndirection of
            -- qualified: tableOrAlias.column
            Just indirection ->
                case toList indirection of
                    [Ast.AttrNameIndirectionEl colIdent] ->
                        let tableRef = identToText colId
                            colName = identToText colIdent
                        in case Map.lookup tableRef aliasMap of
                            Just tableName -> Just (tableName, colName)
                            Nothing -> Nothing
                    _ -> Nothing
            -- unqualified: just column name
            Nothing ->
                case defTable of
                    Just tableName -> Just (tableName, identToText colId)
                    Nothing -> Nothing
    _ -> Nothing

-- | Extract a $N parameter index from an expression.
extractParam :: Ast.AExpr -> Maybe Int
extractParam = \case
    Ast.CExprAExpr (Ast.ParamCExpr n _) -> Just n
    _ -> Nothing

-- | Merge two hint maps. On conflict, the left (earlier) hint wins.
mergeHintMaps :: Map.Map Int ParamHint -> Map.Map Int ParamHint -> Map.Map Int ParamHint
mergeHintMaps = Map.union

----------------------------------------------------------------------
-- Non-nullable computed column detection
----------------------------------------------------------------------

-- | A map from subquery/CTE alias to its SELECT target list entries.
-- Each entry is (column_name, expression).
type SubqueryTargetMap = Map.Map Text [(Text, Ast.AExpr)]

-- | Extract 0-based indices of SELECT columns known to be non-nullable
-- from AST analysis. Currently detects count() and traces through
-- subquery aliases and CTEs.
extractNonNullableComputedColumnsFromAst :: Ast.PreparableStmt -> Set.Set Int
extractNonNullableComputedColumnsFromAst = \case
    Ast.SelectPreparableStmt selectStmt -> nonNullFromSelectStmt selectStmt
    _ -> Set.empty

nonNullFromSelectStmt :: Ast.SelectStmt -> Set.Set Int
nonNullFromSelectStmt (Left (Ast.SelectNoParens maybeWith selectClause _sort _limit _lock)) =
    let cteMap = case maybeWith of
            Just (Ast.WithClause _recursive ctes) -> foldMap buildSubqueryTargetMapFromCte (toList ctes)
            Nothing -> Map.empty
    in nonNullFromSelectClause cteMap selectClause
nonNullFromSelectStmt (Right _) = Set.empty

nonNullFromSelectClause :: SubqueryTargetMap -> Ast.SelectClause -> Set.Set Int
nonNullFromSelectClause sqMap (Left simpleSelect) = nonNullFromSimpleSelect sqMap simpleSelect
nonNullFromSelectClause _ (Right _) = Set.empty

nonNullFromSimpleSelect :: SubqueryTargetMap -> Ast.SimpleSelect -> Set.Set Int
nonNullFromSimpleSelect sqMap = \case
    Ast.NormalSimpleSelect maybeTargeting _into maybeFrom _where _group _having _window ->
        let fromMap = case maybeFrom of
                Just fromClause -> foldMap buildSubqueryTargetMapFromTableRef (toList fromClause)
                Nothing -> Map.empty
            fullMap = Map.union fromMap sqMap
        in case maybeTargeting of
            Just (Ast.NormalTargeting targets) -> analyzeTargets fullMap (toList targets)
            Just (Ast.DistinctTargeting _ targets) -> analyzeTargets fullMap (toList targets)
            _ -> Set.empty
    _ -> Set.empty

analyzeTargets :: SubqueryTargetMap -> [Ast.TargetEl] -> Set.Set Int
analyzeTargets sqMap targets =
    Set.fromList
        [ i
        | (i, target) <- zip [0..] targets
        , isTargetNonNullable sqMap target
        ]

isTargetNonNullable :: SubqueryTargetMap -> Ast.TargetEl -> Bool
isTargetNonNullable sqMap = \case
    Ast.AliasedExprTargetEl expr _ -> isExprNonNullable sqMap expr
    Ast.ImplicitlyAliasedExprTargetEl expr _ -> isExprNonNullable sqMap expr
    Ast.ExprTargetEl expr -> isExprNonNullable sqMap expr
    Ast.AsteriskTargetEl -> False

-- | Determine if an expression is known to be non-nullable.
isExprNonNullable :: SubqueryTargetMap -> Ast.AExpr -> Bool
isExprNonNullable sqMap = \case
    -- Function calls: count(), row_number(), rank(), dense_rank()
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.ApplicationFuncExpr (Ast.FuncApplication funcName _params) _withinGroup _filter _over)) ->
        isNonNullableFunction funcName
    -- Special syntax functions: EXISTS, COALESCE, CURRENT_DATE, etc.
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.SubexprFuncExpr subexpr)) ->
        isSubexprNonNullable sqMap subexpr
    -- EXISTS(...) — always returns Bool, never NULL
    Ast.CExprAExpr (Ast.ExistsCExpr _) -> True
    -- Non-NULL literal constants
    Ast.CExprAExpr (Ast.AexprConstCExpr constant) -> not (isNullConstant constant)
    -- Column reference: alias.col where alias is a subquery/CTE
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref aliasId (Just indirection))) ->
        case toList indirection of
            [Ast.AttrNameIndirectionEl colIdent] ->
                let aliasName = identToText aliasId
                    colName = identToText colIdent
                in case Map.lookup aliasName sqMap of
                    Just targets -> case List.find (\(n, _) -> n == colName) targets of
                        Just (_, subExpr) -> isExprNonNullable sqMap subExpr
                        Nothing -> False
                    Nothing -> False
            _ -> False
    -- Unqualified column reference through a CTE/subquery (when there's only one source)
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref colId Nothing)) ->
        let colName = identToText colId
        in case Map.elems sqMap of
            [targets] -> case List.find (\(n, _) -> n == colName) targets of
                Just (_, subExpr) -> isExprNonNullable sqMap subExpr
                Nothing -> False
            _ -> False
    -- Parenthesized expression
    Ast.CExprAExpr (Ast.InParensCExpr inner Nothing) -> isExprNonNullable sqMap inner
    -- Typecast preserves nullability of inner expression (e.g. 1::bigint)
    Ast.TypecastAExpr inner _ -> isExprNonNullable sqMap inner
    _ -> False

-- | Functions that are guaranteed to return a non-null value.
-- count() always returns 0 for empty groups.
-- row_number(), rank(), dense_rank() are window functions that never return NULL.
isNonNullableFunction :: Ast.FuncName -> Bool
isNonNullableFunction funcName =
    funcNameToText funcName `elem` ["count", "row_number", "rank", "dense_rank"]

-- | Special syntax expressions that are non-nullable.
isSubexprNonNullable :: SubqueryTargetMap -> Ast.FuncExprCommonSubexpr -> Bool
isSubexprNonNullable sqMap = \case
    -- COALESCE(a, b, ...) is non-null when any argument is non-null
    Ast.CoalesceFuncExprCommonSubexpr args ->
        any (isExprNonNullable sqMap) (toList args)
    -- CAST(expr AS type) preserves nullability
    Ast.CastFuncExprCommonSubexpr inner _ -> isExprNonNullable sqMap inner
    -- CURRENT_DATE, CURRENT_TIME, etc. are always non-null
    Ast.CurrentDateFuncExprCommonSubexpr -> True
    Ast.CurrentTimeFuncExprCommonSubexpr _ -> True
    Ast.CurrentTimestampFuncExprCommonSubexpr _ -> True
    Ast.LocalTimeFuncExprCommonSubexpr _ -> True
    Ast.LocalTimestampFuncExprCommonSubexpr _ -> True
    Ast.CurrentRoleFuncExprCommonSubexpr -> True
    Ast.CurrentUserFuncExprCommonSubexpr -> True
    Ast.SessionUserFuncExprCommonSubexpr -> True
    Ast.UserFuncExprCommonSubexpr -> True
    Ast.CurrentCatalogFuncExprCommonSubexpr -> True
    Ast.CurrentSchemaFuncExprCommonSubexpr -> True
    _ -> False

-- | Check if a constant is the NULL literal.
isNullConstant :: Ast.AexprConst -> Bool
isNullConstant Ast.NullAexprConst = True
isNullConstant _ = False

-- | Extract the function name as lowercase text.
funcNameToText :: Ast.FuncName -> Text
funcNameToText = \case
    Ast.TypeFuncName ident -> identToText ident
    Ast.IndirectedFuncName _ indirection ->
        case toList indirection of
            [Ast.AttrNameIndirectionEl funcIdent] -> identToText funcIdent
            _ -> ""

-- | Build SubqueryTargetMap entries from a CTE.
buildSubqueryTargetMapFromCte :: Ast.CommonTableExpr -> SubqueryTargetMap
buildSubqueryTargetMapFromCte (Ast.CommonTableExpr cteName _cols _mat innerStmt) =
    case innerStmt of
        Ast.SelectPreparableStmt selectStmt ->
            case extractTargetsFromSelectStmt selectStmt of
                Just targets -> Map.singleton (identToText cteName) targets
                Nothing -> Map.empty
        _ -> Map.empty

-- | Build SubqueryTargetMap entries from FROM clause table refs.
buildSubqueryTargetMapFromTableRef :: Ast.TableRef -> SubqueryTargetMap
buildSubqueryTargetMapFromTableRef = \case
    Ast.SelectTableRef _lateral subquery (Just (Ast.AliasClause _asKw aliasIdent _cols)) ->
        case extractTargetsFromSelectWithParens subquery of
            Just targets -> Map.singleton (identToText aliasIdent) targets
            Nothing -> Map.empty
    Ast.JoinTableRef joinedTable _alias ->
        buildSubqueryTargetMapFromJoinedTable joinedTable
    _ -> Map.empty

buildSubqueryTargetMapFromJoinedTable :: Ast.JoinedTable -> SubqueryTargetMap
buildSubqueryTargetMapFromJoinedTable = \case
    Ast.InParensJoinedTable inner -> buildSubqueryTargetMapFromJoinedTable inner
    Ast.MethJoinedTable _meth left right ->
        Map.union (buildSubqueryTargetMapFromTableRef left) (buildSubqueryTargetMapFromTableRef right)

-- | Extract (name, expr) pairs from a SelectWithParens.
extractTargetsFromSelectWithParens :: Ast.SelectWithParens -> Maybe [(Text, Ast.AExpr)]
extractTargetsFromSelectWithParens = \case
    Ast.NoParensSelectWithParens (Ast.SelectNoParens _with selectClause _sort _limit _lock) ->
        extractTargetsFromSelectClause selectClause
    Ast.WithParensSelectWithParens inner ->
        extractTargetsFromSelectWithParens inner

extractTargetsFromSelectStmt :: Ast.SelectStmt -> Maybe [(Text, Ast.AExpr)]
extractTargetsFromSelectStmt (Left (Ast.SelectNoParens _with selectClause _sort _limit _lock)) =
    extractTargetsFromSelectClause selectClause
extractTargetsFromSelectStmt (Right _) = Nothing

extractTargetsFromSelectClause :: Ast.SelectClause -> Maybe [(Text, Ast.AExpr)]
extractTargetsFromSelectClause (Left simpleSelect) = extractTargetsFromSimpleSelect simpleSelect
extractTargetsFromSelectClause (Right _) = Nothing

extractTargetsFromSimpleSelect :: Ast.SimpleSelect -> Maybe [(Text, Ast.AExpr)]
extractTargetsFromSimpleSelect = \case
    Ast.NormalSimpleSelect maybeTargeting _into _from _where _group _having _window ->
        case maybeTargeting of
            Just (Ast.NormalTargeting targets) -> Just (map targetToNameExpr (toList targets))
            Just (Ast.DistinctTargeting _ targets) -> Just (map targetToNameExpr (toList targets))
            _ -> Nothing
    _ -> Nothing

-- | Extract (implicit_name, expression) from a TargetEl.
targetToNameExpr :: Ast.TargetEl -> (Text, Ast.AExpr)
targetToNameExpr = \case
    Ast.AliasedExprTargetEl expr alias -> (identToText alias, expr)
    Ast.ImplicitlyAliasedExprTargetEl expr alias -> (identToText alias, expr)
    Ast.ExprTargetEl expr -> (implicitName expr, expr)
    Ast.AsteriskTargetEl -> ("*", Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref (Ast.UnquotedIdent "*") Nothing)))

-- | Derive the implicit column name for an expression (used when no AS alias).
-- PostgreSQL uses the function name for function calls and the column name for column refs.
implicitName :: Ast.AExpr -> Text
implicitName = \case
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.ApplicationFuncExpr (Ast.FuncApplication funcName _) _ _ _)) ->
        case funcName of
            Ast.TypeFuncName ident -> identToText ident
            Ast.IndirectedFuncName _ indirection ->
                case toList indirection of
                    [Ast.AttrNameIndirectionEl ident] -> identToText ident
                    _ -> ""
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref colId Nothing)) -> identToText colId
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref _ (Just indirection))) ->
        case toList indirection of
            [Ast.AttrNameIndirectionEl ident] -> identToText ident
            _ -> ""
    _ -> ""

----------------------------------------------------------------------
-- Type resolution (unchanged from before)
----------------------------------------------------------------------

-- | Convert parameter hints into concrete Haskell types using table metadata.
-- The quasiquoter uses this to apply per-placeholder type annotations.
resolveParamHintTypes :: Map.Map PQ.Oid TableMeta -> Map.Map PQ.Oid PgTypeInfo -> Map.Map Int ParamHint -> TH.Q (Map.Map Int TH.Type)
resolveParamHintTypes tables typeInfo hints = do
    let tablesByName = tables
            |> Map.toList
            |> mapMaybe (\(oid, table@TableMeta { tmName }) -> Just (tmName, (oid, table)))
            |> Map.fromList
    resolved <- mapM (resolveHint tablesByName) (Map.toList hints)
    pure (Map.fromList (catMaybes resolved))
  where
    resolveHint tablesByName (index, ParamHint { phTable, phColumn, phArray }) = do
        case Map.lookup phTable tablesByName of
            Nothing -> pure Nothing
            Just (tableOid, table@TableMeta { tmColumns }) ->
                case findColumn tmColumns phColumn of
                    Nothing -> pure Nothing
                    Just (attnum, ColumnMeta { cmTypeOid }) -> do
                        baseType <- hsTypeForColumn typeInfo tables Set.empty False DescribeColumn
                            { dcName = CS.cs phColumn
                            , dcType = cmTypeOid
                            , dcTable = tableOid
                            , dcAttnum = Just attnum
                            }
                        let stripped = stripMaybeType baseType
                        let hintedType = if phArray then TH.AppT TH.ListT stripped else stripped
                        pure (Just (index, hintedType))

    findColumn columns columnName =
        columns
            |> Map.toList
            |> List.find (\(_, ColumnMeta { cmName }) -> Text.toLower cmName == Text.toLower columnName)

-- | Strip a top-level Maybe wrapper to get the base column type.
-- Used when parameter hints should be non-nullable inputs.
stripMaybeType :: TH.Type -> TH.Type
stripMaybeType (TH.AppT (TH.ConT maybeName) inner)
    | maybeName == ''Maybe = inner
stripMaybeType other = other
