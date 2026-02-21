module IHP.TypedSql.ParamHints
    ( ParamHint (..)
    , extractParamHints
    , extractJoinNullableTables
    , parseSql
    , extractParamHintsFromAst
    , extractJoinNullableTablesFromAst
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
                        baseType <- hsTypeForColumn typeInfo tables Set.empty DescribeColumn
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
