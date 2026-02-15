module IHP.TypedSql.ParamHints
    ( ParamHint (..)
    , extractParamHints
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

data ParamHint = ParamHint
    { phIndex  :: !Int
    , phTable  :: !Text
    , phColumn :: !Text
    , phArray  :: !Bool
    }
    deriving (Eq, Show)

-- | Extract parameter hints by parsing SQL and walking the AST.
-- Falls back to empty map if parsing fails.
extractParamHints :: String -> Map.Map Int ParamHint
extractParamHints sql =
    case Parsing.run Parsing.preparableStmt (Text.pack sql) of
        Left _err -> Map.empty
        Right stmt ->
            let aliasMap = aliasesFromStmt stmt
                defTable = singleTable aliasMap
            in hintsFromStmt aliasMap defTable stmt

----------------------------------------------------------------------
-- Alias map: walk AST to map alias/table names → canonical table names
----------------------------------------------------------------------

aliasesFromStmt :: Ast.PreparableStmt -> Map.Map Text Text
aliasesFromStmt = \case
    Ast.SelectPreparableStmt sel -> aliasesFromSelect sel
    Ast.UpdatePreparableStmt (Ast.UpdateStmt _with rel _set maybeFrom _where _ret) ->
        let base = aliasesFromRel rel
        in case maybeFrom of
            Just from -> Map.union base (aliasesFromFrom from)
            Nothing -> base
    Ast.DeletePreparableStmt (Ast.DeleteStmt _with rel _using _where _ret) ->
        aliasesFromRel rel
    Ast.InsertPreparableStmt (Ast.InsertStmt _with (Ast.InsertTarget qname _) _rest _onConflict _ret) ->
        let t = qualifiedNameToText qname in Map.singleton t t
    Ast.CallPreparableStmt _ -> Map.empty

-- | Extract aliases from a RelationExprOptAlias (UPDATE/DELETE target).
aliasesFromRel :: Ast.RelationExprOptAlias -> Map.Map Text Text
aliasesFromRel (Ast.RelationExprOptAlias relExpr maybeAlias) =
    let t = relationExprName relExpr
        base = Map.singleton t t
    in case maybeAlias of
        Just (_, aliasIdent) -> Map.insert (identToText aliasIdent) t base
        Nothing -> base

aliasesFromSelect :: Ast.SelectStmt -> Map.Map Text Text
aliasesFromSelect (Left (Ast.SelectNoParens maybeWith selectClause _sort _limit _lock)) =
    let withMap = case maybeWith of
            Just (Ast.WithClause _recursive ctes) ->
                foldMap (\(Ast.CommonTableExpr _ _ _ inner) -> aliasesFromStmt inner) (toList ctes)
            Nothing -> Map.empty
    in Map.union withMap (aliasesFromSelectClause selectClause)
aliasesFromSelect (Right _) = Map.empty

aliasesFromSelectClause :: Ast.SelectClause -> Map.Map Text Text
aliasesFromSelectClause (Left simple) = aliasesFromSimple simple
aliasesFromSelectClause (Right _) = Map.empty

aliasesFromSimple :: Ast.SimpleSelect -> Map.Map Text Text
aliasesFromSimple = \case
    Ast.NormalSimpleSelect _ _ maybeFrom _ _ _ _ ->
        maybe Map.empty aliasesFromFrom maybeFrom
    Ast.BinSimpleSelect _ left _ right ->
        Map.union (aliasesFromSelectClause left) (aliasesFromSelectClause right)
    Ast.TableSimpleSelect relExpr ->
        let t = relationExprName relExpr in Map.singleton t t
    Ast.ValuesSimpleSelect _ -> Map.empty

aliasesFromFrom :: Ast.FromClause -> Map.Map Text Text
aliasesFromFrom = foldMap aliasesFromTableRef . toList

aliasesFromTableRef :: Ast.TableRef -> Map.Map Text Text
aliasesFromTableRef = \case
    Ast.RelationExprTableRef relExpr maybeAlias _ ->
        let t = relationExprName relExpr
            base = Map.singleton t t
        in case maybeAlias of
            Just (Ast.AliasClause _ aliasIdent _) -> Map.insert (identToText aliasIdent) t base
            Nothing -> base
    Ast.JoinTableRef joined _ -> aliasesFromJoined joined
    _ -> Map.empty

aliasesFromJoined :: Ast.JoinedTable -> Map.Map Text Text
aliasesFromJoined = \case
    Ast.InParensJoinedTable inner -> aliasesFromJoined inner
    Ast.MethJoinedTable _ left right ->
        Map.union (aliasesFromTableRef left) (aliasesFromTableRef right)

----------------------------------------------------------------------
-- Hint collection: walk WHERE/SET clauses for column = $N patterns
----------------------------------------------------------------------

type AliasMap = Map.Map Text Text

hintsFromStmt :: AliasMap -> Maybe Text -> Ast.PreparableStmt -> Map.Map Int ParamHint
hintsFromStmt am dt = \case
    Ast.SelectPreparableStmt sel -> hintsFromSelect am dt sel
    Ast.UpdatePreparableStmt (Ast.UpdateStmt _ (Ast.RelationExprOptAlias relExpr _) setClauses _ maybeWhere _) ->
        let table = relationExprName relExpr
        in Map.union (foldMap (hintsFromSet table) (toList setClauses)) (hintsFromWhere am dt maybeWhere)
    Ast.DeletePreparableStmt (Ast.DeleteStmt _ _ _ maybeWhere _) ->
        hintsFromWhere am dt maybeWhere
    Ast.InsertPreparableStmt (Ast.InsertStmt _ (Ast.InsertTarget qname _) _ maybeOnConflict _) ->
        case maybeOnConflict of
            Just (Ast.OnConflict _ (Ast.UpdateOnConflictDo setClauses maybeWhere)) ->
                let table = qualifiedNameToText qname
                in Map.union
                    (foldMap (hintsFromSet table) (toList setClauses))
                    (maybe Map.empty (hintsFromExpr am dt) maybeWhere)
            _ -> Map.empty
    Ast.CallPreparableStmt _ -> Map.empty

hintsFromSelect :: AliasMap -> Maybe Text -> Ast.SelectStmt -> Map.Map Int ParamHint
hintsFromSelect am dt (Left (Ast.SelectNoParens _ sc _ _ _)) = hintsFromSelectClause am dt sc
hintsFromSelect _ _ _ = Map.empty

hintsFromSelectClause :: AliasMap -> Maybe Text -> Ast.SelectClause -> Map.Map Int ParamHint
hintsFromSelectClause am dt (Left simple) = hintsFromSimple am dt simple
hintsFromSelectClause _ _ _ = Map.empty

hintsFromSimple :: AliasMap -> Maybe Text -> Ast.SimpleSelect -> Map.Map Int ParamHint
hintsFromSimple am dt = \case
    Ast.NormalSimpleSelect _ _ _ maybeWhere _ _ _ ->
        maybe Map.empty (hintsFromExpr am dt) maybeWhere
    Ast.BinSimpleSelect _ left _ right ->
        Map.union (hintsFromSelectClause am dt left) (hintsFromSelectClause am dt right)
    _ -> Map.empty

hintsFromWhere :: AliasMap -> Maybe Text -> Maybe Ast.WhereOrCurrentClause -> Map.Map Int ParamHint
hintsFromWhere am dt = \case
    Just (Ast.ExprWhereOrCurrentClause expr) -> hintsFromExpr am dt expr
    _ -> Map.empty

hintsFromSet :: Text -> Ast.SetClause -> Map.Map Int ParamHint
hintsFromSet table = \case
    Ast.TargetSetClause (Ast.SetTarget colIdent _) expr ->
        case extractParam expr of
            Just n -> Map.singleton n (mkHint n table (identToText colIdent) False)
            Nothing -> Map.empty
    _ -> Map.empty

hintsFromExpr :: AliasMap -> Maybe Text -> Ast.AExpr -> Map.Map Int ParamHint
hintsFromExpr am dt = \case
    -- column = $N  or  $N = column
    Ast.SymbolicBinOpAExpr left (Ast.MathSymbolicExprBinOp Ast.EqualsMathOp) right ->
        Map.union (matchColParam am dt left right False) (matchColParam am dt right left False)
    -- AND / OR: recurse
    Ast.AndAExpr left right -> Map.union (hintsFromExpr am dt left) (hintsFromExpr am dt right)
    Ast.OrAExpr left right -> Map.union (hintsFromExpr am dt left) (hintsFromExpr am dt right)
    Ast.NotAExpr inner -> hintsFromExpr am dt inner
    -- column IN ($N, ...)
    Ast.ReversableOpAExpr colExpr _ (Ast.InAExprReversableOp (Ast.ExprListInExpr exprs)) ->
        case resolveColumnRef am dt colExpr of
            Just (table, col) ->
                foldMap (\e -> maybe Map.empty (\n -> Map.singleton n (mkHint n table col True)) (extractParam e)) (toList exprs)
            Nothing -> Map.empty
    -- column = ANY($N)
    Ast.SubqueryAExpr colExpr (Ast.AllSubqueryOp (Ast.MathAllOp Ast.EqualsMathOp)) Ast.AnySubType (Right paramExpr) ->
        case (resolveColumnRef am dt colExpr, extractParam paramExpr) of
            (Just (table, col), Just n) -> Map.singleton n (mkHint n table col True)
            _ -> Map.empty
    -- Parenthesized expression
    Ast.CExprAExpr (Ast.InParensCExpr inner _) -> hintsFromExpr am dt inner
    _ -> Map.empty

-- | Try to match: colExpr is a column ref and paramExpr is $N
matchColParam :: AliasMap -> Maybe Text -> Ast.AExpr -> Ast.AExpr -> Bool -> Map.Map Int ParamHint
matchColParam am dt colExpr paramExpr isArray =
    case (resolveColumnRef am dt colExpr, extractParam paramExpr) of
        (Just (table, col), Just n) -> Map.singleton n (mkHint n table col isArray)
        _ -> Map.empty

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

mkHint :: Int -> Text -> Text -> Bool -> ParamHint
mkHint n table col isArray = ParamHint { phIndex = n, phTable = table, phColumn = col, phArray = isArray }

identToText :: Ast.Ident -> Text
identToText (Ast.QuotedIdent t) = Text.toLower t
identToText (Ast.UnquotedIdent t) = Text.toLower t

qualifiedNameToText :: Ast.QualifiedName -> Text
qualifiedNameToText (Ast.SimpleQualifiedName ident) = identToText ident
qualifiedNameToText (Ast.IndirectedQualifiedName schema indirection) =
    case toList indirection of
        [] -> identToText schema
        els -> case List.last els of
            Ast.AttrNameIndirectionEl ident -> identToText ident
            _ -> identToText schema

relationExprName :: Ast.RelationExpr -> Text
relationExprName (Ast.SimpleRelationExpr qname _) = qualifiedNameToText qname
relationExprName (Ast.OnlyRelationExpr qname _) = qualifiedNameToText qname

singleTable :: Map.Map Text Text -> Maybe Text
singleTable aliases =
    case Set.toList (Set.fromList (Map.elems aliases)) of
        [table] -> Just table
        _ -> Nothing

resolveColumnRef :: AliasMap -> Maybe Text -> Ast.AExpr -> Maybe (Text, Text)
resolveColumnRef aliasMap defTable = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref colId maybeIndirection)) ->
        case maybeIndirection of
            Just indirection -> case toList indirection of
                [Ast.AttrNameIndirectionEl colIdent] ->
                    fmap (\t -> (t, identToText colIdent)) (Map.lookup (identToText colId) aliasMap)
                _ -> Nothing
            Nothing -> fmap (\t -> (t, identToText colId)) defTable
    _ -> Nothing

extractParam :: Ast.AExpr -> Maybe Int
extractParam = \case
    Ast.CExprAExpr (Ast.ParamCExpr n _) -> Just n
    _ -> Nothing

----------------------------------------------------------------------
-- Type resolution (unchanged)
----------------------------------------------------------------------

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
            Just (tableOid, TableMeta { tmColumns }) ->
                case findColumn tmColumns phColumn of
                    Nothing -> pure Nothing
                    Just (attnum, ColumnMeta { cmTypeOid }) -> do
                        baseType <- hsTypeForColumn typeInfo tables DescribeColumn
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
            |> fmap (\(attnum, column) -> (attnum, column))

stripMaybeType :: TH.Type -> TH.Type
stripMaybeType (TH.AppT (TH.ConT maybeName) inner)
    | maybeName == ''Maybe = inner
stripMaybeType other = other
