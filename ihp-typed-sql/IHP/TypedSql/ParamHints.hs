module IHP.TypedSql.ParamHints
    ( ParamHint (..)
    , extractParamHints
    , extractJoinNullableTables
    , parseSql
    , sqlCodeOnly
    , extractParamHintsFromAst
    , extractJoinNullableTablesFromAst
    , extractReadTableNamesFromAst
    , extractNonNullableComputedColumnsFromAst
    , resolveParamHintTypes
    , detectStarSelects
    , detectInsertWithoutColumns
    ) where

import           Data.Foldable                (foldMap, toList)
import qualified Data.Char                    as Char
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
--
-- @postgresql-syntax@ expects comments to have already been consumed by its
-- lexer. Typed SQL accepts ordinary PostgreSQL comments, so remove them while
-- preserving quoted strings, quoted identifiers and dollar-quoted bodies.
parseSql :: String -> Maybe Ast.PreparableStmt
parseSql sql =
    case Parsing.run Parsing.preparableStmt (Text.strip (stripSqlComments (Text.pack sql))) of
        Left _err -> Nothing
        Right stmt -> Just stmt

-- | Keep only executable SQL text, replacing comments and quoted values with
-- whitespace. This is used by conservative keyword checks: a word such as
-- @select@ inside a string or comment must not change query classification.
sqlCodeOnly :: String -> String
sqlCodeOnly = scanSql False

stripSqlComments :: Text -> Text
stripSqlComments = Text.pack . scanSql True . Text.unpack

-- | A small PostgreSQL-aware scanner shared by parsing and keyword checks.
-- Block comments can nest in PostgreSQL, and dollar quotes can use arbitrary
-- tags, so a simple regular expression is not sufficient here.
scanSql :: Bool -> String -> String
scanSql keepQuoted = normal
  where
    quoted character = if keepQuoted then character else ' '
    quotedText value = if keepQuoted then value else replicate (length value) ' '

    normal [] = []
    normal ('-':'-':rest) = ' ' : ' ' : lineComment rest
    normal ('/':'*':rest) = ' ' : ' ' : blockComment 1 rest
    normal ('E':'\'':rest) = 'E' : quoted '\'' : singleQuoted True rest
    normal ('e':'\'':rest) = 'e' : quoted '\'' : singleQuoted True rest
    normal ('\'':rest) = quoted '\'' : singleQuoted False rest
    normal ('"':rest) = quoted '"' : doubleQuoted rest
    normal input@('$':_) = case dollarDelimiter input of
        Just (delimiter, rest) -> quotedText delimiter <> dollarQuoted delimiter rest
        Nothing -> '$' : normal (drop 1 input)
    normal (character:rest) = character : normal rest

    lineComment [] = []
    lineComment ('\n':rest) = '\n' : normal rest
    lineComment (_:rest) = ' ' : lineComment rest

    blockComment _ [] = []
    blockComment depth ('/':'*':rest) = ' ' : ' ' : blockComment (depth + 1) rest
    blockComment 1 ('*':'/':rest) = ' ' : ' ' : normal rest
    blockComment depth ('*':'/':rest) = ' ' : ' ' : blockComment (depth - 1) rest
    blockComment depth ('\n':rest) = '\n' : blockComment depth rest
    blockComment depth (_:rest) = ' ' : blockComment depth rest

    singleQuoted _ [] = []
    singleQuoted escape ('\'':'\'':rest) = quoted '\'' : quoted '\'' : singleQuoted escape rest
    singleQuoted True ('\\':character:rest) = quoted '\\' : quoted character : singleQuoted True rest
    singleQuoted _ ('\'':rest) = quoted '\'' : normal rest
    singleQuoted escape (character:rest) = quoted character : singleQuoted escape rest

    doubleQuoted [] = []
    doubleQuoted ('"':'"':rest) = quoted '"' : quoted '"' : doubleQuoted rest
    doubleQuoted ('"':rest) = quoted '"' : normal rest
    doubleQuoted (character:rest) = quoted character : doubleQuoted rest

    dollarQuoted _ [] = []
    dollarQuoted delimiter input
        | delimiter `List.isPrefixOf` input =
            quotedText delimiter <> normal (drop (length delimiter) input)
    dollarQuoted delimiter ('\n':rest) = '\n' : dollarQuoted delimiter rest
    dollarQuoted delimiter (character:rest) = quoted character : dollarQuoted delimiter rest

    dollarDelimiter ('$':rest) =
        let (tag, suffix) = span (\character -> Char.isAlphaNum character || character == '_') rest
        in case suffix of
            '$':after -> Just ('$' : tag <> "$", after)
            _ -> Nothing
    dollarDelimiter _ = Nothing

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

-- | Extract the physical tables read by a SELECT statement.
--
-- DML statements deliberately return an empty set: a statement with RETURNING
-- rows is not a table read for AutoRefresh purposes.
extractReadTableNamesFromAst :: Ast.PreparableStmt -> Set.Set Text
extractReadTableNamesFromAst statement = case statement of
    Ast.SelectPreparableStmt selectStmt -> readTableNamesFromSelectStmt Set.empty selectStmt
    _ -> Set.empty

readTableNamesFromSelectStmt :: Set.Set Text -> Ast.SelectStmt -> Set.Set Text
readTableNamesFromSelectStmt inheritedCtes = \case
    Left (Ast.SelectNoParens maybeWith selectClause maybeSort maybeLimit _lock) ->
        let ctes = case maybeWith of
                Just (Ast.WithClause _recursive values) -> toList values
                Nothing -> []
            cteNames = Set.fromList (map cteName ctes)
            visibleCtes = inheritedCtes <> cteNames
            cteTables = foldMap (readTableNamesFromCte visibleCtes) ctes
        in cteTables
            <> readTableNamesFromSelectClause visibleCtes selectClause
            <> maybe Set.empty (readTableNamesFromSortClause visibleCtes) maybeSort
            <> maybe Set.empty (readTableNamesFromSelectLimit visibleCtes) maybeLimit
    Right selectWithParens -> readTableNamesFromSelectWithParens inheritedCtes selectWithParens

readTableNamesFromCte :: Set.Set Text -> Ast.CommonTableExpr -> Set.Set Text
readTableNamesFromCte visibleCtes (Ast.CommonTableExpr _name _cols _mat statement) =
    case statement of
        Ast.SelectPreparableStmt selectStmt -> readTableNamesFromSelectStmt visibleCtes selectStmt
        _ -> Set.empty

cteName :: Ast.CommonTableExpr -> Text
cteName (Ast.CommonTableExpr name _cols _mat _statement) = identToText name

readTableNamesFromSelectWithParens :: Set.Set Text -> Ast.SelectWithParens -> Set.Set Text
readTableNamesFromSelectWithParens visibleCtes = \case
    Ast.NoParensSelectWithParens selectNoParens ->
        readTableNamesFromSelectStmt visibleCtes (Left selectNoParens)
    Ast.WithParensSelectWithParens inner -> readTableNamesFromSelectWithParens visibleCtes inner

readTableNamesFromSelectClause :: Set.Set Text -> Ast.SelectClause -> Set.Set Text
readTableNamesFromSelectClause visibleCtes = \case
    Left simpleSelect -> readTableNamesFromSimpleSelect visibleCtes simpleSelect
    Right selectWithParens -> readTableNamesFromSelectWithParens visibleCtes selectWithParens

readTableNamesFromSimpleSelect :: Set.Set Text -> Ast.SimpleSelect -> Set.Set Text
readTableNamesFromSimpleSelect visibleCtes = \case
    Ast.NormalSimpleSelect targeting _into maybeFrom maybeWhere maybeGroup maybeHaving maybeWindow ->
        maybe Set.empty (readTableNamesFromTargeting visibleCtes) targeting
        <> maybe Set.empty (foldMap (readTableNamesFromTableRef visibleCtes) . toList) maybeFrom
        <> maybe Set.empty (readTableNamesFromAExpr visibleCtes) maybeWhere
        <> maybe Set.empty (foldMap (readTableNamesFromGroupByItem visibleCtes) . toList) maybeGroup
        <> maybe Set.empty (readTableNamesFromAExpr visibleCtes) maybeHaving
        <> maybe Set.empty (foldMap (readTableNamesFromWindowDefinition visibleCtes) . toList) maybeWindow
    Ast.BinSimpleSelect _op left _distinct right ->
        readTableNamesFromSelectClause visibleCtes left <> readTableNamesFromSelectClause visibleCtes right
    Ast.TableSimpleSelect relation ->
        let tableName = relationExprName relation
        in if tableName `Set.member` visibleCtes then Set.empty else Set.singleton tableName
    Ast.ValuesSimpleSelect _ -> Set.empty

readTableNamesFromTableRef :: Set.Set Text -> Ast.TableRef -> Set.Set Text
readTableNamesFromTableRef visibleCtes = \case
    Ast.RelationExprTableRef relation _alias _sample ->
        let tableName = relationExprName relation
        in if tableName `Set.member` visibleCtes then Set.empty else Set.singleton tableName
    Ast.JoinTableRef joinedTable _alias -> readTableNamesFromJoinedTable visibleCtes joinedTable
    Ast.SelectTableRef _lateral subquery _alias -> readTableNamesFromSelectWithParens visibleCtes subquery
    Ast.FuncTableRef _lateral function _alias -> readTableNamesFromFuncTable visibleCtes function

readTableNamesFromJoinedTable :: Set.Set Text -> Ast.JoinedTable -> Set.Set Text
readTableNamesFromJoinedTable visibleCtes = \case
    Ast.InParensJoinedTable inner -> readTableNamesFromJoinedTable visibleCtes inner
    Ast.MethJoinedTable method left right ->
        readTableNamesFromTableRef visibleCtes left
        <> readTableNamesFromTableRef visibleCtes right
        <> readTableNamesFromJoinMeth visibleCtes method

readTableNamesFromTargeting :: Set.Set Text -> Ast.Targeting -> Set.Set Text
readTableNamesFromTargeting visibleCtes = \case
    Ast.NormalTargeting targets -> foldMap (readTableNamesFromTargetEl visibleCtes) targets
    Ast.AllTargeting maybeTargets -> maybe Set.empty (foldMap (readTableNamesFromTargetEl visibleCtes)) maybeTargets
    Ast.DistinctTargeting maybeExpressions targets ->
        maybe Set.empty (foldMap (readTableNamesFromAExpr visibleCtes)) maybeExpressions
        <> foldMap (readTableNamesFromTargetEl visibleCtes) targets

readTableNamesFromTargetEl :: Set.Set Text -> Ast.TargetEl -> Set.Set Text
readTableNamesFromTargetEl visibleCtes = \case
    Ast.AliasedExprTargetEl expression _alias -> readTableNamesFromAExpr visibleCtes expression
    Ast.ImplicitlyAliasedExprTargetEl expression _alias -> readTableNamesFromAExpr visibleCtes expression
    Ast.ExprTargetEl expression -> readTableNamesFromAExpr visibleCtes expression
    Ast.AsteriskTargetEl -> Set.empty

readTableNamesFromAExpr :: Set.Set Text -> Ast.AExpr -> Set.Set Text
readTableNamesFromAExpr visibleCtes = \case
    Ast.CExprAExpr expression -> readTableNamesFromCExpr visibleCtes expression
    Ast.TypecastAExpr expression _typeName -> recurse expression
    Ast.CollateAExpr expression _collation -> recurse expression
    Ast.AtTimeZoneAExpr left right -> recurse left <> recurse right
    Ast.PlusAExpr expression -> recurse expression
    Ast.MinusAExpr expression -> recurse expression
    Ast.SymbolicBinOpAExpr left _operator right -> recurse left <> recurse right
    Ast.PrefixQualOpAExpr _operator expression -> recurse expression
    Ast.SuffixQualOpAExpr expression _operator -> recurse expression
    Ast.AndAExpr left right -> recurse left <> recurse right
    Ast.OrAExpr left right -> recurse left <> recurse right
    Ast.NotAExpr expression -> recurse expression
    Ast.VerbalExprBinOpAExpr left _not _operator right maybeEscape ->
        recurse left <> recurse right <> maybe Set.empty recurse maybeEscape
    Ast.ReversableOpAExpr expression _not operator ->
        recurse expression <> readTableNamesFromReversableOp visibleCtes operator
    Ast.IsnullAExpr expression -> recurse expression
    Ast.NotnullAExpr expression -> recurse expression
    Ast.OverlapsAExpr left right -> readTableNamesFromRow visibleCtes left <> readTableNamesFromRow visibleCtes right
    Ast.SubqueryAExpr expression _operator _subType subquery ->
        recurse expression <> either (readTableNamesFromSelectWithParens visibleCtes) recurse subquery
    Ast.UniqueAExpr subquery -> readTableNamesFromSelectWithParens visibleCtes subquery
    Ast.DefaultAExpr -> Set.empty
  where
    recurse = readTableNamesFromAExpr visibleCtes

readTableNamesFromCExpr :: Set.Set Text -> Ast.CExpr -> Set.Set Text
readTableNamesFromCExpr visibleCtes = \case
    Ast.ColumnrefCExpr _ -> Set.empty
    Ast.AexprConstCExpr _ -> Set.empty
    Ast.ParamCExpr _ _ -> Set.empty
    Ast.InParensCExpr expression _ -> readTableNamesFromAExpr visibleCtes expression
    Ast.CaseCExpr expression -> readTableNamesFromCaseExpr visibleCtes expression
    Ast.FuncCExpr expression -> readTableNamesFromFuncExpr visibleCtes expression
    Ast.SelectWithParensCExpr subquery _ -> readTableNamesFromSelectWithParens visibleCtes subquery
    Ast.ExistsCExpr subquery -> readTableNamesFromSelectWithParens visibleCtes subquery
    Ast.ArrayCExpr value -> either (readTableNamesFromSelectWithParens visibleCtes) (readTableNamesFromArrayExpr visibleCtes) value
    Ast.ExplicitRowCExpr maybeExpressions -> maybe Set.empty (foldMap (readTableNamesFromAExpr visibleCtes)) maybeExpressions
    Ast.ImplicitRowCExpr (Ast.ImplicitRow expressions lastExpression) ->
        foldMap (readTableNamesFromAExpr visibleCtes) expressions
        <> readTableNamesFromAExpr visibleCtes lastExpression
    Ast.GroupingCExpr expressions -> foldMap (readTableNamesFromAExpr visibleCtes) expressions

readTableNamesFromFuncExpr :: Set.Set Text -> Ast.FuncExpr -> Set.Set Text
readTableNamesFromFuncExpr visibleCtes = \case
    Ast.ApplicationFuncExpr (Ast.FuncApplication _name maybeParams) maybeWithinGroup maybeFilter maybeOver ->
        maybe Set.empty (readTableNamesFromFuncParams visibleCtes) maybeParams
        <> maybe Set.empty (readTableNamesFromSortClause visibleCtes) maybeWithinGroup
        <> maybe Set.empty (readTableNamesFromAExpr visibleCtes) maybeFilter
        <> maybe Set.empty (readTableNamesFromOverClause visibleCtes) maybeOver
    Ast.SubexprFuncExpr subexpr -> readTableNamesFromFuncSubexpr visibleCtes subexpr

readTableNamesFromFuncTable :: Set.Set Text -> Ast.FuncTable -> Set.Set Text
readTableNamesFromFuncTable visibleCtes = \case
    Ast.FuncExprFuncTable expression _ordinality -> readWindowless expression
    Ast.RowsFromFuncTable rows _ordinality -> foldMap (\(Ast.RowsfromItem expression _columns) -> readWindowless expression) rows
  where
    readWindowless (Ast.ApplicationFuncExprWindowless (Ast.FuncApplication _name maybeParams)) =
        maybe Set.empty (readTableNamesFromFuncParams visibleCtes) maybeParams
    readWindowless (Ast.CommonSubexprFuncExprWindowless subexpr) =
        readTableNamesFromFuncSubexpr visibleCtes subexpr

readTableNamesFromFuncParams :: Set.Set Text -> Ast.FuncApplicationParams -> Set.Set Text
readTableNamesFromFuncParams visibleCtes = \case
    Ast.NormalFuncApplicationParams _distinct expressions maybeSort ->
        foldMap (readTableNamesFromFuncArgExpr visibleCtes) expressions
        <> maybe Set.empty (readTableNamesFromSortClause visibleCtes) maybeSort
    Ast.VariadicFuncApplicationParams maybeExpressions lastExpression maybeSort ->
        maybe Set.empty (foldMap (readTableNamesFromFuncArgExpr visibleCtes)) maybeExpressions
        <> readTableNamesFromFuncArgExpr visibleCtes lastExpression
        <> maybe Set.empty (readTableNamesFromSortClause visibleCtes) maybeSort
    Ast.StarFuncApplicationParams -> Set.empty

readTableNamesFromFuncArgExpr :: Set.Set Text -> Ast.FuncArgExpr -> Set.Set Text
readTableNamesFromFuncArgExpr visibleCtes = \case
    Ast.ExprFuncArgExpr expression -> recurse expression
    Ast.ColonEqualsFuncArgExpr _name expression -> recurse expression
    Ast.EqualsGreaterFuncArgExpr _name expression -> recurse expression
  where
    recurse = readTableNamesFromAExpr visibleCtes

readTableNamesFromFuncSubexpr :: Set.Set Text -> Ast.FuncExprCommonSubexpr -> Set.Set Text
readTableNamesFromFuncSubexpr visibleCtes = \case
    Ast.CollationForFuncExprCommonSubexpr expression -> recurse expression
    Ast.CastFuncExprCommonSubexpr expression _ -> recurse expression
    Ast.TreatFuncExprCommonSubexpr expression _ -> recurse expression
    Ast.NullIfFuncExprCommonSubexpr left right -> recurse left <> recurse right
    Ast.CoalesceFuncExprCommonSubexpr expressions -> foldMap recurse expressions
    Ast.GreatestFuncExprCommonSubexpr expressions -> foldMap recurse expressions
    Ast.LeastFuncExprCommonSubexpr expressions -> foldMap recurse expressions
    Ast.ExtractFuncExprCommonSubexpr maybeList -> maybe Set.empty (\(Ast.ExtractList _ expression) -> recurse expression) maybeList
    Ast.OverlayFuncExprCommonSubexpr (Ast.OverlayList source placing from maybeFor) ->
        recurse source <> recurse placing <> recurse from <> maybe Set.empty recurse maybeFor
    Ast.PositionFuncExprCommonSubexpr maybeList ->
        maybe Set.empty (\(Ast.PositionList left right) -> recurseB left <> recurseB right) maybeList
    Ast.SubstringFuncExprCommonSubexpr maybeList -> maybe Set.empty readSubstring maybeList
    Ast.TrimFuncExprCommonSubexpr _modifier trimList -> readTrim trimList
    _ -> Set.empty
  where
    recurse = readTableNamesFromAExpr visibleCtes
    recurseB = readTableNamesFromBExpr visibleCtes
    readSubstring (Ast.ExprSubstrList expression fromFor) = recurse expression <> readFromFor fromFor
    readSubstring (Ast.ExprListSubstrList expressions) = foldMap recurse expressions
    readFromFor (Ast.FromForSubstrListFromFor from for) = recurse from <> recurse for
    readFromFor (Ast.ForFromSubstrListFromFor for from) = recurse for <> recurse from
    readFromFor (Ast.FromSubstrListFromFor from) = recurse from
    readFromFor (Ast.ForSubstrListFromFor for) = recurse for
    readTrim (Ast.ExprFromExprListTrimList expression expressions) = recurse expression <> foldMap recurse expressions
    readTrim (Ast.FromExprListTrimList expressions) = foldMap recurse expressions
    readTrim (Ast.ExprListTrimList expressions) = foldMap recurse expressions

readTableNamesFromCaseExpr :: Set.Set Text -> Ast.CaseExpr -> Set.Set Text
readTableNamesFromCaseExpr visibleCtes (Ast.CaseExpr maybeArg whenClauses maybeDefault) =
    maybe Set.empty recurse maybeArg
    <> foldMap (\(Ast.WhenClause condition result) -> recurse condition <> recurse result) whenClauses
    <> maybe Set.empty recurse maybeDefault
  where
    recurse = readTableNamesFromAExpr visibleCtes

readTableNamesFromArrayExpr :: Set.Set Text -> Ast.ArrayExpr -> Set.Set Text
readTableNamesFromArrayExpr visibleCtes = \case
    Ast.ExprListArrayExpr expressions -> foldMap (readTableNamesFromAExpr visibleCtes) expressions
    Ast.ArrayExprListArrayExpr arrays -> foldMap (readTableNamesFromArrayExpr visibleCtes) arrays
    Ast.EmptyArrayExpr -> Set.empty

readTableNamesFromRow :: Set.Set Text -> Ast.Row -> Set.Set Text
readTableNamesFromRow visibleCtes = \case
    Ast.ExplicitRowRow maybeExpressions -> maybe Set.empty (foldMap (readTableNamesFromAExpr visibleCtes)) maybeExpressions
    Ast.ImplicitRowRow (Ast.ImplicitRow expressions lastExpression) ->
        foldMap (readTableNamesFromAExpr visibleCtes) expressions
        <> readTableNamesFromAExpr visibleCtes lastExpression

readTableNamesFromReversableOp :: Set.Set Text -> Ast.AExprReversableOp -> Set.Set Text
readTableNamesFromReversableOp visibleCtes = \case
    Ast.DistinctFromAExprReversableOp expression -> recurse expression
    Ast.BetweenAExprReversableOp _not lower upper -> readTableNamesFromBExpr visibleCtes lower <> recurse upper
    Ast.BetweenSymmetricAExprReversableOp lower upper -> readTableNamesFromBExpr visibleCtes lower <> recurse upper
    Ast.InAExprReversableOp (Ast.SelectInExpr subquery) -> readTableNamesFromSelectWithParens visibleCtes subquery
    Ast.InAExprReversableOp (Ast.ExprListInExpr expressions) -> foldMap recurse expressions
    _ -> Set.empty
  where
    recurse = readTableNamesFromAExpr visibleCtes

readTableNamesFromBExpr :: Set.Set Text -> Ast.BExpr -> Set.Set Text
readTableNamesFromBExpr visibleCtes = \case
    Ast.CExprBExpr expression -> readTableNamesFromCExpr visibleCtes expression
    Ast.TypecastBExpr expression _ -> recurse expression
    Ast.PlusBExpr expression -> recurse expression
    Ast.MinusBExpr expression -> recurse expression
    Ast.SymbolicBinOpBExpr left _ right -> recurse left <> recurse right
    Ast.QualOpBExpr _ expression -> recurse expression
    Ast.IsOpBExpr expression _ _ -> recurse expression
  where
    recurse = readTableNamesFromBExpr visibleCtes

readTableNamesFromJoinMeth :: Set.Set Text -> Ast.JoinMeth -> Set.Set Text
readTableNamesFromJoinMeth visibleCtes = \case
    Ast.QualJoinMeth _ (Ast.OnJoinQual expression) -> readTableNamesFromAExpr visibleCtes expression
    _ -> Set.empty

readTableNamesFromGroupByItem :: Set.Set Text -> Ast.GroupByItem -> Set.Set Text
readTableNamesFromGroupByItem visibleCtes = \case
    Ast.ExprGroupByItem expression -> recurse expression
    Ast.RollupGroupByItem expressions -> foldMap recurse expressions
    Ast.CubeGroupByItem expressions -> foldMap recurse expressions
    Ast.GroupingSetsGroupByItem groups -> foldMap (readTableNamesFromGroupByItem visibleCtes) groups
    Ast.EmptyGroupingSetGroupByItem -> Set.empty
  where
    recurse = readTableNamesFromAExpr visibleCtes

readTableNamesFromSortClause :: Set.Set Text -> Ast.SortClause -> Set.Set Text
readTableNamesFromSortClause visibleCtes = foldMap \case
    Ast.UsingSortBy expression _ _ -> readTableNamesFromAExpr visibleCtes expression
    Ast.AscDescSortBy expression _ _ -> readTableNamesFromAExpr visibleCtes expression

readTableNamesFromWindowDefinition :: Set.Set Text -> Ast.WindowDefinition -> Set.Set Text
readTableNamesFromWindowDefinition visibleCtes (Ast.WindowDefinition _name specification) =
    readTableNamesFromWindowSpecification visibleCtes specification

readTableNamesFromOverClause :: Set.Set Text -> Ast.OverClause -> Set.Set Text
readTableNamesFromOverClause visibleCtes = \case
    Ast.WindowOverClause specification -> readTableNamesFromWindowSpecification visibleCtes specification
    Ast.ColIdOverClause _ -> Set.empty

readTableNamesFromWindowSpecification :: Set.Set Text -> Ast.WindowSpecification -> Set.Set Text
readTableNamesFromWindowSpecification visibleCtes (Ast.WindowSpecification _existing maybePartition maybeSort maybeFrame) =
    maybe Set.empty (foldMap (readTableNamesFromAExpr visibleCtes)) maybePartition
    <> maybe Set.empty (readTableNamesFromSortClause visibleCtes) maybeSort
    <> maybe Set.empty (readTableNamesFromFrameClause visibleCtes) maybeFrame

readTableNamesFromFrameClause :: Set.Set Text -> Ast.FrameClause -> Set.Set Text
readTableNamesFromFrameClause visibleCtes (Ast.FrameClause _mode extent _exclusion) = case extent of
    Ast.SingularFrameExtent bound -> readTableNamesFromFrameBound visibleCtes bound
    Ast.BetweenFrameExtent lower upper ->
        readTableNamesFromFrameBound visibleCtes lower <> readTableNamesFromFrameBound visibleCtes upper

readTableNamesFromFrameBound :: Set.Set Text -> Ast.FrameBound -> Set.Set Text
readTableNamesFromFrameBound visibleCtes = \case
    Ast.PrecedingFrameBound expression -> readTableNamesFromAExpr visibleCtes expression
    Ast.FollowingFrameBound expression -> readTableNamesFromAExpr visibleCtes expression
    _ -> Set.empty

readTableNamesFromSelectLimit :: Set.Set Text -> Ast.SelectLimit -> Set.Set Text
readTableNamesFromSelectLimit visibleCtes = \case
    Ast.LimitOffsetSelectLimit limitClause offsetClause -> readLimit limitClause <> readOffset offsetClause
    Ast.OffsetLimitSelectLimit offsetClause limitClause -> readOffset offsetClause <> readLimit limitClause
    Ast.LimitSelectLimit limitClause -> readLimit limitClause
    Ast.OffsetSelectLimit offsetClause -> readOffset offsetClause
  where
    readLimit (Ast.LimitLimitClause value maybeExpression) =
        readLimitValue value <> maybe Set.empty (readTableNamesFromAExpr visibleCtes) maybeExpression
    readLimit (Ast.FetchOnlyLimitClause _ maybeValue _) = maybe Set.empty readFetchValue maybeValue
    readLimitValue (Ast.ExprSelectLimitValue expression) = readTableNamesFromAExpr visibleCtes expression
    readLimitValue Ast.AllSelectLimitValue = Set.empty
    readOffset (Ast.ExprOffsetClause expression) = readTableNamesFromAExpr visibleCtes expression
    readOffset (Ast.FetchFirstOffsetClause value _) = readFetchValue value
    readFetchValue (Ast.ExprSelectFetchFirstValue expression) = readTableNamesFromCExpr visibleCtes expression
    readFetchValue (Ast.NumSelectFetchFirstValue _ _) = Set.empty

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
-- json[b]_build_object and json[b]_build_array return JSON null/object/array
-- values rather than SQL NULL when their arguments are NULL.
isNonNullableFunction :: Ast.FuncName -> Bool
isNonNullableFunction funcName =
    funcNameToText funcName `elem`
        [ "count"
        , "row_number"
        , "rank"
        , "dense_rank"
        , "json_build_object"
        , "jsonb_build_object"
        , "json_build_array"
        , "jsonb_build_array"
        ]

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
    resolveHint tablesByName (index, ParamHint { phTable, phColumn }) = do
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
                        -- The quasiquoter annotates each ${...} with the column's *scalar*
                        -- Haskell type and lets 'IHP.TypedSql.ParamEncoder.typedSqlParam'
                        -- accept the value as a scalar, Maybe, list, or list-of-Maybe. So we
                        -- strip the column's Maybe wrapper and ignore phArray here — the
                        -- value's own shape (e.g. a list for IN/ANY) selects the encoding.
                        let scalarType = stripMaybeType baseType
                        pure (Just (index, scalarType))

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

----------------------------------------------------------------------
-- Star select detection
----------------------------------------------------------------------

-- | Detect SELECT * and SELECT table.* in the target list of a query.
-- Returns a list of descriptive strings (e.g. ["*", "items.*"]) for use in error messages.
-- Does NOT flag COUNT(*) (function argument) or (expr).* (composite expansion).
detectStarSelects :: Ast.PreparableStmt -> [String]
detectStarSelects = \case
    Ast.SelectPreparableStmt selectStmt -> starFromSelectStmt selectStmt
    _ -> []

starFromSelectStmt :: Ast.SelectStmt -> [String]
starFromSelectStmt (Left (Ast.SelectNoParens _with selectClause _sort _limit _lock)) =
    starFromSelectClause selectClause
starFromSelectStmt (Right parens) = starFromSelectWithParens parens

starFromSelectWithParens :: Ast.SelectWithParens -> [String]
starFromSelectWithParens (Ast.NoParensSelectWithParens (Ast.SelectNoParens _with selectClause _sort _limit _lock)) =
    starFromSelectClause selectClause
starFromSelectWithParens (Ast.WithParensSelectWithParens inner) =
    starFromSelectWithParens inner

starFromSelectClause :: Ast.SelectClause -> [String]
starFromSelectClause (Left simpleSelect) = starFromSimpleSelect simpleSelect
starFromSelectClause (Right parens) = starFromSelectWithParens parens

starFromSimpleSelect :: Ast.SimpleSelect -> [String]
starFromSimpleSelect = \case
    Ast.NormalSimpleSelect maybeTargeting _into _from _where _group _having _window ->
        case maybeTargeting of
            Just (Ast.NormalTargeting targets) -> concatMap starFromTargetEl (toList targets)
            Just (Ast.DistinctTargeting _ targets) -> concatMap starFromTargetEl (toList targets)
            _ -> []
    Ast.BinSimpleSelect _op left _distinct right ->
        starFromSelectClause left <> starFromSelectClause right
    _ -> []

starFromTargetEl :: Ast.TargetEl -> [String]
starFromTargetEl = \case
    Ast.AsteriskTargetEl -> ["*"]
    Ast.ExprTargetEl expr -> starFromExpr expr
    Ast.ImplicitlyAliasedExprTargetEl expr _ -> starFromExpr expr
    Ast.AliasedExprTargetEl expr _ -> starFromExpr expr

-- | Detect table.* pattern: a simple column reference (identifier) followed by AllIndirectionEl.
-- Does NOT flag (expr).* like (ROW(...))::type.* which is composite expansion.
starFromExpr :: Ast.AExpr -> [String]
starFromExpr = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref ident (Just indirection)))
        | any isAllIndirection (toList indirection) ->
            [Text.unpack (identToText ident) <> ".*"]
    _ -> []
  where
    isAllIndirection Ast.AllIndirectionEl = True
    isAllIndirection _ = False

----------------------------------------------------------------------
-- INSERT without explicit column list detection
----------------------------------------------------------------------

-- | Detect INSERT statements without an explicit column list.
-- Returns descriptive strings (e.g. ["INSERT INTO foo"]) for use in error messages.
-- Allows `INSERT INTO foo DEFAULT VALUES` since it has no positional column binding.
detectInsertWithoutColumns :: Ast.PreparableStmt -> [String]
detectInsertWithoutColumns = \case
    Ast.InsertPreparableStmt (Ast.InsertStmt _with target rest _onConflict _ret) ->
        case rest of
            Ast.SelectInsertRest Nothing _override _selectStmt ->
                let Ast.InsertTarget qname _alias = target
                in ["INSERT INTO " <> Text.unpack (qualifiedNameToText qname)]
            _ -> []
    _ -> []
