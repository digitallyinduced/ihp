module IHP.TypedSql.ParamHints
    ( SqlToken (..)
    , ParamHint (..)
    , tokenizeSql
    , buildAliasMap
    , collectParamHints
    , resolveParamHintTypes
    ) where

import           Control.Monad               (guard)
import qualified Data.Char                   as Char
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import qualified Data.String.Conversions     as CS
import qualified Database.PostgreSQL.LibPQ   as PQ
import qualified Language.Haskell.TH         as TH
import           IHP.Prelude

import           IHP.TypedSql.Metadata       (ColumnMeta (..), DescribeColumn (..),
                                              PgTypeInfo, TableMeta (..))
import           IHP.TypedSql.TypeMapping    (hsTypeForColumn)

-- | Minimal SQL token used for placeholder context inspection.
-- This is a lightweight scanner, not a full SQL parser.
data SqlToken
    = TokIdent !Text
    | TokSymbol !Char
    | TokParam !Int
    deriving (Eq, Show)

-- | A derived hint about the expected type of a placeholder.
-- The quasiquoter uses this to coerce ${...} to a column-compatible type.
data ParamHint = ParamHint
    { phIndex  :: !Int
    , phTable  :: !Text
    , phColumn :: !Text
    , phArray  :: !Bool
    }
    deriving (Eq, Show)

-- | Tokenize SQL just enough to locate placeholders and nearby identifiers.
-- This feeds alias detection and parameter hint extraction in typedSql.
tokenizeSql :: String -> [SqlToken]
tokenizeSql = go [] where
    go acc [] = reverse acc
    go acc ('-':'-':rest) = go acc (dropLineComment rest)
    go acc ('/':'*':rest) = go acc (dropBlockComment rest)
    go acc ('\'':rest) = go acc (dropStringLiteral rest)
    go acc ('"':rest) =
        let (ident, remaining) = parseQuotedIdent rest
        in go (TokIdent ident : acc) remaining
    go acc ('$':rest) =
        let (digits, remaining) = span Char.isDigit rest
        in if null digits
            then go acc remaining
            else go (TokParam (digitsToInt digits) : acc) remaining
    go acc (c:rest)
        | Char.isSpace c = go acc rest
        | isIdentStart c =
            let (identTail, remaining) = span isIdentChar rest
                identText = Text.toLower (CS.cs (c : identTail))
            in go (TokIdent identText : acc) remaining
        | isSymbolToken c = go (TokSymbol c : acc) rest
        | otherwise = go acc rest

    isIdentStart ch = Char.isLetter ch || ch == '_'
    isIdentChar ch = Char.isAlphaNum ch || ch == '_' || ch == '$'
    isSymbolToken ch = ch `elem` ['.', '=', '(', ')', ',']

    dropLineComment = dropWhile (/= '\n')
    dropBlockComment = dropUntil "*/"
    dropStringLiteral = dropSingleQuoted

    dropUntil _ [] = []
    dropUntil pattern@(p1:p2:_) (x:y:rest)
        | x == p1 && y == p2 = rest
        | otherwise = dropUntil pattern (y:rest)
    dropUntil _ rest = rest

    dropSingleQuoted [] = []
    dropSingleQuoted ('\'':'\'':xs) = dropSingleQuoted xs
    dropSingleQuoted ('\'':xs) = xs
    dropSingleQuoted (_:xs) = dropSingleQuoted xs

    parseQuotedIdent = go "" where
        go acc [] = (Text.toLower (CS.cs (reverse acc)), [])
        go acc ('"':'"':xs) = go ('"':acc) xs
        go acc ('"':xs) = (Text.toLower (CS.cs (reverse acc)), xs)
        go acc (x:xs) = go (x:acc) xs

-- | Convert a list of digit chars to an Int for $1/$2 token indices.
digitsToInt :: String -> Int
digitsToInt = foldl' (\acc digit -> acc * 10 + Char.digitToInt digit) 0

-- | Safe indexing helper for the token stream.
tokenAtIndex :: [a] -> Int -> Maybe a
tokenAtIndex xs ix =
    case List.drop ix xs of
        (value:_) -> Just value
        [] -> Nothing

-- | Keywords that should not be treated as table aliases.
reservedKeywords :: Set.Set Text
reservedKeywords =
    Set.fromList (map Text.pack
        [ "as", "where", "join", "inner", "left", "right", "full", "cross"
        , "on", "group", "order", "limit", "offset", "having", "union"
        , "intersect", "except", "returning", "set", "values", "from", "update"
        , "delete", "insert", "select"
        ])

-- | Keywords that introduce a table reference.
clauseKeywords :: Set.Set Text
clauseKeywords = Set.fromList (map Text.pack ["from", "join", "update", "into"])

-- | Build a map of aliases to base table names from FROM/JOIN clauses.
-- Used to resolve qualified columns when inferring parameter types.
buildAliasMap :: [SqlToken] -> Map.Map Text Text
buildAliasMap tokens = go tokens Map.empty where
    go [] acc = acc
    go (TokIdent keyword : rest) acc
        | keyword `Set.member` clauseKeywords =
            case parseTable rest of
                Nothing -> go rest acc
                Just (tableName, afterTable) ->
                    let (alias, afterAlias) = parseAlias afterTable
                        acc' = Map.insert tableName tableName acc
                        acc'' = maybe acc' (\name -> Map.insert name tableName acc') alias
                    in go afterAlias acc''
        | otherwise = go rest acc
    go (_:rest) acc = go rest acc

    parseTable (TokIdent _schemaName : TokSymbol '.' : TokIdent tableName : rest) =
        Just (tableName, rest)
    parseTable (TokIdent tableName : rest) =
        Just (tableName, rest)
    parseTable _ = Nothing

    parseAlias (TokIdent aliasKeyword : TokIdent alias : rest)
        | aliasKeyword == Text.pack "as" = (Just alias, rest)
    parseAlias (TokIdent alias : rest)
        | alias `Set.notMember` reservedKeywords = (Just alias, rest)
    parseAlias rest = (Nothing, rest)

-- | Find placeholder sites that look like column comparisons and capture their types.
-- This is how typedSql infers a more precise parameter type than the DB-provided OID.
collectParamHints :: [SqlToken] -> Map.Map Text Text -> Map.Map Int ParamHint
collectParamHints tokens aliasMap =
    let defaultTable = singleTable aliasMap
    in tokens
        |> zip [0..]
        |> mapMaybe (hintForToken aliasMap defaultTable)
        |> foldl' mergeHints Map.empty
        |> Map.mapMaybe id
  where
    tokenAt ix
        | ix < 0 = Nothing
        | otherwise = tokenAtIndex tokens ix

    singleTable aliases =
        case Set.toList (Set.fromList (Map.elems aliases)) of
            [table] -> Just table
            _ -> Nothing

    hasDotBefore ix =
        case tokenAt (ix - 1) of
            Just (TokSymbol '.') -> True
            _ -> False

    hasDotAfter ix =
        case tokenAt (ix + 1) of
            Just (TokSymbol '.') -> True
            _ -> False

    hintForToken aliases defaultTable (ix, TokParam index) =
        let matches = catMaybes
                [ matchEqRight aliases ix index
                , matchEqLeft aliases ix index
                , matchInRight aliases ix index
                , matchAnyRight aliases ix index
                , matchEqRightUnqualified defaultTable ix index
                , matchEqLeftUnqualified defaultTable ix index
                , matchInRightUnqualified defaultTable ix index
                , matchAnyRightUnqualified defaultTable ix index
                ]
        in listToMaybe matches
    hintForToken _ _ _ = Nothing

    matchEqRight aliases ix index = do
        TokSymbol '=' <- tokenAt (ix - 1)
        TokIdent column <- tokenAt (ix - 2)
        TokSymbol '.' <- tokenAt (ix - 3)
        TokIdent tableRef <- tokenAt (ix - 4)
        tableName <- Map.lookup tableRef aliases
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = False }

    matchEqRightUnqualified defaultTable ix index = do
        tableName <- defaultTable
        TokSymbol '=' <- tokenAt (ix - 1)
        TokIdent column <- tokenAt (ix - 2)
        guard (not (hasDotBefore (ix - 2)))
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = False }

    matchEqLeft aliases ix index = do
        TokSymbol '=' <- tokenAt (ix + 1)
        TokIdent tableRef <- tokenAt (ix + 2)
        TokSymbol '.' <- tokenAt (ix + 3)
        TokIdent column <- tokenAt (ix + 4)
        tableName <- Map.lookup tableRef aliases
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = False }

    matchEqLeftUnqualified defaultTable ix index = do
        tableName <- defaultTable
        TokSymbol '=' <- tokenAt (ix + 1)
        TokIdent column <- tokenAt (ix + 2)
        guard (not (hasDotAfter (ix + 2)))
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = False }

    matchInRight aliases ix index = do
        TokSymbol '(' <- tokenAt (ix - 1)
        TokIdent keyword <- tokenAt (ix - 2)
        guard (keyword == Text.pack "in")
        TokIdent column <- tokenAt (ix - 3)
        TokSymbol '.' <- tokenAt (ix - 4)
        TokIdent tableRef <- tokenAt (ix - 5)
        tableName <- Map.lookup tableRef aliases
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = True }

    matchInRightUnqualified defaultTable ix index = do
        tableName <- defaultTable
        TokSymbol '(' <- tokenAt (ix - 1)
        TokIdent keyword <- tokenAt (ix - 2)
        guard (keyword == Text.pack "in")
        TokIdent column <- tokenAt (ix - 3)
        guard (not (hasDotBefore (ix - 3)))
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = True }

    matchAnyRight aliases ix index = do
        TokSymbol '(' <- tokenAt (ix - 1)
        TokIdent keyword <- tokenAt (ix - 2)
        guard (keyword == Text.pack "any")
        TokSymbol '=' <- tokenAt (ix - 3)
        TokIdent column <- tokenAt (ix - 4)
        TokSymbol '.' <- tokenAt (ix - 5)
        TokIdent tableRef <- tokenAt (ix - 6)
        tableName <- Map.lookup tableRef aliases
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = True }

    matchAnyRightUnqualified defaultTable ix index = do
        tableName <- defaultTable
        TokSymbol '(' <- tokenAt (ix - 1)
        TokIdent keyword <- tokenAt (ix - 2)
        guard (keyword == Text.pack "any")
        TokSymbol '=' <- tokenAt (ix - 3)
        TokIdent column <- tokenAt (ix - 4)
        guard (not (hasDotBefore (ix - 4)))
        pure ParamHint { phIndex = index, phTable = tableName, phColumn = column, phArray = True }

    mergeHints acc hint =
        Map.alter (mergeHint hint) (phIndex hint) acc

    mergeHint hint Nothing = Just (Just hint)
    mergeHint hint (Just Nothing) = Just Nothing
    mergeHint hint (Just (Just existing))
        | existing == hint = Just (Just existing)
        | otherwise = Just Nothing

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

-- | Strip a top-level Maybe wrapper to get the base column type.
-- Used when parameter hints should be non-nullable inputs.
stripMaybeType :: TH.Type -> TH.Type
stripMaybeType (TH.AppT (TH.ConT maybeName) inner)
    | maybeName == ''Maybe = inner
stripMaybeType other = other
