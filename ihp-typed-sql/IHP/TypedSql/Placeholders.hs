{-# LANGUAGE TemplateHaskell #-}

module IHP.TypedSql.Placeholders
    ( PlaceholderPlan (..)
    , planPlaceholders
    , parseExpr
    ) where

import qualified Data.String.Conversions     as CS
import qualified Language.Haskell.Meta.Parse as HaskellMeta
import qualified Language.Haskell.TH         as TH
import           IHP.Prelude

-- | Output of placeholder parsing used by the typedSql quasiquoter.
-- It carries the SQL variant for describe, the SQL variant for runtime execution,
-- and the original Haskell placeholder expressions to splice.
data PlaceholderPlan = PlaceholderPlan
    { ppDescribeSql :: !String -- ^ SQL with $1/$2 placeholders for the describe step.
    , ppRuntimeSql  :: !String -- ^ SQL with ? placeholders for hasql snippet execution.
    , ppExprs       :: ![String] -- ^ Raw Haskell expressions from ${...}.
    }

-- | Replace ${expr} placeholders with PostgreSQL-style $1 for describe and ? for runtime.
-- High-level: turns a templated SQL string into SQL strings plus expr list.
planPlaceholders :: String -> PlaceholderPlan
planPlaceholders = go 1 "" "" [] where
    go _ accDescribe accRuntime exprs [] =
        PlaceholderPlan
            { ppDescribeSql = reverse accDescribe
            , ppRuntimeSql = reverse accRuntime
            , ppExprs = reverse exprs
            }
    go n accDescribe accRuntime exprs ('$':'{':rest) =
        let (expr, after) = breakOnClosing 0 "" rest -- parse until matching }
            describeToken = reverse ('$' : CS.cs (show n))
        in go (n + 1)
              (describeToken <> accDescribe)
              ('\0' : accRuntime)
              (expr : exprs)
              after
    go n accDescribe accRuntime exprs (c:rest) =
        go n (c : accDescribe) (c : accRuntime) exprs rest

    breakOnClosing depth acc [] = (reverse acc, []) -- no closing brace found
    breakOnClosing depth acc ('{':xs) = breakOnClosing (depth + 1) ('{':acc) xs -- nested { increases depth
    breakOnClosing depth acc ('}':xs)
        | depth == 0 = (reverse acc, xs) -- close the current placeholder
        | otherwise = breakOnClosing (depth - 1) ('}':acc) xs -- close a nested brace
    breakOnClosing depth acc (x:xs) = breakOnClosing depth (x:acc) xs -- accumulate placeholder chars

-- | Parse a placeholder expression into TH.
-- Used by typedSql to turn ${...} splices into typed expressions.
parseExpr :: String -> TH.ExpQ
parseExpr exprText =
    case HaskellMeta.parseExp exprText of
        Left err -> fail ("typedSql: failed to parse expression {" <> exprText <> "}: " <> err) -- parse error
        Right expr -> pure expr -- success: return parsed TH expression
