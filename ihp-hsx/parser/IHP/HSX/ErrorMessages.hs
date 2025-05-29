{-# LANGUAGE OverloadedStrings #-}
module IHP.HSX.ErrorMessages (
    improveHaskellError,
    improveHSXParseError
) where

import qualified Prelude
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

-- | Improve Haskell expression errors inside HSX
improveHaskellError :: String -> String -> Int -> Int -> String
improveHaskellError code lineStr colStr err
    | "Variable not in scope" `isInfixOf` err =
        let var = extractVarName err
        in formatError code lineStr colStr err $
            "Suggestion: Make sure the variable '" ++ var ++ "' is defined in the current scope."
    | "No instance for" `isInfixOf` err && "ToHtml" `isInfixOf` err =
        let typeName = extractTypeName err
        in formatError code lineStr colStr err $
            "Suggestion: Add a ToHtml instance for '" ++ typeName ++ "' or convert it to Text."
    | "Couldn't match expected type" `isInfixOf` err && "->" `isInfixOf` err =
        formatError code lineStr colStr err "Suggestion: Check for partially applied functions in your HSX block."
    | otherwise = formatError code lineStr colStr err ""

-- | Improve HSX parser errors (e.g., missing tags, invalid tag names)
improveHSXParseError :: String -> Int -> Int -> String -> String
improveHSXParseError code line col err
    | "Invalid tag name" `isInfixOf` err =
        let tag = extractTagName err
        in formatError code (show line) (show col) err $
            "Suggestion: Use a valid HTML tag name or add it to the allowed custom tags."
    | "expected closing tag" `isInfixOf` (map toLower err) =
        formatError code (show line) (show col) err "Suggestion: Make sure all opening tags have matching closing tags."
    | otherwise = formatError code (show line) (show col) err ""

-- | Extract variable name from GHC error
extractVarName :: String -> String
extractVarName err =
    let pat = "`([a-zA-Z0-9_']+)`" :: String
        res = err =~ pat :: [[String]]
    in case res of
        (_:name:_) : _ -> name
        _ -> ""

-- | Extract type name from GHC error
extractTypeName :: String -> String
extractTypeName err =
    let pat = "`([a-zA-Z0-9_']+)`" :: String
        res = err =~ pat :: [[String]]
    in case res of
        (_:name:_) : _ -> name
        _ -> ""

-- | Extract tag name from error
extractTagName :: String -> String
extractTagName err =
    let pat = "<([a-zA-Z0-9:_-]+)>" :: String
        res = err =~ pat :: [[String]]
    in case res of
        (_:name:_) : _ -> name
        _ -> ""

-- | Format the error with context and suggestion
formatError :: String -> String -> String -> String -> String -> String
formatError code lineStr colStr err suggestion =
    let line = read lineStr :: Int
        col = read colStr :: Int
        codeLines = lines code
        context = getContext codeLines line col
    in Prelude.unlines $
        [ "[HSX Error] " ++ err
        , "--> Line " ++ lineStr ++ ", Column " ++ colStr
        , "Context:"
        , context
        ] ++ if null suggestion then [] else [suggestion]

-- | Get code context (2 lines before and after, highlight col)
getContext :: [String] -> Int -> Int -> String
getContext codeLines line col =
    let start = max 1 (line - 2)
        end = min (length codeLines) (line + 2)
        numbered = zip [1..] codeLines
        contextLines = filter (\(n,_) -> n >= start && n <= end) numbered
        markLine (n, l) | n == line = take (col-1) l ++ ">>>" ++ drop (col-1) l ++ "<<<"
                        | otherwise = l
    in Prelude.unlines $ map markLine contextLines 