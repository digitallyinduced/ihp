{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module: IHP.HSX.ErrorMessage
Description: Enhanced error messages for HSX parser
Copyright: (c) digitally induced GmbH, 2022

This module provides enhanced error reporting for HSX parsing errors.
It extracts context from source code and provides helpful suggestions
to developers when HSX parsing fails.
-}
module IHP.HSX.ErrorMessage
( HSXError (..)
, HSXErrorType (..)
, enhanceHSXError
, formatHSXError
, extractSourceContext
, suggestFixForError
) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Text.Megaparsec (SourcePos(..), unPos)
import qualified Text.Megaparsec as Megaparsec
import Data.String.Conversions (cs)

-- | Represents different types of HSX errors with specific context
data HSXErrorType
    = InvalidTagName Text                    -- ^ Invalid HTML tag name
    | InvalidAttributeName Text             -- ^ Invalid HTML attribute name
    | UnmatchedTag Text Text                -- ^ Expected tag, actual tag
    | UndefinedVariable Text                -- ^ Variable not in scope
    | MissingToHtmlInstance Text            -- ^ Type missing ToHtml instance
    | DuplicateAttribute Text              -- ^ Duplicate attribute in tag
    | SyntaxError Text                     -- ^ Generic syntax error
    | HaskellExpressionError Text Int Int   -- ^ Error in embedded Haskell expression
    deriving (Eq, Show)

-- | Enhanced HSX error with context and suggestions
data HSXError = HSXError
    { errorType :: HSXErrorType
    , sourcePos :: SourcePos
    , sourceCode :: Text
    , contextLines :: (Text, Text, Text)  -- ^ (before, current, after)
    , suggestions :: [Text]
    } deriving (Show)

-- | Extracts source context around an error position
extractSourceContext :: SourcePos -> Text -> (Text, Text, Text)
extractSourceContext pos sourceCode =
    let
        lines' = Text.splitOn "\n" sourceCode
        lineNum = unPos (sourceLine pos) - 1  -- Convert to 0-based indexing
        
        beforeLine = if lineNum > 0 
                    then List.take 1 (List.drop (lineNum - 1) lines')
                    else []
        currentLine = if lineNum < length lines'
                     then List.take 1 (List.drop lineNum lines')
                     else []
        afterLine = if lineNum + 1 < length lines'
                   then List.take 1 (List.drop (lineNum + 1) lines')
                   else []
                   
        before = case beforeLine of
            [line] -> line
            _ -> ""
        current = case currentLine of
            [line] -> line
            _ -> ""
        after = case afterLine of
            [line] -> line
            _ -> ""
    in
        (before, current, after)

-- | Provides helpful suggestions based on error type
suggestFixForError :: HSXErrorType -> [Text]
suggestFixForError errorType = case errorType of
    InvalidTagName tagName ->
        [ "'" <> tagName <> "' is not a valid HTML tag name"
        , "Check if you meant a similar valid tag like: " <> suggestSimilarTag tagName
        , "For custom web components, use kebab-case with at least one hyphen (e.g., 'my-component')"
        , "Use 'uncheckedHsx' if you need to use non-standard tag names"
        ]
        
    InvalidAttributeName attrName ->
        [ "'" <> attrName <> "' is not a valid HTML attribute name"
        , "Valid HTML attributes include standard ones like 'class', 'id', 'style'"
        , "Data attributes must start with 'data-' (e.g., 'data-toggle')"
        , "ARIA attributes must start with 'aria-' (e.g., 'aria-label')"
        , "Use 'customHsx' with additionalAttributeNames if you need custom attributes"
        ]
        
    UnmatchedTag expected actual ->
        [ "Opening tag '<" <> expected <> ">' doesn't match closing tag '</" <> actual <> ">'"
        , "Make sure every opening tag has a matching closing tag"
        , "Check for typos in tag names"
        , "Ensure proper nesting of HTML elements"
        ]
        
    UndefinedVariable varName ->
        [ "Variable '" <> varName <> "' is not in scope"
        , "Make sure the variable is defined before using it in HSX"
        , "Check for typos in the variable name"
        , "Import the module that defines this variable if it's from another module"
        ]
        
    MissingToHtmlInstance typeName ->
        [ "Type '" <> typeName <> "' doesn't have a ToHtml instance"
        , "Add 'import IHP.HSX.ToHtml' to import common ToHtml instances"
        , "Define a custom ToHtml instance for your type:"
        , "  instance ToHtml " <> typeName <> " where"
        , "    toHtml value = [hsx|{show value}|]"
        ]
        
    DuplicateAttribute attrName ->
        [ "Attribute '" <> attrName <> "' appears multiple times in the same tag"
        , "Remove duplicate attributes"
        , "If you need conditional attributes, use Haskell expressions"
        ]
        
    SyntaxError msg ->
        [ "Syntax error: " <> msg
        , "Check for missing closing tags, quotes, or brackets"
        , "Make sure HSX syntax is properly formed"
        ]
        
    HaskellExpressionError expr line col ->
        [ "Error in Haskell expression: " <> expr
        , "Check the syntax of the Haskell code inside {}"
        , "Make sure all variables are in scope"
        , "Verify that all functions are imported"
        ]

-- | Suggests similar valid HTML tags for common typos
suggestSimilarTag :: Text -> Text
suggestSimilarTag tagName = case Text.toLower tagName of
    "img" -> "img (self-closing: <img/>)"
    "para" -> "p"
    "paragraph" -> "p" 
    "link" -> "a (for links) or link (for stylesheets)"
    "bold" -> "b or strong"
    "italic" -> "i or em"
    "button" -> "button"
    "form" -> "form"
    "input" -> "input (self-closing: <input/>)"
    "textarea" -> "textarea"
    "select" -> "select"
    "option" -> "option"
    "table" -> "table"
    "row" -> "tr"
    "cell" -> "td or th"
    "header" -> "header, h1, h2, h3, h4, h5, or h6"
    "footer" -> "footer"
    "nav" -> "nav"
    "section" -> "section"
    "article" -> "article"
    "aside" -> "aside"
    "main" -> "main"
    _ -> "div, span, or check the HTML specification"

-- | Enhances a basic parse error with additional context and suggestions
enhanceHSXError :: SourcePos -> Text -> Text -> HSXError
enhanceHSXError pos sourceCode errorMsg =
    let
        contextLines = extractSourceContext pos sourceCode
        errorType = classifyError errorMsg
        suggestions = suggestFixForError errorType
    in
        HSXError
            { errorType = errorType
            , sourcePos = pos
            , sourceCode = sourceCode
            , contextLines = contextLines
            , suggestions = suggestions
            }

-- | Classifies an error message into a specific error type
classifyError :: Text -> HSXErrorType
classifyError msg
    | "Invalid tag name:" `Text.isInfixOf` msg =
        let tagName = Text.strip $ Text.drop (Text.length "Invalid tag name: ") msg
        in InvalidTagName tagName
    | "Invalid attribute name:" `Text.isInfixOf` msg =
        let attrName = Text.strip $ Text.drop (Text.length "Invalid attribute name: ") msg
        in InvalidAttributeName attrName
    | "Duplicate attribute found in tag:" `Text.isInfixOf` msg =
        let attrName = extractAttrFromDuplicateMsg msg
        in DuplicateAttribute attrName
    | "unexpected" `Text.isInfixOf` msg && "expecting" `Text.isInfixOf` msg =
        SyntaxError msg
    | otherwise = SyntaxError msg
  where
    extractAttrFromDuplicateMsg :: Text -> Text
    extractAttrFromDuplicateMsg msg = 
        case Text.words msg of
            ws | length ws > 6 -> ws !! 6  -- Extract attribute name from error message
            _ -> "unknown"

-- | Formats an enhanced HSX error for display
formatHSXError :: HSXError -> Text
formatHSXError HSXError{..} =
    let
        (beforeLine, currentLine, afterLine) = contextLines
        lineNum = unPos (sourceLine sourcePos)
        colNum = unPos (sourceColumn sourcePos)
        
        -- Format the error header
        errorHeader = "HSX Parse Error at " <> cs (sourceName sourcePos) 
                     <> ":" <> Text.pack (show lineNum) 
                     <> ":" <> Text.pack (show colNum)
        
        -- Format the source context with line numbers
        sourceContext = formatSourceContext lineNum beforeLine currentLine afterLine colNum
        
        -- Format suggestions
        suggestionText = if null suggestions
                        then ""
                        else "\nSuggestions:\n" <> Text.unlines (map ("  • " <>) suggestions)
        
        -- Error type specific message
        errorTypeMsg = case errorType of
            InvalidTagName tag -> "Invalid HTML tag: '" <> tag <> "'"
            InvalidAttributeName attr -> "Invalid HTML attribute: '" <> attr <> "'"
            UnmatchedTag expected actual -> "Tag mismatch: expected '" <> expected <> "', got '" <> actual <> "'"
            UndefinedVariable var -> "Undefined variable: '" <> var <> "'"
            MissingToHtmlInstance typ -> "Missing ToHtml instance for type: '" <> typ <> "'"
            DuplicateAttribute attr -> "Duplicate attribute: '" <> attr <> "'"
            SyntaxError msg -> "Syntax error: " <> msg
            HaskellExpressionError expr _ _ -> "Error in Haskell expression: " <> expr
    in
        errorHeader <> "\n\n" <> errorTypeMsg <> "\n\n" <> sourceContext <> suggestionText

-- | Formats source context with line numbers and error pointer
formatSourceContext :: Int -> Text -> Text -> Text -> Int -> Text
formatSourceContext lineNum beforeLine currentLine afterLine colNum =
    let
        lineNumStr = Text.pack (show lineNum)
        beforeLineNum = Text.pack (show (lineNum - 1))
        afterLineNum = Text.pack (show (lineNum + 1))
        
        -- Calculate padding for line numbers
        maxLineNumLen = maximum [Text.length beforeLineNum, Text.length lineNumStr, Text.length afterLineNum]
        padLineNum n = Text.justifyRight maxLineNumLen ' ' n
        
        -- Format lines with numbers
        beforeFormatted = if Text.null beforeLine 
                         then ""
                         else padLineNum beforeLineNum <> " | " <> beforeLine <> "\n"
        currentFormatted = padLineNum lineNumStr <> " | " <> currentLine <> "\n"
        
        -- Create error pointer
        pointer = Text.replicate (maxLineNumLen + 3 + colNum - 1) " " <> "^"
        
        afterFormatted = if Text.null afterLine
                        then ""
                        else padLineNum afterLineNum <> " | " <> afterLine <> "\n"
    in
        beforeFormatted <> currentFormatted <> pointer <> "\n" <> afterFormatted
