{-# LANGUAGE CPP #-}
module IHP.HSX.HaskellParser (parseHaskellExpression) where

import Prelude
import GHC.Parser.Lexer (ParseResult (..), PState (..))
import GHC.Types.SrcLoc
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Parser.PostProcess
import Text.Megaparsec.Pos
import qualified "template-haskell" Language.Haskell.TH as TH

import qualified GHC.Data.EnumSet as EnumSet
import GHC
import IHP.HSX.HsExpToTH (toExp)

import GHC.Types.Error
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Error
import qualified GHC.Types.SrcLoc as SrcLoc
#if __GLASGOW_HASKELL__ >= 908
import GHC.Unit.Module.Warnings
#endif

parseHaskellExpression :: SourcePos -> [TH.Extension] -> String -> Either (Int, Int, String) TH.Exp
parseHaskellExpression sourcePos extensions input =
        case expr of
            POk parserState result -> Right (toExp (unLoc result))
            PFailed parserState ->
                let
                    error = renderWithContext defaultSDocContext
                        $ vcat
#if __GLASGOW_HASKELL__ >= 908
                        $ map formatBulleted
#else
                        $ map (formatBulleted defaultSDocContext)
#endif
#if __GLASGOW_HASKELL__ >= 906
                        $ map (diagnosticMessage NoDiagnosticOpts)
#else
                        $ map diagnosticMessage
#endif
                        $ map errMsgDiagnostic
                        $ sortMsgBag Nothing
                        $ getMessages parserState.errors
                    line = SrcLoc.srcLocLine parserState.loc.psRealLoc
                    col = SrcLoc.srcLocCol parserState.loc.psRealLoc
                    -- Improve error messages for common cases
                    improvedError = case error of
                        err | "Variable not in scope" `isInfixOf` err ->
                            let varName = extractVarName err
                                suggestion = if not (null varName)
                                    then "\n\nSuggestion: Make sure the variable '" ++ varName ++ "' is defined in the current scope."
                                    else ""
                            in err ++ suggestion
                        err | "No instance for" `isInfixOf` err && "ToHtml" `isInfixOf` err ->
                            let typeName = extractTypeName err
                                suggestion = if not (null typeName)
                                    then "\n\nSuggestion: Add a ToHtml instance for the type '" ++ typeName ++ "' or convert it to a string first."
                                    else ""
                            in err ++ suggestion
                        err -> err
                in
                    Left (line, col, improvedError)
    where
        expr :: ParseResult (LocatedA (HsExpr GhcPs))
        expr = case Lexer.unP Parser.parseExpression parseState of
                POk parserState result -> Lexer.unP (runPV (unECP result)) parserState
                PFailed parserState -> PFailed parserState

        location :: RealSrcLoc
        location = mkRealSrcLoc filename line col
        
        filename :: FastString
        filename = mkFastString sourcePos.sourceName

        line :: Int
        line = unPos sourcePos.sourceLine

        col :: Int
        col = unPos sourcePos.sourceColumn

        buffer = stringToStringBuffer input
        parseState = Lexer.initParserState parserOpts buffer location

        parserOpts :: Lexer.ParserOpts
        parserOpts = Lexer.mkParserOpts (EnumSet.fromList extensions) diagOpts [] False False False False

diagOpts :: DiagOpts
diagOpts =
    DiagOpts
    { diag_warning_flags = EnumSet.empty
    , diag_fatal_warning_flags = EnumSet.empty
    , diag_warn_is_error = False
    , diag_reverse_errors = False
    , diag_max_errors = Nothing
    , diag_ppr_ctx = defaultSDocContext
#if __GLASGOW_HASKELL__ >= 908
    , diag_custom_warning_categories = emptyWarningCategorySet
    , diag_fatal_custom_warning_categories = emptyWarningCategorySet
#endif
    }

-- Helper functions to extract information from error messages
extractVarName :: String -> String
extractVarName err = case words err of
    (_:_:"`":name:"`":_) -> name
    (_:_:name:_) -> name
    _ -> ""

extractTypeName :: String -> String
extractTypeName err = case words err of
    (_:_:"`":name:"`":_) -> name
    (_:_:name:_) -> name
    _ -> ""
