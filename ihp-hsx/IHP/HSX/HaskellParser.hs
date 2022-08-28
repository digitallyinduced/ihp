module IHP.HSX.HaskellParser (parseHaskellExpression) where

import Prelude
import GHC.Parser.Lexer (ParseResult (..), PState (..))
import qualified GHC.Parser.Errors.Ppr as ParserErrorPpr
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

parseHaskellExpression :: SourcePos -> [TH.Extension] -> String -> Either (Int, Int, String) TH.Exp
parseHaskellExpression sourcePos extensions input =
        case expr of
            POk parserState result -> Right (toExp (unLoc result))
            PFailed parserState ->
                let
                    error = concatMap (show . ParserErrorPpr.pprError) (parserState.errors)
                    realLoc = (psRealLoc parserState.loc)
                    line = srcLocLine realLoc
                    col = srcLocCol realLoc
                in
                    Left (line, col, error)
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
        parserOpts = Lexer.mkParserOpts EnumSet.empty (EnumSet.fromList extensions) False False False False
