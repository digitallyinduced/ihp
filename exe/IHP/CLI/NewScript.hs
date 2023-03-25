module Main where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.IDE.CodeGen.ScriptGenerator
import IHP.IDE.CodeGen.Controller (executePlan)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    args <- getArgs
    case headMay args of
        Just scriptName | not (Text.null scriptName) -> do
            let planOrError = buildPlan scriptName
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        _ -> usage

usage :: IO ()
usage = putStrLn "Usage: new-script APPLICATION_NAME"
