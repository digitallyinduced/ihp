module Main where

import ClassyPrelude
import Data.String.Conversions (cs)
import qualified System.Directory as Directory
import qualified System.Posix.Env.ByteString as Posix
import Control.Monad.Fail
import IHP.IDE.CodeGen.ApplicationGenerator
import IHP.IDE.CodeGen.Controller (executePlan)

main :: IO ()
main = do
    ensureIsInAppDirectory

    args <- map cs <$> Posix.getArgs
    case headMay args of
        Just "" -> usage
        Just applicationName -> do
            planOrError <- buildPlan applicationName
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        _ -> usage


usage :: IO ()
usage = putStrLn "Usage: new-application RESOURCE_NAME"


ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")