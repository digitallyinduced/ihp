module Main where

import ClassyPrelude
import Data.String.Conversions (cs)
import qualified System.Directory as Directory
import qualified System.Posix.Env.ByteString as Posix
import Control.Monad.Fail
import IHP.IDE.CodeGen.ControllerGenerator
import IHP.IDE.CodeGen.Controller (executePlan)
import Data.Text.Encoding as Text

main :: IO ()
main = do
    ensureIsInAppDirectory

    args <- map Text.decodeUtf8 <$> Posix.getArgs
    case args of
        [] -> usage
        [applicationName, controllerName] -> do
            planOrError <- buildPlan controllerName applicationName
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        [controllerName] -> do
            planOrError <- buildPlan controllerName "Web"
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        _ -> usage


usage :: IO ()
usage = putStrLn "Usage: new-controller appName controllerName \n new-controller controllerName"


ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")