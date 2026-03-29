module Main where

import IHP.Prelude
import qualified System.Directory.OsPath as Directory
import qualified System.Posix.Env.ByteString as Posix
import IHP.IDE.CodeGen.ControllerGenerator
import IHP.IDE.CodeGen.Controller (executePlan)
import qualified Data.Text as Text
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    ensureIsInAppDirectory

    args <- map cs <$> Posix.getArgs
    case headMay args of
        Just "" -> usage
        Just appAndControllerName -> do
            generateController appAndControllerName
        Nothing -> usage

usage :: IO ()
usage = putStrLn "Usage: new-controller RESOURCE_NAME"


ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")
    
generateController :: Text -> IO ()
generateController appAndControllerName = do
    case Text.splitOn "." appAndControllerName of
        [controllerName] -> do
            planOrError <- buildPlan controllerName defaultControllerConfig
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        [applicationName, controllerName] -> do
            planOrError <- buildPlan controllerName defaultControllerConfig { applicationName }
            case planOrError of
                Left error -> putStrLn error
                Right plan -> executePlan plan
        _ -> usage

    