module Main where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified System.Posix.Env.ByteString as Posix
import IHP.IDE.CodeGen.ControllerGenerator
import IHP.IDE.CodeGen.Controller (undoPlan)
import qualified Data.Text as Text
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    ensureIsInAppDirectory

    args <- map cs <$> Posix.getArgs
    case headMay args of
        Just "" -> usage
        Just appAndControllerName -> case Text.splitOn "." appAndControllerName of
            [controllerName] -> deleteController "Web" controllerName
            [applicationName, controllerName] -> deleteController applicationName controllerName
            _ -> usage
        _ -> usage

deleteController applicationName controllerName = do
    let paginationEnabled = False
    planOrError <- buildPlan applicationName controllerName paginationEnabled
    case planOrError of
        Left error -> putStrLn error
        Right plan -> undoPlan $ reverse plan

usage :: IO ()
usage = putStrLn "Usage: delete-controller RESOURCE_NAME"

ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")