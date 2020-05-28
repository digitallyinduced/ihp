module Main where

import ClassyPrelude
import Data.String.Conversions (cs)
import qualified System.Directory as Directory
import qualified System.Posix.Env.ByteString as Posix
import Control.Monad.Fail
import IHP.IDE.CodeGen.ControllerGenerator
import IHP.IDE.CodeGen.Controller (undoPlan)
import qualified Data.Text as Text

main :: IO ()
main = do
    ensureIsInAppDirectory

    args <- map cs <$> Posix.getArgs
    case headMay args of
        Just "" -> usage
        Just appAndControllerName -> do
            planOrError <- buildPlan appAndControllerName
            case planOrError of
                Left error -> putStrLn error
                Right plan -> undoPlan $ reverse plan
        _ -> usage


usage :: IO ()
usage = putStrLn "Usage: delete-controller RESOURCE_NAME"


ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")

ensureControllerExists :: Text -> IO ()
ensureControllerExists appAndControllerName = do
     case Text.splitOn "." appAndControllerName of
        [applicationName, controllerName'] -> do
            controllerFileExists <- Directory.doesFileExist $ cs (applicationName <> "/Controller/" <> controllerName' <> ".hs")
            unless controllerFileExists (fail "Controller does not exist.")
        [controllerName'] -> do
            controllerFileExists <- Directory.doesFileExist $ cs ("Web/Controller/" <> controllerName' <> ".hs")
            unless controllerFileExists (fail "Controller does not exist.")