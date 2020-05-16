module Main where

import IHP.Prelude
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ViewSupport
import qualified System.Process as Process

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Just taskName | not (Text.null taskName) -> do
            let taskPath = "Application/Scripts/" <> cs taskName <> ".hs"
            Text.writeFile taskPath (taskTemplate taskName)
            _ <- Process.system ("chmod +x " <> taskPath)
            putStrLn $ "+ " <> cs taskPath
            putStrLn $ "Run your script with: ./" <> cs taskPath
        _ -> usage

usage :: IO ()
usage = putStrLn "Usage: new-application APPLICATION_NAME"

taskTemplate :: Text -> Text
taskTemplate taskName' = cs [plain|#!/usr/bin/env run-script
module Application.Scripts.#{taskName} where

import Application.Scripts.Prelude

run :: Script
run = do
    putStrLn "Hello World!"
|]
    where taskName :: String = cs taskName'