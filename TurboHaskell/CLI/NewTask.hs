module Main where

import TurboHaskell.Prelude
import TurboHaskell.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import TurboHaskell.ViewSupport
import qualified System.Process as Process

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Just taskName | not (Text.null taskName) -> do
            let taskPath = "Application/Tasks/" <> cs taskName <> ".hs"
            Text.writeFile taskPath (taskTemplate taskName)
            _ <- Process.system ("chmod +x " <> taskPath)
            putStrLn $ "+ " <> cs taskPath
            putStrLn $ "Run your task with: ./" <> cs taskPath
        _ -> usage

usage :: IO ()
usage = putStrLn "Usage: new-application APPLICATION_NAME"

taskTemplate :: Text -> Text
taskTemplate taskName' = cs [plain|#!/usr/bin/env run-task
module Application.Tasks.#{taskName} where

import Application.Tasks.Prelude

run :: Task
run = do
    putStrLn "Hello World!"
|]
    where taskName :: String = cs taskName'