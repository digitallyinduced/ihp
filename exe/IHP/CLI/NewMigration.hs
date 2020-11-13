module Main where

import IHP.Prelude
import IHP.SchemaMigration
import qualified System.Posix.Env.ByteString as Posix
import qualified System.Directory as Directory
import Control.Monad.Fail
import IHP.IDE.ToolServer.Helper.Controller (openEditor)

main :: IO ()
main = do
    ensureIsInAppDirectory

    let doCreateMigration description = do
            migration <- createMigration description
            let path = migrationPath migration
            putStrLn $ "Created migration: " <> path
            openEditor path 0 0
    
    args <- Posix.getArgs
    case headMay args of
        Just "--help" -> usage
        Just description -> doCreateMigration (cs description)
        Nothing -> doCreateMigration ""

usage :: IO ()
usage = putStrLn "Usage: new-migration [DESCRIPTION]"

ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")