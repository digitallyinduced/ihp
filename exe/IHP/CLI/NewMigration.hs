module Main where

import IHP.Prelude
import IHP.SchemaMigration
import qualified System.Posix.Env.ByteString as Posix
import qualified System.Directory as Directory
import IHP.IDE.ToolServer.Helper.Controller (openEditor)
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import IHP.IDE.CodeGen.Controller (executePlan)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    ensureIsInAppDirectory

    let doCreateMigration description = do
            (_, plan) <- MigrationGenerator.buildPlan description
            executePlan plan
            let path = MigrationGenerator.migrationPathFromPlan plan
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