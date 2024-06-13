module Main where

import IHP.Prelude
import IHP.SchemaCompiler
import Main.Utf8 (withUtf8)
import qualified Data.Text.IO as Text
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified IHP.DataSync.TypeScript.Compiler as Compiler

main :: IO ()
main = withUtf8 do
    (sqlPath, tsPath) <- decodeArgs
    errorOrSchema <- Parser.parseSqlFile sqlPath

    case errorOrSchema of
        Left error -> fail (cs error)
        Right schema -> do
            let types = Compiler.generateTypeScriptTypeDefinitions schema

            Text.writeFile tsPath types

decodeArgs :: IO (String, String)
decodeArgs = do
    args <- getArgs

    case args of
        [sqlPath, tsPath] -> pure (cs sqlPath, cs tsPath)
        otherwise -> fail "Invalid usage: generate-datasync-types Schema.sql Schema.ts"