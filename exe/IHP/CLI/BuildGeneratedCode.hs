{-|
Module: IHP.CLI.BuildGeneratedCode
Description:  Provides the @build-generated-code@ command which generates the Generated.Types module
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import IHP.Prelude
import qualified IHP.SchemaCompiler as Schema
import qualified IHP.TypeDefinitions.TypeScript as TypeScript
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    Schema.compile
    TypeScript.generateDeclarations
