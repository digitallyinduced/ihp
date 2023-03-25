{-|
Module: IHP.CLI.BuildGeneratedCode
Description:  Provides the @build-generated-code@ command which generates the Generated.Types module
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import IHP.Prelude
import IHP.SchemaCompiler
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    compile