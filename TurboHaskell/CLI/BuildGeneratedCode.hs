{-|
Module: TurboHaskell.CLI.BuildGeneratedCode
Description:  Provides the @build-generated-code@ command which generates the Generated.Types module
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import TurboHaskell.Prelude
import TurboHaskell.SchemaCompiler

main :: IO ()
main = compile