module Main where

import Prelude
import Test.Hspec

import qualified GraphQL.ParserSpec
import qualified GraphQL.CompilerSpec
import qualified GraphQL.SchemaCompilerSpec

main :: IO ()
main = hspec do
    GraphQL.ParserSpec.tests
    GraphQL.CompilerSpec.tests
    GraphQL.SchemaCompilerSpec.tests