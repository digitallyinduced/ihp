module Main where

import Prelude
import Test.Hspec

import qualified Test.GraphQL.ParserSpec
import qualified Test.GraphQL.CompilerSpec
import qualified Test.GraphQL.SchemaCompilerSpec

main :: IO ()
main = hspec do
    Test.GraphQL.ParserSpec.tests
    Test.GraphQL.CompilerSpec.tests
    Test.GraphQL.SchemaCompilerSpec.tests