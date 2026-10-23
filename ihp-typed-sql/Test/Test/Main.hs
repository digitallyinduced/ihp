module Main where

import Test.Hspec
import Prelude

import qualified Test.TypedSqlSpec
import qualified Test.HasqlAdapterSpec

main :: IO ()
main = hspec do
    Test.TypedSqlSpec.tests
    Test.HasqlAdapterSpec.tests
