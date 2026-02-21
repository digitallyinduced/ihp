module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.TypedSqlSpec

main :: IO ()
main = hspec do
    Test.TypedSqlSpec.tests
