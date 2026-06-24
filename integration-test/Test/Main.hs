module Test.Main where

import Prelude
import Test.Hspec
import qualified Test.IntegrationSpec
import qualified Test.TypedSqlSpec

main :: IO ()
main = hspec do
    Test.IntegrationSpec.tests
    Test.TypedSqlSpec.tests
