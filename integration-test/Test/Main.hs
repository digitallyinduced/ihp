module Test.Main where

import Prelude
import Test.Hspec
import qualified Test.IntegrationSpec

main :: IO ()
main = hspec do
    Test.IntegrationSpec.tests
