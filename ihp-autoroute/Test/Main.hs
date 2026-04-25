module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.AutoRoute.RouterSupportSpec
import qualified Test.AutoRoute.MixedModeSpec

main :: IO ()
main = hspec do
    Test.AutoRoute.RouterSupportSpec.tests
    Test.AutoRoute.MixedModeSpec.tests
