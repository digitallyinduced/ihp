module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.AutoRefreshSpec

main :: IO ()
main = hspec do
    Test.AutoRefreshSpec.tests
