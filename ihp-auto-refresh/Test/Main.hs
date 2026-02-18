module Main where

import Test.Hspec
import IHP.Prelude

import qualified AutoRefreshSpec

main :: IO ()
main = hspec do
    AutoRefreshSpec.tests
