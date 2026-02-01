module Main where

import Test.Hspec
import qualified Test.PGListenerSpec

main :: IO ()
main = hspec do
    Test.PGListenerSpec.tests
