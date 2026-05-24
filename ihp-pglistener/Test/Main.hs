module Main where

import Test.Hspec
import qualified PGListenerSpec

main :: IO ()
main = hspec do
    PGListenerSpec.tests
