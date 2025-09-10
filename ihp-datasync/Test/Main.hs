module Main where

import Prelude
import Test.Hspec
import qualified Test.DataSync.DynamicQueryCompiler

main :: IO ()
main = hspec do
    Test.DataSync.DynamicQueryCompiler.tests
