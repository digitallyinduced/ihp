module Main where

import Test.Hspec
import IHP.Prelude

import qualified WithIHPAppSpec

main :: IO ()
main = hspec do
    WithIHPAppSpec.tests
