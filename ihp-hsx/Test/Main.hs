module Main where

import Prelude

import Test.Hspec
import qualified IHP.HSX.QQSpec
import qualified IHP.HSX.ParserSpec

main :: IO ()
main = hspec do
    IHP.HSX.QQSpec.tests
    IHP.HSX.ParserSpec.tests
