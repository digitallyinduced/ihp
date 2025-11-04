module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.ServerSideComponent.HtmlParserSpec
import qualified Test.ServerSideComponent.HtmlDiffSpec

main :: IO ()
main = hspec do
    Test.ServerSideComponent.HtmlParserSpec.tests
    Test.ServerSideComponent.HtmlDiffSpec.tests
