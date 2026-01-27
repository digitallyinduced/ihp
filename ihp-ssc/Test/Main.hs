module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.ServerSideComponent.HtmlParserSpec
import qualified Test.ServerSideComponent.HtmlDiffSpec
import qualified Test.ServerSideComponent.TypesSpec

main :: IO ()
main = hspec do
    Test.ServerSideComponent.HtmlParserSpec.tests
    Test.ServerSideComponent.HtmlDiffSpec.tests
    Test.ServerSideComponent.TypesSpec.tests
