module Main where

import Prelude
import Main.Utf8 (withUtf8)
import qualified SchemaCompiler.CacheSpec
import Test.Hspec (hspec)

main :: IO ()
main = withUtf8 (hspec SchemaCompiler.CacheSpec.tests)
