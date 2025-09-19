{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import IHP.Prelude
import Test.Hspec
import qualified Test.AutoRefreshSpec as AutoRefreshSpec

main :: IO ()
main = hspec AutoRefreshSpec.tests
