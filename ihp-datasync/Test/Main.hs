module Main where

import Prelude
import Test.Hspec
import qualified Test.DataSync.DynamicQueryCompiler
import qualified Test.DataSync.TypedEncoder
import qualified Test.DataSync.ChangeNotifications

main :: IO ()
main = hspec do
    Test.DataSync.DynamicQueryCompiler.tests
    Test.DataSync.TypedEncoder.tests
    Test.DataSync.ChangeNotifications.tests
