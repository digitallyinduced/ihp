module Main where

import Prelude
import Test.Hspec
import qualified DataSync.DynamicQueryCompiler
import qualified DataSync.TypedEncoder
import qualified DataSync.ChangeNotifications
import qualified DataSync.RLSIntegrationSpec
import qualified DataSync.DataSyncIntegrationSpec

main :: IO ()
main = hspec do
    DataSync.DynamicQueryCompiler.tests
    DataSync.TypedEncoder.tests
    DataSync.ChangeNotifications.tests
    DataSync.RLSIntegrationSpec.tests
    DataSync.DataSyncIntegrationSpec.tests
