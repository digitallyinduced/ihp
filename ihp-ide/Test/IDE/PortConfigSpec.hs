{-|
Module: IDE.PortConfigSpec
-}
module IDE.PortConfigSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.PortConfig
import Control.Exception (bracket_)
import qualified System.Environment as Env

tests :: Spec
tests = do
    describe "IHP.IDE.PortConfig.portConfigFromEnvironment" do
        it "binds the PORT env var as the app port and PORT + 1 as the tool server port" do
            portConfig <- withPortEnv "9000" portConfigFromEnvironment
            portConfig.appPort `shouldBe` 9000
            portConfig.toolServerPort `shouldBe` 9001

        it "falls back to scanning from the default port when PORT is unset" do
            portConfig <- withoutPortEnv portConfigFromEnvironment
            portConfig.appPort `shouldSatisfy` (>= defaultAppPort)
            portConfig.toolServerPort `shouldBe` (portConfig.appPort + 1)

-- | Runs the action with the @PORT@ env var set, restoring the previous state afterwards.
withPortEnv :: String -> IO a -> IO a
withPortEnv value action = bracket_ (Env.setEnv "PORT" value) (Env.unsetEnv "PORT") action

-- | Runs the action with the @PORT@ env var guaranteed unset.
withoutPortEnv :: IO a -> IO a
withoutPortEnv action = Env.unsetEnv "PORT" >> action
