{-|
Module: IDE.PortConfigSpec
-}
module IDE.PortConfigSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.PortConfig
import Control.Exception (bracket_, try)
import qualified System.Environment as Env

tests :: Spec
tests = do
    describe "IHP.IDE.PortConfig.portConfigFromEnvironment" do
        it "binds the PORT env var as the app port and PORT + 1 as the tool server port" do
            portConfig <- withPortEnv "9000" portConfigFromEnvironment
            portConfig.appPort `shouldBe` 9000
            portConfig.toolServerPort `shouldBe` 9001

        it "falls back to scanning from the default port when PORT is unset" do
            -- Scanning for a free port connects to candidate ports, which some
            -- sandboxes forbid (notably the Nix build sandbox on macOS), where it
            -- fails with "Network.Socket.connect: permission denied". Skip there
            -- rather than fail; any other exception still propagates.
            result <- try (withoutPortEnv portConfigFromEnvironment)
            case result of
                Right portConfig -> do
                    portConfig.appPort `shouldSatisfy` (>= defaultAppPort)
                    portConfig.toolServerPort `shouldBe` (portConfig.appPort + 1)
                Left exception
                    | isPermissionError exception ->
                        pendingWith "Scanning for a free port is not permitted in this sandbox"
                    | otherwise -> throwIO exception

-- | Runs the action with the @PORT@ env var set, restoring the previous state afterwards.
withPortEnv :: String -> IO a -> IO a
withPortEnv value action = bracket_ (Env.setEnv "PORT" value) (Env.unsetEnv "PORT") action

-- | Runs the action with the @PORT@ env var guaranteed unset.
withoutPortEnv :: IO a -> IO a
withoutPortEnv action = Env.unsetEnv "PORT" >> action
