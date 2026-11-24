{-|
Module: IDE.TelemetrySpec
-}
module IDE.TelemetrySpec where

import Test.Hspec
import IHP.Prelude
import IHP.Telemetry
import Control.Exception (bracket_)
import qualified Data.Text as Text
import qualified System.Environment as Env
import System.Log.FastLogger (FastLogger, fromLogStr)

data TestContext = TestContext { logger :: FastLogger }

tests :: Spec
tests = do
    describe "IHP.Telemetry" do
        it "returns normally and logs a single line when the request fails" do
            -- Nothing listens on port 1, so the request fails without network access
            logLines <- collectLogLines (reportTelemetryTo "http://127.0.0.1:1/CreateEvent")
            filter ("Telemetry failed:" `Text.isPrefixOf`) logLines `shouldSatisfy` ((== 1) . length)
            filter ("\n" `Text.isInfixOf`) logLines `shouldBe` []

        it "does nothing when IHP_TELEMETRY_DISABLED=1" do
            logLines <- bracket_ (Env.setEnv "IHP_TELEMETRY_DISABLED" "1") (Env.unsetEnv "IHP_TELEMETRY_DISABLED") do
                collectLogLines reportTelemetry
            logLines `shouldBe` []

-- | Runs the action with a logger that records every log line
collectLogLines :: ((?context :: TestContext) => IO ()) -> IO [Text]
collectLogLines action = do
    logLinesRef <- newIORef []
    let ?context = TestContext { logger = \logStr -> modifyIORef' logLinesRef (cs (fromLogStr logStr) :) }
    action
    reverse <$> readIORef logLinesRef
