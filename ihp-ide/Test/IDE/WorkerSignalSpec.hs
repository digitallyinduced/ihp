{-|
Module: Test.IDE.WorkerSignalSpec
-}
module Test.IDE.WorkerSignalSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.WorkerSignal as WorkerSignal
import qualified Control.Concurrent.Async as Async
import qualified System.IO.Temp as Temp
import qualified System.FilePath as FilePath
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (threadDelay)
import System.Timeout (timeout)
import Data.IORef (newIORef, readIORef, modifyIORef')

tests :: Spec
tests = do
    describe "IHP.IDE.WorkerSignal" do
        it "delivers a single reload signal end-to-end over a Unix socket" do
            withSocketPath \socketPath -> do
                received <- newEmptyMVar
                Async.withAsync (server socketPath received) \_ -> do
                    -- Give the server a beat to bind before connecting.
                    threadDelay 50000
                    WorkerSignal.sendReload socketPath
                    -- The server should observe a reload within a reasonable
                    -- timeout. Anything > 1s indicates a deadlock / wiring bug.
                    result <- timeout (2 * 1000000) (takeMVar received)
                    result `shouldBe` Just ()

        it "collapses many rapid signals into at-least-one delivery (debounce)" do
            -- Rapid back-to-back reloads should never block sendReload, and the
            -- server should observe at least one. We don't assert exactly one
            -- because the debounce is best-effort: if one signal is fully
            -- consumed before the next arrives, the second wakes a new wait.
            withSocketPath \socketPath -> do
                signalCount <- newIORef (0 :: Int)
                Async.withAsync (countingServer socketPath signalCount) \_ -> do
                    threadDelay 50000
                    -- Fire 10 sends back to back. Each connects, writes, closes.
                    forM_ [1 .. 10 :: Int] \_ -> WorkerSignal.sendReload socketPath
                    -- Give the server some time to drain.
                    threadDelay 200000
                    n <- readIORef signalCount
                    n `shouldSatisfy` (>= 1)

        it "retries when the server isn't listening yet" do
            withSocketPath \socketPath -> do
                received <- newEmptyMVar
                -- Start the sender BEFORE the server. sendReload should retry
                -- until the server is up. (sendReload retries internally.)
                sender <- Async.async (WorkerSignal.sendReload socketPath)
                threadDelay 100000 -- ensure sender has tried and failed at least once
                Async.withAsync (server socketPath received) \_ -> do
                    Async.wait sender
                    result <- timeout (2 * 1000000) (takeMVar received)
                    result `shouldBe` Just ()

-- | Run a one-shot signal server: blocks until one reload arrives, then writes
-- to @received@ and exits.
server :: FilePath -> MVar () -> IO ()
server path received =
    WorkerSignal.withWorkerSignalServer path \waitForReload -> do
        waitForReload
        putMVar received ()

-- | Run a counting signal server: increments @counter@ on every reload,
-- forever (until cancelled).
countingServer :: FilePath -> IORef Int -> IO ()
countingServer path counter =
    WorkerSignal.withWorkerSignalServer path \waitForReload -> do
        forever do
            waitForReload
            modifyIORef' counter (+ 1)

withSocketPath :: (FilePath -> IO a) -> IO a
withSocketPath action =
    -- macOS caps SOCK_UNIX paths at 104 bytes; the system temp dir already eats
    -- ~80, so we use a short fixed name under /tmp instead.
    Temp.withTempDirectory "/tmp" "ihp-ws" \dir ->
        action (dir FilePath.</> "s.sock")
