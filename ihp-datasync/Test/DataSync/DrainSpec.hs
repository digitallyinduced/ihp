{-# LANGUAGE BlockArguments #-}
module DataSync.DrainSpec (tests) where

import Prelude
import IHP.DataSync.Drain
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (when)
import Data.IORef
import Data.Maybe (isNothing)
import Test.Hspec
import System.Timeout (timeout)

tests :: Spec
tests = describe "DataSync graceful drain" do
    it "releases idle subscription waits even when setup finishes after drain" do
        signal <- newDrainSignal
        close <- newEmptyMVar
        requestDrain signal
        bounded (waitForSubscriptionClose signal close)

    it "retains ordinary subscription close semantics" do
        signal <- newDrainSignal
        close <- newEmptyMVar
        putMVar close ()
        bounded (waitForSubscriptionClose signal close)

    it "joins dispatched callbacks and rejects callbacks after scope ends" do
        saved <- newEmptyMVar
        started <- newEmptyMVar
        release <- newEmptyMVar
        endScope <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        let callback () = do
                modifyIORef' calls (+ 1)
                putMVar started ()
                takeMVar release
        withAsync (withJoinedCallback callback \wrapped ->
            putMVar saved wrapped >> takeMVar endScope) \scope -> do
            wrapped <- bounded (takeMVar saved)
            withAsync (wrapped ()) \running -> do
                bounded (takeMVar started)
                putMVar endScope ()
                poll scope >>= (`shouldSatisfy` isNothing)
                putMVar release ()
                bounded (wait running)
                bounded (wait scope)
            wrapped ()
        readIORef calls `shouldReturn` 1

    it "does not receive or handle after admission is closed" do
        signal <- newDrainSignal
        requestDrain signal
        runDrainingWorkers signal 1 (fail "received") (\() -> fail "handled")

    it "rechecks admission when receive and drain complete together" do
        signal <- newDrainSignal
        runDrainingWorkers signal 1 (requestDrain signal >> pure ())
            (\() -> expectationFailure "accepted after drain")

    it "joins active and semaphore-queued handlers without cancelling" do
        signal <- newDrainSignal
        received <- newIORef (0 :: Int)
        queued <- newEmptyMVar
        firstStarted <- newEmptyMVar
        release <- newEmptyMVar
        completed <- newIORef ([] :: [Int])
        never <- newEmptyMVar
        let receive = do
                n <- atomicModifyIORef' received (\x -> (x + 1, x))
                if n < 2 then pure n else putMVar queued () >> takeMVar never
            handle n = do
                when (n == 0) (putMVar firstStarted () >> takeMVar release)
                atomicModifyIORef' completed (\xs -> (n : xs, ()))
        withAsync (runDrainingWorkers signal 1 receive handle) \running -> do
            bounded (takeMVar firstStarted)
            bounded (takeMVar queued)
            requestDrain signal
            poll running >>= (`shouldSatisfy` isNothing)
            putMVar release ()
            bounded (wait running)
        readIORef completed `shouldReturn` [1, 0]

    it "cancels and joins handlers on receive failure" do
        signal <- newDrainSignal
        started <- newEmptyMVar
        finished <- newEmptyMVar
        never <- newEmptyMVar
        received <- newIORef False
        let receive = do
                seen <- atomicModifyIORef' received (\old -> (True, old))
                if seen then takeMVar started >> fail "disconnected" else pure ()
            handle () = (putMVar started () >> takeMVar never)
                `finally` putMVar finished ()
        runDrainingWorkers signal 1 receive handle `shouldThrow` anyIOException
        bounded (takeMVar finished)

    it "does not turn cancellation during drain into successful completion" do
        signal <- newDrainSignal
        started <- newEmptyMVar
        finished <- newEmptyMVar
        never <- newEmptyMVar
        input <- newMVar ()
        withAsync (runDrainingWorkers signal 1 (takeMVar input)
            (\() -> (putMVar started () >> takeMVar never) `finally` putMVar finished ())) \running -> do
                bounded (takeMVar started)
                requestDrain signal
                cancel running
                bounded (takeMVar finished)
                outcome <- waitCatch running
                case outcome of
                    Left _ -> pure ()
                    Right () -> expectationFailure "cancelled drain reported completion"

    it "joins fast handlers without losing registration" do
        signal <- newDrainSignal
        count <- newIORef (0 :: Int)
        next <- newIORef (0 :: Int)
        never <- newEmptyMVar
        let receive = do
                n <- atomicModifyIORef' next (\x -> (x + 1, x))
                if n < 1000 then pure () else requestDrain signal >> takeMVar never
        bounded $ runDrainingWorkers signal 4 receive \() ->
            atomicModifyIORef' count (\n -> (n + 1, ()))
        readIORef count `shouldReturn` 1000

    it "propagates an uncaught handler failure after joining on drain" do
        signal <- newDrainSignal
        input <- newMVar ()
        runDrainingWorkers signal 1 (takeMVar input)
            (\() -> requestDrain signal >> fail "handler failed")
            `shouldThrow` anyIOException

bounded :: IO a -> IO a
bounded action = timeout 3000000 action >>= maybe (fail "test timed out") pure
