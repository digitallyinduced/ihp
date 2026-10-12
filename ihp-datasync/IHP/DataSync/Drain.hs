{-# LANGUAGE BlockArguments #-}
-- | Cooperative message admission and shutdown. A signal may be shared by
-- several connections. It is one-way: reopening requires a new signal.
module IHP.DataSync.Drain
    ( DrainSignal, newDrainSignal, requestDrain, runDrainingWorkers
    , waitForSubscriptionClose, withJoinedCallback
    ) where

import Prelude
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.QSem
import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (bracket_, mask, onException, throwIO, finally)
import Control.Monad (filterM, void, when)
import Data.IORef

-- | Opaque, process-local admission latch.
newtype DrainSignal = DrainSignal (TVar Bool)

-- | Create an open admission latch.
newDrainSignal :: IO DrainSignal
newDrainSignal = DrainSignal <$> newTVarIO False

-- | Close admission. This does not cancel workers or wait for connections.
requestDrain :: DrainSignal -> IO ()
requestDrain (DrainSignal closed) = atomically (writeTVar closed True)

-- | Only the idle subscription wait is cancellable; setup and callbacks are
-- not raced against drain. A subscription starting after the latch closes
-- therefore still unwinds its normal resource bracket.
waitForSubscriptionClose :: DrainSignal -> MVar () -> IO ()
waitForSubscriptionClose (DrainSignal closed) close =
    void (race (atomically (readTVar closed >>= check)) (takeMVar close))

-- | PGListener may already have dispatched a callback when unsubscribed.
-- Close callback admission and join accepted callbacks before leaving scope.
-- Late invocations of the supplied wrapper become no-ops.
withJoinedCallback :: (a -> IO ()) -> ((a -> IO ()) -> IO b) -> IO b
withJoinedCallback callback use = do
    state <- newTVarIO (False, 0 :: Int)
    let wrapped value = mask \restore -> do
            accepted <- atomically do
                (closed, count) <- readTVar state
                if closed then pure False else
                    writeTVar state (False, count + 1) >> pure True
            when accepted $
                restore (callback value) `finally`
                    atomically (modifyTVar' state (\(closed, count) -> (closed, count - 1)))
        close = do
            atomically (modifyTVar' state (\(_, count) -> (True, count)))
            atomically (readTVar state >>= \(_, count) -> check (count == 0))
    use wrapped `finally` close

-- | Run a receiving loop with bounded active handlers. On requested drain,
-- join every accepted handler, including those queued for a concurrency slot.
-- On receive failure or parent cancellation, cancel and join the handlers.
--
-- The receive action must be safe to cancel (normally a socket read); do not
-- perform business effects there. Handler completion does not cover detached
-- work. Returning proves joined handlers, not success of effects whose errors
-- the handler caught. Uncaught handler failures are propagated.
runDrainingWorkers :: DrainSignal -> Int -> IO a -> (a -> IO ()) -> IO ()
runDrainingWorkers (DrainSignal closed) concurrency receive handle = do
    when (concurrency < 1) (fail "DataSync: concurrency must be positive")
    sem <- newQSem concurrency
    workers <- newIORef []
    let reap worker = poll worker >>= \result -> case result of
            Nothing -> pure True
            Just (Right ()) -> pure False
            Just (Left failure) -> throwIO failure
        join = do
            outcomes <- readIORef workers >>= mapM waitCatch
            mapM_ (either throwIO pure) outcomes
        cleanup = readIORef workers >>= mapConcurrently_ cancel
        loop = mask \restore -> do
            open <- atomically (not <$> readTVar closed)
            if not open then restore join else do
                next <- restore $ race
                    (atomically (readTVar closed >>= check))
                    receive
                case next of
                    Left () -> restore join
                    Right message -> do
                        -- This STM read is the acceptance linearization point.
                        -- Masking spans acceptance, spawn and registration.
                        accepted <- atomically (not <$> readTVar closed)
                        if not accepted then restore join else do
                            remaining <- readIORef workers >>= filterM reap
                            writeIORef workers remaining
                            worker <- asyncWithUnmask \unmask ->
                                bracket_ (waitQSem sem) (signalQSem sem)
                                    (unmask (handle message))
                            -- Only the parent removes handles, after poll has
                            -- observed completion. Fast children cannot race
                            -- registration or disappear before they are joined.
                            modifyIORef' workers (worker :)
                            restore loop
    void (loop `onException` cleanup)
