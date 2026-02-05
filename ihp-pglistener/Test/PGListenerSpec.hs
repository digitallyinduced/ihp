{-|
Module: Test.PGListenerSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.PGListenerSpec where

import Prelude
import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.String.Conversions (cs)
import Data.Function ((&))
import Data.HashMap.Strict as HashMap
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import System.Environment (lookupEnv)

import IHP.Log.Types (Logger(..), LogLevel(..))
import qualified IHP.PGListener as PGListener

import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Session as Session

logger :: Logger
logger = Logger
    { write = \_ -> pure ()
    , level = Debug
    , formatter = \_ _ msg -> msg
    , timeCache = pure ""
    , cleanup = pure ()
    }

getDatabaseUrl :: IO ByteString
getDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    pure (maybe "postgresql:///postgres" cs envUrl)

acquireConnection :: ByteString -> IO Hasql.Connection
acquireConnection databaseUrl = do
    let settings = [HasqlSetting.connection (HasqlConnection.string (cs databaseUrl))]
    result <- Hasql.acquire settings
    case result of
        Right connection -> pure connection
        Left err -> error ("Test: Failed to connect to database: " <> show err)

canConnectToPostgres :: IO Bool
canConnectToPostgres = do
    connStr <- getDatabaseUrl
    result <- Exception.try (Exception.bracket (acquireConnection connStr) Hasql.release (\_ -> pure ()))
    case result of
        Left (_ :: Exception.SomeException) -> pure False
        Right _ -> pure True

-- | Run a test that requires a real PostgreSQL connection, skipping if unavailable
withDB :: (ByteString -> IO ()) -> IO ()
withDB action = do
    available <- canConnectToPostgres
    if available
        then getDatabaseUrl >>= action
        else pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"

-- | Execute a raw SQL statement via a temporary hasql connection
execSQL :: ByteString -> ByteString -> IO ()
execSQL connStr sql = Exception.bracket (acquireConnection connStr) Hasql.release \conn -> do
    result <- Session.run (Session.sql sql) conn
    case result of
        Right () -> pure ()
        Left err -> error ("SQL exec failed: " <> show err)

tests :: Spec
tests = do
    describe "IHP.PGListener" do
        describe "subscribe" do
            it "should add a subscriber" do
                PGListener.withPGListener "" logger \pgListener -> do
                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions
                    subscriptionsCount `shouldBe` 0

                    let didInsertRecordCallback notification = pure ()

                    pgListener & PGListener.subscribe "did_insert_record" didInsertRecordCallback

                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions
                    subscriptionsCount `shouldBe` 1

        describe "unsubscribe" do
            it "remove the subscription" do
                PGListener.withPGListener "" logger \pgListener -> do
                    subscription <- pgListener & PGListener.subscribe "did_insert_record" (const (pure ()))
                    pgListener & PGListener.unsubscribe subscription

                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions

                    subscriptionsCount `shouldBe` 0

        describe "multi-channel notifications" do
            it "should receive notifications on multiple channels" do
                withDB \connStr -> do
                    PGListener.withPGListener connStr logger \pgListener -> do
                        received1 <- newIORef ([] :: [ByteString])
                        received2 <- newIORef ([] :: [ByteString])

                        _sub1 <- pgListener & PGListener.subscribe "test_channel_1" \n ->
                            modifyIORef' received1 (n.notificationData :)
                        _sub2 <- pgListener & PGListener.subscribe "test_channel_2" \n ->
                            modifyIORef' received2 (n.notificationData :)

                        -- Allow time for LISTEN commands to be processed
                        threadDelay 200_000

                        -- Send notifications via a separate hasql connection
                        execSQL connStr "NOTIFY test_channel_1, 'hello1'"
                        execSQL connStr "NOTIFY test_channel_2, 'hello2'"

                        -- Allow time for notifications to be delivered
                        threadDelay 200_000

                        r1 <- readIORef received1
                        r2 <- readIORef received2
                        r1 `shouldBe` ["hello1"]
                        r2 `shouldBe` ["hello2"]

            it "should not drop notifications when a new channel is subscribed" do
                withDB \connStr -> do
                    PGListener.withPGListener connStr logger \pgListener -> do
                        received <- newIORef ([] :: [ByteString])

                        -- Subscribe to channel_1 and wait for the LISTEN to be processed
                        _sub1 <- pgListener & PGListener.subscribe "nodrop_1" \n ->
                            modifyIORef' received (n.notificationData :)
                        threadDelay 200_000

                        -- One thread continuously sends notifications on channel_1.
                        -- Concurrently, the main thread subscribes to new channels,
                        -- which triggers the internal race cancellation of
                        -- waitForNotifications. If the cancellation drops a notification
                        -- that was mid-delivery, the final count will be wrong.
                        let totalNotifications = 100 :: Int
                        Exception.bracket (acquireConnection connStr) Hasql.release \notifyConn -> do
                            Async.concurrently_
                                -- Sender thread: fire notifications with small delays
                                -- so they arrive while waitForNotifications is active
                                (forM_ [1..totalNotifications] \i -> do
                                    let payload = BS8.pack (show i)
                                    Session.run (Session.sql ("NOTIFY nodrop_1, '" <> payload <> "'")) notifyConn
                                        >>= \case
                                            Right () -> pure ()
                                            Left err -> error ("NOTIFY failed: " <> show err)
                                    threadDelay 1_000
                                )
                                -- Main thread: subscribe to new channels while
                                -- notifications are being delivered, triggering
                                -- the race cancellation multiple times
                                (do
                                    threadDelay 10_000
                                    forM_ [(2::Int)..10] \i -> do
                                        let ch = BS8.pack ("nodrop_" <> show i)
                                        _ <- pgListener & PGListener.subscribe ch (const (pure ()))
                                        threadDelay 10_000
                                )

                        -- Wait for all notifications to be delivered
                        threadDelay 500_000

                        r <- readIORef received
                        length r `shouldBe` totalNotifications

        describe "runOncePerConnection" do
            it "should run action only once per key" do
                PGListener.withPGListener "" logger \pgListener -> do
                    counter <- newIORef (0 :: Int)
                    runOnce <- PGListener.runOncePerConnection pgListener

                    -- First call should run the action
                    runOnce "table1" (modifyIORef' counter (+ 1))
                    count1 <- readIORef counter
                    count1 `shouldBe` 1

                    -- Second call with same key should be no-op
                    runOnce "table1" (modifyIORef' counter (+ 1))
                    count2 <- readIORef counter
                    count2 `shouldBe` 1

                    -- Call with different key should run
                    runOnce "table2" (modifyIORef' counter (+ 1))
                    count3 <- readIORef counter
                    count3 `shouldBe` 2

            it "should clear cache when connection epoch changes" do
                PGListener.withPGListener "" logger \pgListener -> do
                    counter <- newIORef (0 :: Int)
                    runOnce <- PGListener.runOncePerConnection pgListener

                    -- First call runs
                    runOnce "table1" (modifyIORef' counter (+ 1))
                    count1 <- readIORef counter
                    count1 `shouldBe` 1

                    -- Second call is cached
                    runOnce "table1" (modifyIORef' counter (+ 1))
                    count2 <- readIORef counter
                    count2 `shouldBe` 1

                    -- Simulate a reconnect by incrementing the epoch
                    modifyIORef' pgListener.connectionEpoch (+ 1)

                    -- Now the cache should be cleared, so the action runs again
                    runOnce "table1" (modifyIORef' counter (+ 1))
                    count3 <- readIORef counter
                    count3 `shouldBe` 2

            it "should handle concurrent calls safely" do
                PGListener.withPGListener "" logger \pgListener -> do
                    counter <- newIORef (0 :: Int)
                    runOnce <- PGListener.runOncePerConnection pgListener

                    -- Run 100 concurrent calls for the same key
                    Async.replicateConcurrently_ 100 $
                        runOnce "table1" (modifyIORef' counter (+ 1))

                    count <- readIORef counter
                    count `shouldBe` 1  -- Should only run once despite concurrency
