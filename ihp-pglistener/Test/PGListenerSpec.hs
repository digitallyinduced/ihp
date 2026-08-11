{-|
Module: Test.PGListenerSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module PGListenerSpec where

import Prelude
import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.String.Conversions (cs)
import Data.Function ((&))
import Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forM_, void)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import System.Environment (lookupEnv)

import System.Log.FastLogger (FastLogger)
import qualified IHP.PGListener as PGListener

import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session

logger :: FastLogger
logger = \_ -> pure ()

getDatabaseUrl :: IO ByteString
getDatabaseUrl = do
    envUrl <- lookupEnv "DATABASE_URL"
    pure (maybe "postgresql:///postgres" cs envUrl)

acquireConnection :: ByteString -> IO Hasql.Connection
acquireConnection databaseUrl = do
    result <- Hasql.acquire (HasqlSettings.connectionString (cs databaseUrl))
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
    result <- Hasql.use conn (Session.script (cs sql))
    case result of
        Right () -> pure ()
        Left err -> error ("SQL exec failed: " <> show err)

withApplicationName :: ByteString -> ByteString -> ByteString
withApplicationName connStr applicationName
    | "postgresql://" `BS8.isPrefixOf` connStr || "postgres://" `BS8.isPrefixOf` connStr =
        connStr <> separator <> "application_name=" <> applicationName
    | otherwise = connStr <> " application_name=" <> applicationName
    where
        separator = if BS8.elem '?' connStr then "&" else "?"

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

            it "keeps desired subscriptions separate from live readiness" do
                PGListener.withPGListener "" logger \pgListener -> do
                    _ <- pgListener & PGListener.subscribe "desired_but_not_live" (const (pure ()))

                    subscriptions <- readIORef pgListener.subscriptions
                    HashMap.member "desired_but_not_live" subscriptions `shouldBe` True
                    liveChannels <- MVar.readMVar pgListener.listeningTo
                    Set.member "desired_but_not_live" liveChannels `shouldBe` False

                    -- The invalid connection string cannot establish LISTEN. Readiness
                    -- must not be inferred from the subscription set, and waiting
                    -- must remain bounded.
                    ready <- PGListener.waitUntilListening 10_000 "desired_but_not_live" pgListener
                    ready `shouldBe` False

            it "times out while no LISTEN connection has been requested" do
                PGListener.withPGListener "" logger \pgListener -> do
                    ready <- PGListener.waitUntilListening 5_000 "never_requested" pgListener
                    ready `shouldBe` False

            it "preserves the public PGListener record construction shape" do
                listeningTo <- MVar.newMVar Set.empty
                listenTo <- MVar.newEmptyMVar
                subscriptions <- newIORef HashMap.empty
                notifyLoopAsync <- Async.async (pure ())
                reconnectCallbacks <- newIORef []
                let pgListener = PGListener.PGListener
                        { logger
                        , databaseUrl = ""
                        , listeningTo
                        , listenTo
                        , subscriptions
                        , notifyLoopAsync
                        , reconnectCallbacks
                        }
                pgListener.databaseUrl `shouldBe` ""
                Async.cancel notifyLoopAsync

            it "adds an application name to URI and keyword connection strings" do
                withApplicationName "postgresql:///postgres" "listener"
                    `shouldBe` "postgresql:///postgres?application_name=listener"
                withApplicationName "postgresql:///postgres?host=/tmp" "listener"
                    `shouldBe` "postgresql:///postgres?host=/tmp&application_name=listener"
                withApplicationName "host=/tmp dbname=postgres" "listener"
                    `shouldBe` "host=/tmp dbname=postgres application_name=listener"

        describe "unsubscribe" do
            it "remove the subscription" do
                PGListener.withPGListener "" logger \pgListener -> do
                    subscription <- pgListener & PGListener.subscribe "did_insert_record" (const (pure ()))
                    pgListener & PGListener.unsubscribe subscription

                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions

                    subscriptionsCount `shouldBe` 0

        describe "stop" do
            it "cancels and drains subscription readers and registrations" do
                Exception.bracket (PGListener.init "" logger) PGListener.stop \pgListener -> do
                    subscription <- pgListener & PGListener.subscribeWithReconnect
                        "stop_test"
                        (const (pure ()))
                        (pure ())
                    PGListener.onReconnect (const (pure ())) pgListener

                    PGListener.stop pgListener

                    readerResult <- Async.poll subscription.reader
                    readerResult `shouldSatisfy` \case
                        Just _ -> True
                        Nothing -> False
                    (HashMap.null <$> readIORef pgListener.subscriptions) `shouldReturn` True
                    MVar.readMVar pgListener.listeningTo `shouldReturn` Set.empty
                    (Prelude.null <$> readIORef pgListener.reconnectCallbacks) `shouldReturn` True

                    -- stop is a lifecycle barrier: registrations racing after
                    -- the drain must not create readers or reconnect callbacks.
                    lateSubscription <- Exception.tryAny
                        (pgListener & PGListener.subscribe "after_stop" (const (pure ())))
                    case lateSubscription of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "subscribe succeeded after PGListener.stop"
                    PGListener.onReconnect (const (pure ())) pgListener
                    (HashMap.null <$> readIORef pgListener.subscriptions) `shouldReturn` True
                    (Prelude.null <$> readIORef pgListener.reconnectCallbacks) `shouldReturn` True

        describe "multi-channel notifications" do
            it "restores every desired LISTEN when concurrent wakeups collapse" do
                withDB \connStr -> do
                    PGListener.withPGListener connStr logger \pgListener -> do
                        let channels = Set.fromList
                                [ BS8.pack ("collapsed_wakeup_" <> show index)
                                | index <- [(1 :: Int)..50]
                                ]
                        _subscriptions <- Async.mapConcurrently
                            (\channel -> pgListener & PGListener.subscribe channel (const (pure ())))
                            (Set.toList channels)

                        PGListener.waitUntilListeningTo 2_000_000 channels pgListener
                            `shouldReturn` True

            it "should receive notifications on multiple channels" do
                withDB \connStr -> do
                    PGListener.withPGListener connStr logger \pgListener -> do
                        received1 <- newIORef ([] :: [ByteString])
                        received2 <- newIORef ([] :: [ByteString])

                        _sub1 <- pgListener & PGListener.subscribe "test_channel_1" \n ->
                            modifyIORef' received1 (n.notificationData :)
                        _sub2 <- pgListener & PGListener.subscribe "test_channel_2" \n ->
                            modifyIORef' received2 (n.notificationData :)

                        ready <- PGListener.waitUntilListeningTo 2_000_000
                            (Set.fromList ["test_channel_1", "test_channel_2"])
                            pgListener
                        ready `shouldBe` True

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

                        -- Subscribe to channel_1 and wait for the current LISTEN
                        -- generation rather than sleeping on stale desired state.
                        _sub1 <- pgListener & PGListener.subscribe "nodrop_1" \n ->
                            modifyIORef' received (n.notificationData :)
                        ready <- PGListener.waitUntilListening 2_000_000 "nodrop_1" pgListener
                        ready `shouldBe` True

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
                                    Hasql.use notifyConn (Session.script (cs ("NOTIFY nodrop_1, '" <> payload <> "'")))
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

            it "restores LISTEN state and signals reconnect subscribers" do
                withDB \connStr -> do
                    -- Give the dedicated LISTEN connection a stable identifier.
                    -- pg_stat_activity.query is not reliable here because libpq's
                    -- notification API does not leave the LISTEN text there while
                    -- the backend is idle.
                    let listenerApplicationName = "ihp_pglistener_reconnect_test"
                    let listenerConnStr = withApplicationName connStr listenerApplicationName
                    PGListener.withPGListener listenerConnStr logger \pgListener -> do
                        let channel = "reconnect_restore_test"
                        reconnected <- MVar.newEmptyMVar
                        received <- MVar.newEmptyMVar
                        _subscription <- pgListener & PGListener.subscribeWithReconnect channel
                            (\notification -> void (MVar.tryPutMVar received notification.notificationData))
                            (void (MVar.tryPutMVar reconnected ()))

                        PGListener.waitUntilListening 2_000_000 channel pgListener `shouldReturn` True

                        execSQL connStr (BS8.pack (
                            "DO $$ DECLARE listener_pid integer; BEGIN " <>
                            "SELECT pid INTO listener_pid FROM pg_stat_activity " <>
                            "WHERE datname = current_database() AND pid <> pg_backend_pid() " <>
                            "AND application_name = '" <> BS8.unpack listenerApplicationName <> "' LIMIT 1; " <>
                            "IF listener_pid IS NULL THEN RAISE EXCEPTION 'listener backend not found'; END IF; " <>
                            "PERFORM pg_terminate_backend(listener_pid); END $$;"
                            ))

                        reconnectResult <- Async.race (threadDelay 10_000_000) (MVar.takeMVar reconnected)
                        reconnectResult `shouldBe` Right ()
                        PGListener.waitUntilListening 2_000_000 channel pgListener `shouldReturn` True

                        execSQL connStr "NOTIFY reconnect_restore_test, 'restored'"
                        notificationResult <- Async.race (threadDelay 2_000_000) (MVar.takeMVar received)
                        notificationResult `shouldBe` Right "restored"
