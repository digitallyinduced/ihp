{-|
Module: IHP.PGListener
Description: Event listener handling pg_notify messages
Copyright: (c) digitally induced GmbH, 2021

This module is solving the problem, that previously IHP was using one database connection
per running @LISTEN ..;@ statement. A @PGListener@ provides one central object to listen on
postgres channels, without manually dealing with connection management.
-}
module IHP.PGListener
( Channel
, Callback
, Notification (..)
, Subscription (..)
, PGListener (..)
, init
, stop
, withPGListener
, subscribe
, subscribeWithReconnect
, subscribeJSON
, subscribeJSONWithReconnect
, unsubscribe
, onReconnect
, waitUntilListening
, waitUntilListeningTo
) where

import Prelude hiding (init, show, error)
import qualified Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Data.String.Conversions (cs)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Control.Monad (forever, unless, when, forM_, void)
import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, displayException, uninterruptibleMask_)
import Control.Concurrent.Async (Async, async, cancel, uninterruptibleCancel, waitCatch)
import Data.Function ((&))

import System.Log.FastLogger (FastLogger, toLogStr)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Data.HashMap.Strict as HashMap
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Aeson as Aeson
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Chan.Unagi as Queue
import qualified Control.Concurrent

import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Notifications as HasqlNotifications

-- | Local helper: show as Text
tshow :: Prelude.Show a => a -> Text
tshow = Text.pack . Prelude.show

-- | Logger type alias for PGListener
type Logger = FastLogger

-- TODO: How to deal with timeout of the connection?

-- | The channel is like the event name
--
-- It's used in the postgres NOTIFY call:
--
-- > NOTIFY channel [ , payload ]
--
type Channel = ByteString

-- | A notification received from postgres
data Notification = Notification
    { notificationChannel :: !ByteString
    , notificationData :: !ByteString
    } deriving (Prelude.Show)

-- | An event callback receives the notification and can do IO
type Callback = Notification -> IO ()

-- | Returned by a call to 'subscribe'
data Subscription = Subscription
    { id :: !UUID
    , reader :: !(Async ())
    , inChan :: !(Queue.InChan Notification)
    , channel :: !Channel
    }

-- | The main datatype of the service. Keeps tracks of all channels we're watching on, as well as all open subscriptions
--
-- Use 'init' to create a new object and 'stop' to deallocate it.
data PGListener = PGListener
    { logger :: !Logger
    , databaseUrl :: !ByteString
    , listeningTo :: !(MVar (Set Channel))
    , listenTo :: !(MVar Channel)
    , subscriptions :: !(IORef (HashMap Channel [Subscription]))
    , notifyLoopAsync :: !(Async ())
    , reconnectCallbacks :: !(IORef [Hasql.Connection -> IO ()])
    }

-- | Creates a new 'PGListener' object
--
-- > pgListener <- PGListener.init databaseUrl logger
--
-- This will start a new async listening for postgres notifications. This will open a dedicated
-- database connection and keep it blocked until 'stop' is called.
--
init :: ByteString -> Logger -> IO PGListener
init databaseUrl logger = do
    listeningTo <- MVar.newMVar Set.empty
    subscriptions <- newIORef HashMap.empty
    listenTo <- MVar.newEmptyMVar
    reconnectCallbacks <- newIORef []

    notifyLoopAsync <- async (notifyLoop logger databaseUrl listeningTo listenTo subscriptions reconnectCallbacks)
    pure PGListener { logger, databaseUrl, listeningTo, subscriptions, listenTo, notifyLoopAsync, reconnectCallbacks }

-- | Stops the database listener async and releases the database connection
--
-- > PGListener.stop pgListener
--
stop :: PGListener -> IO ()
stop PGListener { notifyLoopAsync, subscriptions, listeningTo, listenTo, reconnectCallbacks } = Exception.mask_ do
    cancel notifyLoopAsync
    void (waitCatch notifyLoopAsync)

    -- 'listeningTo' is also the lifecycle lock. Once the impossible PostgreSQL
    -- channel sentinel is installed in 'listenTo', later subscribe/onReconnect
    -- calls cannot repopulate state after this drain. Keeping the tombstone out
    -- of the public 'listeningTo' set preserves its live-channel semantics and
    -- the historical PGListener record construction shape.
    readers <- MVar.modifyMVar listeningTo \_activeChannels -> do
        void (MVar.tryTakeMVar listenTo)
        MVar.putMVar listenTo stoppedListenerChannel
        currentReaders <- atomicModifyIORef' subscriptions \currentSubscriptions ->
            (HashMap.empty, Prelude.map (.reader) (concat (HashMap.elems currentSubscriptions)))
        atomicModifyIORef_ reconnectCallbacks (const [])
        pure (Set.empty, currentReaders)
    mapM_ uninterruptibleCancel readers

withPGListener :: ByteString -> Logger -> (PGListener -> IO a) -> IO a
withPGListener databaseUrl logger =
    Exception.bracket (init databaseUrl logger) stop

-- | After you subscribed to a channel, the provided callback will be called whenever there's a new
-- notification on the channel.
--
-- > pgListener <- PGListener.init
-- >
-- > let callback notification = do
-- >         let payload :: Text = cs (notification.notificationData)
-- >         putStrLn ("Received notification: " <> payload)
-- >
-- > subscription <- pgListener |> PGListener.subscribe "my_channel" callback
--
-- The @callback@ function will now be called whenever @NOTIFY "my_channel", "my payload"@ is executed on the postgres server.
--
-- When the subscription is not used anymore, call 'unsubscribe' to stop the callback from being called anymore.
--
subscribe :: Channel -> Callback -> PGListener -> IO Subscription
subscribe channel callback pgListener =
    subscribeInternal channel ignoreReconnectNotification pgListener
    where
        ignoreReconnectNotification notification =
            unless (notification.notificationChannel == reconnectNotificationChannel) (callback notification)

-- | Subscription primitive with a lightweight callback that is run
-- after all desired LISTEN statements have been restored on a new connection.
-- The callback should only signal work; expensive work would delay notification
-- delivery for every channel.
subscribeWithReconnect :: Channel -> Callback -> IO () -> PGListener -> IO Subscription
subscribeWithReconnect channel callback reconnectCallback pgListener =
    subscribeInternal channel dispatch pgListener
    where
        dispatch notification
            | notification.notificationChannel == reconnectNotificationChannel = reconnectCallback
            | otherwise = callback notification

subscribeInternal :: Channel -> Callback -> PGListener -> IO Subscription
subscribeInternal channel callback pgListener = Exception.mask \restore -> do
    when (ByteString.elem 0 channel) do
        Exception.throwIO (userError "PGListener channels cannot contain NUL")
    id <- UUID.nextRandom

    -- We use a queue here to guarantee that the messages are processed in the right order
    -- while keeping high performance.
    --
    -- A naive implementation might be just kicking of an async for each message. But in that case
    -- the messages might be delivered to the final consumer out of order.
    (inChan, outChan) <- Queue.newChan

    let
        -- We need to log any exception, otherwise there might be silent errors
        logException :: SomeException -> IO ()
        logException exception = logError pgListener ("Error in pg_notify handler: " <> cs (displayException exception))

    reader <- Async.asyncWithUnmask \unmask -> unmask $ forever do
        message <- Queue.readChan outChan
        callback message `Exception.catch` logException
    let subscription = Subscription { .. }

    let removeSubscription = atomicModifyIORef_ (pgListener.subscriptions) (deleteSubscription subscription)
    let cleanupRegistration = removeSubscription >> uninterruptibleCancel reader

    let addSubscription = atomicModifyIORef' (pgListener.subscriptions) $ \subscriptions ->
            (HashMap.insertWith mappend channel [subscription] subscriptions, ())

    -- Install the local subscriber before requesting LISTEN. Registration and
    -- the stop tombstone check share the live-set MVar, making stop linearizable
    -- without adding a field to the public PGListener constructor.
    let register = MVar.modifyMVar_ pgListener.listeningTo \activeChannels -> do
            stopped <- listenerIsStopped pgListener
            when stopped do
                Exception.throwIO (userError "PGListener has already been stopped")
            addSubscription
            unless (channel `Set.member` activeChannels) do
                -- The MVar is only a wakeup. The listener reconciles every
                -- channel in the subscriptions map, so collapsed wakeups are safe.
                void (MVar.tryPutMVar pgListener.listenTo channel)
            pure activeChannels

    register `Exception.onException` cleanupRegistration

    -- Restore async exceptions before ownership of the reader is transferred to
    -- the caller. A cancellation in this final hand-off runs cleanup instead of
    -- leaking a registered reader whose Subscription was never returned.
    restore (pure subscription) `Exception.onException` cleanupRegistration

-- | Like 'subscribe' but decodes the notification payload from JSON and passes the decoded data structure to the callback
--
-- When JSON parsing fails, this will ignore the notification.
--
-- > pgListener <- PGListener.init
-- >
-- > let callback (jsonObject :: Aeson.Value) = do
-- >         putStrLn ("Received notification: " <> tshow jsonObject)
-- >
-- > subscription <- pgListener |> PGListener.subscribeJSON "my_json_channel" callback
--
-- The @callback@ function will now be called whenever @NOTIFY "my_json_channel", "{\"hello\":\"world\"}"@ is executed on the postgres server.
subscribeJSON :: Aeson.FromJSON jsonValue => Channel -> (jsonValue -> IO ()) -> PGListener -> IO Subscription
subscribeJSON channel callback pgListener = subscribe channel callback' pgListener
    where
        callback' notification = do
            let payload = (notification.notificationData)
            case Aeson.decodeStrict' payload of
                Just payload -> callback payload
                Nothing -> logError pgListener ("PGListener.subscribeJSON: Failed to parse " <> tshow payload)

-- | Like 'subscribeJSON', but also schedules a callback whenever PostgreSQL
-- LISTEN state has been restored after a connection loss. This allows consumers
-- to authoritatively revalidate state for notifications that may have committed
-- while the listener was disconnected.
subscribeJSONWithReconnect :: Aeson.FromJSON jsonValue => Channel -> (jsonValue -> IO ()) -> IO () -> PGListener -> IO Subscription
subscribeJSONWithReconnect channel callback reconnectCallback pgListener =
    subscribeWithReconnect channel callback' reconnectCallback pgListener
    where
        callback' notification = do
            let payload = notification.notificationData
            case Aeson.decodeStrict' payload of
                Just payload -> callback payload
                Nothing -> logError pgListener ("PGListener.subscribeJSONWithReconnect: Failed to parse " <> tshow payload)

-- | Stops the callback of a subscription from receiving further notifications
--
-- > pgListener <- PGListener.init
-- >
-- > subscription <- pgListener |> PGListener.subscribe "my_channel" callback
-- > doSomethingExpensive
-- > pgListener |> PGListener.unsubscribe subscription
--
unsubscribe :: Subscription -> PGListener -> IO ()
unsubscribe subscription@(Subscription { .. }) pgListener = do
    atomicModifyIORef_ (pgListener.subscriptions) (deleteSubscription subscription)
    uninterruptibleCancel reader
    pure ()

-- | Register a callback to be called when the PGListener reconnects after a connection loss.
--
-- The callback receives the live 'Hasql.Connection' so callers can run SQL directly
-- on the known-good connection (e.g. to recreate notification triggers).
--
-- > PGListener.onReconnect (\connection -> do
-- >     Hasql.Session.run (Hasql.Session.script triggerSQL) connection
-- >     pure ()
-- > ) pgListener
--
onReconnect :: (Hasql.Connection -> IO ()) -> PGListener -> IO ()
onReconnect callback pgListener =
    MVar.withMVar pgListener.listeningTo \_ -> do
        stopped <- listenerIsStopped pgListener
        unless stopped do
            atomicModifyIORef_ (pgListener.reconnectCallbacks) (callback :)

-- | Wait until a channel is known to be LISTENing on the current live
-- PostgreSQL connection. The live set is cleared before each reconnect, so a
-- successful result always belongs to the current connection generation.
waitUntilListening :: Int -> Channel -> PGListener -> IO Bool
waitUntilListening timeoutMicroseconds channel =
    waitUntilListeningTo timeoutMicroseconds (Set.singleton channel)

-- | Like 'waitUntilListening', for a set of channels that must all be live on
-- the same current connection generation.
waitUntilListeningTo :: Int -> Set Channel -> PGListener -> IO Bool
waitUntilListeningTo timeoutMicroseconds channels pgListener = go (max 0 timeoutMicroseconds)
    where
        go remaining = do
            activeChannels <- MVar.readMVar pgListener.listeningTo
            if channels `Set.isSubsetOf` activeChannels
                then pure True
                else if remaining <= 0
                    then pure False
                    else do
                        let delay = min 1_000 remaining
                        Control.Concurrent.threadDelay delay
                        go (remaining - delay)

-- | Acquires a dedicated hasql connection from the given database URL.
acquireConnection :: ByteString -> IO Hasql.Connection
acquireConnection databaseUrl = do
    result <- Hasql.acquire (HasqlSettings.connectionString (cs databaseUrl))
    case result of
        Right connection -> pure connection
        Left err -> Prelude.error ("PGListener: Failed to connect to database: " <> Prelude.show err)

-- | The main loop that is receiving events from the database and triggering callbacks
--
-- Todo: What happens when the connection dies?
notifyLoop :: Logger -> ByteString -> MVar (Set Channel) -> MVar Channel -> IORef (HashMap Channel [Subscription]) -> IORef [Hasql.Connection -> IO ()] -> IO ()
notifyLoop logger databaseUrl listeningToVar listenToVar subscriptions reconnectCallbacksRef = do
    -- Wait until the first LISTEN is requested before opening a database connection
    MVar.readMVar listenToVar
    connectionGenerationRef <- newIORef (0 :: Int)

    let innerLoop = do
            connection <- acquireConnection databaseUrl
            let cleanup = do
                    MVar.modifyMVar_ listeningToVar (const (pure Set.empty))
                    Hasql.release connection

            flip Exception.finally cleanup do
                -- Subscription keys are the durable desired set. The public
                -- listeningTo MVar contains only channels live on this connection.
                desiredChannels <- desiredSubscriptionChannels subscriptions
                forM_ desiredChannels (listenToChannel connection)
                MVar.modifyMVar_ listeningToVar (const (pure desiredChannels))

                previousGeneration <- atomicModifyIORef' connectionGenerationRef \generation ->
                    (generation + 1, generation)

                -- Fire reconnection callbacks only after every desired LISTEN has
                -- been restored and the new generation is visible to waiters.
                when (previousGeneration > 0) do
                    callbacks <- readIORef reconnectCallbacksRef
                    forM_ callbacks \callback ->
                        Exception.tryAny (callback connection) >>= \case
                            Left e -> logger (toLogStr ("PGListener reconnect callback failed: " <> displayException e))
                            Right _ -> pure ()

                    currentSubscriptions <- readIORef subscriptions
                    let reconnectNotification = Notification
                            { notificationChannel = reconnectNotificationChannel
                            , notificationData = ""
                            }
                    forM_ (concat (HashMap.elems currentSubscriptions)) \subscription ->
                        Queue.writeChan subscription.inChan reconnectNotification

                -- We use 'race' to alternate between waiting for notifications and
                -- processing new LISTEN requests. This avoids a deadlock: both
                -- 'waitForNotifications' and 'listen' acquire an exclusive lock on
                -- the underlying libpq connection via 'Connection.use'. If we ran
                -- them concurrently (as before), 'listen' would block forever
                -- waiting for 'waitForNotifications' to release the lock.
                --
                -- When 'race' cancels 'waitForNotifications', the lock is released,
                -- allowing 'listenToChannel' to acquire it. Any buffered notifications
                -- are preserved in the libpq connection and picked up when
                -- 'waitForNotifications' is restarted.
                let notifyAndListenLoop = do
                        result <- Async.race
                            (HasqlNotifications.waitForNotifications
                                (\channel payload -> uninterruptibleMask_ do
                                    -- uninterruptibleMask_ ensures that once waitForNotifications
                                    -- has dequeued a notification from libpq's buffer, the callback
                                    -- runs to completion even if race cancels us with an async exception.
                                    -- Without this, a notification could be lost: dequeued from libpq
                                    -- but never delivered to the subscription's inChan.
                                    let notification = Notification { notificationChannel = channel, notificationData = payload }

                                    allSubscriptions <- readIORef subscriptions
                                    let channelSubscriptions = allSubscriptions
                                            & HashMap.lookup channel
                                            & fromMaybe []

                                    forM_ channelSubscriptions \subscription ->
                                        Queue.writeChan (subscription.inChan) notification
                                )
                                connection
                            )
                            (MVar.takeMVar listenToVar)

                        case result of
                            Left () ->
                                -- waitForNotifications returned (connection lost) - exit to trigger retry
                                pure ()
                            Right _wakeupChannel -> do
                                -- The MVar is a wakeup, not a complete work queue. Multiple
                                -- concurrent subscribe calls may collapse into one slot, so
                                -- always reconcile the full desired set with this generation.
                                desiredChannels <- desiredSubscriptionChannels subscriptions
                                activeChannels <- MVar.readMVar listeningToVar
                                let missingChannels = desiredChannels `Set.difference` activeChannels
                                forM_ missingChannels \missingChannel -> do
                                    listenToChannel connection missingChannel
                                    MVar.modifyMVar_ listeningToVar \channels ->
                                        pure (Set.insert missingChannel channels)
                                notifyAndListenLoop

                notifyAndListenLoop

    -- Initial delay (in microseconds)
    let initialDelay = 500 * 1000
    -- Max delay (in microseconds)
    let maxDelay = 60 * 1000 * 1000
    -- This outer loop restarts the listeners if the database connection dies (e.g. due to a timeout)
    let retryLoop delay isFirstError = do
            result <- Exception.tryAny innerLoop
            case result of
                Left error -> do
                    if isFirstError then do
                        logger (toLogStr ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying immediately."))
                        retryLoop delay False -- Retry with no delay interval on first error, but will increase delay interval in subsequent retries
                    else do
                        let increasedDelay = delay * 2 -- Double current delay
                        let nextDelay = min increasedDelay maxDelay -- Picks whichever delay is lowest of increasedDelay * 2 or maxDelay
                        logger (toLogStr ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying in " <> cs (printTimeToNextRetry delay) <> "."))
                        Control.Concurrent.threadDelay delay -- Sleep for the current delay
                        retryLoop nextDelay False -- Retry with longer interval
                Right _ ->
                    retryLoop initialDelay True -- If all went well, re-run with no sleeping and reset current delay to the initial value
    retryLoop initialDelay True

printTimeToNextRetry :: Int -> Text
printTimeToNextRetry microseconds
    | microseconds >= 1000000000 = tshow (microseconds `div` 1000000000) <> " min"
    | microseconds >= 1000000 = tshow (microseconds `div` 1000000) <> " s"
    | microseconds >= 1000 = tshow (microseconds `div` 1000) <> " ms"
    | otherwise = tshow microseconds <> " µs"

listenToChannel :: Hasql.Connection -> Channel -> IO ()
listenToChannel connection channel = do
    HasqlNotifications.listen connection (HasqlNotifications.toPgIdentifier (cs channel))

logError :: PGListener -> Text -> IO ()
logError pgListener message = pgListener.logger (toLogStr message)

-- PostgreSQL identifiers cannot contain NUL, so this can never collide with a
-- real notification channel.
reconnectNotificationChannel :: Channel
reconnectNotificationChannel = "\0ihp_pglistener_reconnect"

-- Stored only in the private wakeup slot after 'stop'. PostgreSQL identifiers
-- cannot contain NUL, and 'subscribeInternal' rejects such channels.
stoppedListenerChannel :: Channel
stoppedListenerChannel = "\0ihp_pglistener_stopped"

listenerIsStopped :: PGListener -> IO Bool
listenerIsStopped pgListener =
    (== Just stoppedListenerChannel) <$> MVar.tryReadMVar pgListener.listenTo

desiredSubscriptionChannels :: IORef (HashMap Channel [Subscription]) -> IO (Set Channel)
desiredSubscriptionChannels subscriptionsRef = do
    subscriptions <- readIORef subscriptionsRef
    pure (Set.fromList (HashMap.keys subscriptions))

deleteSubscription :: Subscription -> HashMap Channel [Subscription] -> HashMap Channel [Subscription]
deleteSubscription subscription =
    HashMap.update removeById subscription.channel
    where
        removeById subscriptions =
            case List.deleteBy (\a b -> a.id == b.id) subscription subscriptions of
                [] -> Nothing
                remaining -> Just remaining

atomicModifyIORef_ :: IORef value -> (value -> value) -> IO ()
atomicModifyIORef_ ref update =
    atomicModifyIORef' ref (\value -> (update value, ()))
