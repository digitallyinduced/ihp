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
, subscribeJSON
, unsubscribe
) where

import Prelude hiding (init, show, error)
import qualified Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Data.String.Conversions (cs)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Control.Monad (forever, unless, void, forM_)
import GHC.Records (HasField)
import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, displayException, uninterruptibleMask_)
import Control.Concurrent.Async (Async, async, cancel, uninterruptibleCancel)
import Data.Function ((&))

import IHP.Log.Types (Logger)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Data.HashMap.Strict as HashMap
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Aeson as Aeson
import qualified IHP.Log as Log
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Chan.Unagi as Queue
import qualified Control.Concurrent

import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Notifications as HasqlNotifications

-- | Local helper: show as Text
tshow :: Prelude.Show a => a -> Text
tshow = Text.pack . Prelude.show

-- | Wrapper to satisfy 'LoggingProvider' constraint for standalone logging
data LogContext = LogContext { logger :: !Logger }

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

    notifyLoopAsync <- async (notifyLoop logger databaseUrl listeningTo listenTo subscriptions)
    pure PGListener { logger, databaseUrl, listeningTo, subscriptions, listenTo, notifyLoopAsync }

-- | Stops the database listener async and releases the database connection
--
-- > PGListener.stop pgListener
--
stop :: PGListener -> IO ()
stop PGListener { notifyLoopAsync } = do
    cancel notifyLoopAsync

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
subscribe channel callback pgListener = do
    id <- UUID.nextRandom
    listenToChannelIfNeeded channel pgListener

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

    reader <- async $ forever do
            message <- Queue.readChan outChan
            callback message `Exception.catch` logException
    let subscription = Subscription { .. }

    modifyIORef' (pgListener.subscriptions) (HashMap.insertWith mappend channel [subscription] )

    pure subscription

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
    let
        deleteById :: [Subscription] -> [Subscription]
        deleteById = List.deleteBy (\a b -> a.id == b.id) subscription
    modifyIORef' (pgListener.subscriptions) (HashMap.adjust deleteById channel)
    uninterruptibleCancel reader
    pure ()

-- | Runs a @LISTEN ..;@ statements on the postgres connection, if not already listening on that channel
listenToChannelIfNeeded :: Channel -> PGListener -> IO ()
listenToChannelIfNeeded channel pgListener = do
    listeningTo <- MVar.readMVar (pgListener.listeningTo)
    let alreadyListening = channel `Set.member` listeningTo

    unless alreadyListening do
        MVar.putMVar (pgListener.listenTo) channel



-- | Acquires a dedicated hasql connection from the given database URL.
acquireConnection :: ByteString -> IO Hasql.Connection
acquireConnection databaseUrl = do
    let settings = [HasqlSetting.connection (HasqlConnection.string (cs databaseUrl))]
    result <- Hasql.acquire settings
    case result of
        Right connection -> pure connection
        Left err -> Prelude.error ("PGListener: Failed to connect to database: " <> Prelude.show err)

-- | The main loop that is receiving events from the database and triggering callbacks
--
-- Todo: What happens when the connection dies?
notifyLoop :: Logger -> ByteString -> MVar (Set Channel) -> MVar Channel -> IORef (HashMap Channel [Subscription]) -> IO ()
notifyLoop logger databaseUrl listeningToVar listenToVar subscriptions = do
    -- Wait until the first LISTEN is requested before opening a database connection
    MVar.readMVar listenToVar

    let innerLoop = do
            connection <- acquireConnection databaseUrl
            let cleanup = Hasql.release connection

            flip Exception.finally cleanup do
                -- If listeningTo already contains channels, this means that previously the database connection
                -- died, so we're restarting here. Therefore we need to replay all LISTEN calls to restore the previous state
                listeningTo <- MVar.readMVar listeningToVar
                forM_ listeningTo (listenToChannel connection)

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
                            Right newChannel -> do
                                MVar.modifyMVar_ listeningToVar \currentListeningTo ->
                                    if newChannel `Set.member` currentListeningTo
                                        then pure currentListeningTo
                                        else do
                                            listenToChannel connection newChannel
                                            pure (Set.insert newChannel currentListeningTo)
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
                    let ?context = LogContext logger
                    if isFirstError then do
                        Log.info ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying immediately.")
                        retryLoop delay False -- Retry with no delay interval on first error, but will increase delay interval in subsequent retries
                    else do
                        let increasedDelay = delay * 2 -- Double current delay
                        let nextDelay = min increasedDelay maxDelay -- Picks whichever delay is lowest of increasedDelay * 2 or maxDelay
                        Log.info ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying in " <> cs (printTimeToNextRetry delay) <> ".")
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
logError pgListener message = let ?context = LogContext pgListener.logger in Log.error message
