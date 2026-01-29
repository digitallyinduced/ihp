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
, Subscription (..)
, PGListener (..)
, Notification (..)
, init
, stop
, withPGListener
, subscribe
, subscribeJSON
, unsubscribe
) where

import IHP.Prelude hiding (init)
import IHP.ModelSupport
import qualified Data.Set as Set
import qualified Hasql.Connection as Hasql
import qualified Hasql.Notifications as Notifications
import qualified Data.UUID.V4 as UUID
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

-- TODO: How to deal with timeout of the connection?

-- | The channel is like the event name
--
-- It's used in the postgres NOTIFY call:
--
-- > NOTIFY channel [ , payload ]
--
type Channel = ByteString

-- | A notification received from PostgreSQL
data Notification = Notification
    { notificationChannel :: !ByteString
    , notificationData :: !ByteString
    }

-- | An event callback receives a notification object and can do IO
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
    { modelContext :: !ModelContext
    , listeningTo :: !(MVar (Set Channel))
    , listenTo :: !(MVar Channel)
    , subscriptions :: !(IORef (HashMap Channel [Subscription]))
    , notifyLoopAsync :: !(Async ())
    }

-- | Creates a new 'PGListener' object
--
-- > let modelContext = ..
-- > pgListener <- PGListener.init modelContext
--
-- This will start a new async listening for postgres notifications. This will take one connection
-- from the database pool and keep it blocked until 'stop' is called.
--
init :: ModelContext -> IO PGListener
init modelContext = do
    listeningTo <- MVar.newMVar Set.empty
    subscriptions <- newIORef HashMap.empty
    listenTo <- MVar.newEmptyMVar

    let ?modelContext = modelContext
    notifyLoopAsync <- async (notifyLoop listeningTo listenTo subscriptions)
    pure PGListener { modelContext, listeningTo, subscriptions, listenTo, notifyLoopAsync }

-- | Stops the database listener async and puts the database connection used back into the database pool
--
-- > PGListener.stop pgListener
--
stop :: PGListener -> IO ()
stop PGListener { notifyLoopAsync } = do
    cancel notifyLoopAsync

withPGListener :: ModelContext -> (PGListener -> IO a) -> IO a
withPGListener modelContext =
    Exception.bracket (init modelContext) stop

-- | After you subscribed to a channel, the provided callback will be called whenever there's a new
-- notification on the channel.
subscribe :: Channel -> Callback -> PGListener -> IO Subscription
subscribe channel callback pgListener = do
    id <- UUID.nextRandom
    listenToChannelIfNeeded channel pgListener

    (inChan, outChan) <- Queue.newChan

    let
        logException :: SomeException -> IO ()
        logException exception = logError pgListener ("Error in pg_notify handler: " <> cs (displayException exception))

    reader <- async $ forever do
            message <- Queue.readChan outChan
            callback message `Exception.catch` logException
    let subscription = Subscription { .. }

    modifyIORef' (pgListener.subscriptions) (HashMap.insertWith mappend channel [subscription] )

    pure subscription

-- | Like 'subscribe' but decodes the notification payload from JSON
subscribeJSON :: Aeson.FromJSON jsonValue => Channel -> (jsonValue -> IO ()) -> PGListener -> IO Subscription
subscribeJSON channel callback pgListener = subscribe channel callback' pgListener
    where
        callback' notification = do
            let payload = notification.notificationData
            case Aeson.decodeStrict' payload of
                Just payload -> callback payload
                Nothing -> logError pgListener ("PGListener.subscribeJSON: Failed to parse " <> tshow payload)

-- | Stops the callback of a subscription from receiving further notifications
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


-- | The main loop that is receiving events from the database and triggering callbacks
notifyLoop :: (?modelContext :: ModelContext) => MVar (Set Channel) -> MVar Channel -> IORef (HashMap Channel [Subscription]) -> IO ()
notifyLoop listeningToVar listenToVar subscriptions = do
    -- Wait until the first LISTEN is requested before taking a database connection from the pool
    MVar.readMVar listenToVar

    let innerLoop = do
            -- Acquire a dedicated connection for listening
            conn <- acquireConnection ?modelContext

            -- Restore any existing LISTEN channels
            listeningTo <- MVar.readMVar listeningToVar
            forEach listeningTo \channel -> do
                Notifications.listen conn (Notifications.toPgIdentifier (cs channel))

            -- Start the listen loop in a separate thread to handle new LISTEN requests
            let listenLoop = forever do
                    channel <- MVar.takeMVar listenToVar

                    MVar.modifyMVar_ listeningToVar \currentListeningTo -> do
                        let alreadyListening = channel `Set.member` currentListeningTo

                        if alreadyListening
                            then pure currentListeningTo
                            else do
                                Notifications.listen conn (Notifications.toPgIdentifier (cs channel))
                                pure (Set.insert channel currentListeningTo)

            Async.withAsync listenLoop \_ -> do
                Notifications.waitForNotifications (\channel payload -> do
                    let notification = Notification { notificationChannel = channel, notificationData = payload }

                    allSubscriptions <- readIORef subscriptions
                    let channelSubscriptions = allSubscriptions
                            |> HashMap.lookup channel
                            |> fromMaybe []

                    forEach channelSubscriptions \subscription -> do
                        let inChan = subscription.inChan
                        Queue.writeChan inChan notification
                    ) conn

            Hasql.release conn

    -- Initial delay (in microseconds)
    let initialDelay = 500 * 1000
    -- Max delay (in microseconds)
    let maxDelay = 60 * 1000 * 1000
    -- This outer loop restarts the listeners if the database connection dies (e.g. due to a timeout)
    let retryLoop delay isFirstError = do
            result <- Exception.tryAny innerLoop
            case result of
                Left error -> do
                    let ?context = ?modelContext
                    if isFirstError then do
                        Log.info ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying immediately.")
                        retryLoop delay False
                    else do
                        let increasedDelay = delay * 2
                        let nextDelay = min increasedDelay maxDelay
                        Log.info ("PGListener is going to restart, loop failed with exception: " <> (displayException error) <> ". Retrying in " <> cs (printTimeToNextRetry delay) <> ".")
                        Control.Concurrent.threadDelay delay
                        retryLoop nextDelay False
                Right _ ->
                    retryLoop initialDelay True
    retryLoop initialDelay True

printTimeToNextRetry :: Int -> Text
printTimeToNextRetry microseconds
    | microseconds >= 1000000000 =  show (microseconds `div` 1000000000) <> " min"
    | microseconds >= 1000000 =  show (microseconds `div` 1000000) <> " s"
    | microseconds >= 1000 = show (microseconds `div` 1000) <> " ms"
    | otherwise = show microseconds <> " µs"

logError :: PGListener -> Text -> IO ()
logError pgListener message = let ?context = pgListener.modelContext in Log.error message
