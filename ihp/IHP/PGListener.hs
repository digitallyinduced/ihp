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
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
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

-- | An event callback receives the postgres notification object and can do IO
type Callback = PG.Notification -> IO ()

-- | Returned by a call to 'subscribe'
data Subscription = Subscription
    { id :: !UUID
    , reader :: !(Async ())
    , inChan :: !(Queue.InChan PG.Notification)
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
            

            

-- | The main loop that is receiving events from the database and triggering callbacks
--
-- Todo: What happens when the connection dies?
notifyLoop :: (?modelContext :: ModelContext) => MVar (Set Channel) -> MVar Channel -> IORef (HashMap Channel [Subscription]) -> IO ()
notifyLoop listeningToVar listenToVar subscriptions = do
    -- Wait until the first LISTEN is requested before taking a database connection from the pool
    MVar.readMVar listenToVar

    let innerLoop = do
            withDatabaseConnection \databaseConnection -> do

                -- If listeningTo already contains channels, this means that previously the database connection
                -- died, so we're restarting here. Therefore we need to replay all LISTEN calls to restore the previous state
                listeningTo <- MVar.readMVar listeningToVar
                forEach listeningTo (listenToChannel databaseConnection)

                -- This loop reads channels from the 'listenToVar' and then triggers a LISTEN statement on
                -- the current database connections
                let listenLoop = forever do
                        channel <- MVar.takeMVar listenToVar
                        
                        MVar.modifyMVar_ listeningToVar \listeningTo -> do
                            let alreadyListening = channel `Set.member` listeningTo

                            if alreadyListening
                                then pure listeningTo
                                else do
                                    listenToChannel databaseConnection channel
                                    pure (Set.insert channel listeningTo)

                Async.withAsync listenLoop \listenLoopAsync -> do
                    forever do
                        notification <- PG.getNotification databaseConnection
                        let channel = notification.notificationChannel

                        allSubscriptions <- readIORef subscriptions
                        let channelSubscriptions = allSubscriptions
                                |> HashMap.lookup channel
                                |> fromMaybe []

                        forEach channelSubscriptions \subscription -> do
                            let inChan = subscription.inChan
                            Queue.writeChan inChan notification

    -- Initial delay (in microseconds)
    let initialDelay = 500 * 1000
    -- Max delay (in microseconds)
    let maxDelay = 60 * 1000 * 1000
    -- This outer loop restarts the listeners if the database connection dies (e.g. due to a timeout)
    let retryLoop delay isFirstError = do
            result <- Exception.tryAny innerLoop
            case result of
                Left error -> do
                    let ?context = ?modelContext -- Log onto the modelContext logger
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
    | microseconds >= 1000000000 =  show (microseconds `div` 1000000000) <> " min"
    | microseconds >= 1000000 =  show (microseconds `div` 1000000) <> " s"
    | microseconds >= 1000 = show (microseconds `div` 1000) <> " ms"
    | otherwise = show microseconds <> " µs"

listenToChannel :: PG.Connection -> Channel -> IO ()
listenToChannel databaseConnection channel = do
    PG.execute databaseConnection "LISTEN ?" [PG.Identifier (cs channel)]
    pure ()

logError :: PGListener -> Text -> IO ()
logError pgListener message = let ?context = pgListener.modelContext in Log.error message
