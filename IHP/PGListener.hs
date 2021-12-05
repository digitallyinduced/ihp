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
, PGListener
, init
, stop
, subscribe
, subscribeJSON
, unsubscribe
) where

import IHP.Prelude hiding (init)
import IHP.ModelSupport
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Data.UUID.V4 as UUID
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HashMap
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Aeson as Aeson

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
    , callback :: !Callback
    , channel :: !Channel
    }

-- | The main datatype of the service. Keeps tracks of all channels we're watching on, as well as all open subscriptions
--
-- Use 'init' to create a new object and 'stop' to deallocate it.
data PGListener = PGListener
    { modelContext :: !ModelContext
    , listeningTo :: !(MVar (Set Channel))
    , subscriptions :: !(IORef (HashMap Channel [Subscription]))
    , connection :: !(MVar PG.Connection)
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
    connection <- MVar.newEmptyMVar

    let ?modelContext = modelContext
    notifyLoopAsync <- async (notifyLoop connection subscriptions)
    pure PGListener { modelContext, listeningTo, subscriptions, connection, notifyLoopAsync }

-- | Stops the database listener async and puts the database connection used back into the database pool
--
-- > PGListener.stop pgListener
--
stop :: PGListener -> IO ()
stop PGListener { notifyLoopAsync } = do
    cancel notifyLoopAsync

-- | After you subscribed to a channel, the provided callback will be called whenever there's a new
-- notification on the channel.
--
-- > pgListener <- PGListener.init
-- >
-- > let callback notification = do
-- >         let payload :: Text = cs (get #notificationData notification)
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

    let subscription = Subscription { .. }

    modifyIORef' (get #subscriptions pgListener) (HashMap.insertWith mappend channel [subscription] )

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
            let payload = (get #notificationData notification)
            case Aeson.decodeStrict' payload of
                Just payload -> callback payload
                Nothing -> pure ()

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
        deleteById :: UUID -> [Subscription] -> [Subscription]
        deleteById id = List.deleteBy (\a b -> get #id a == get #id b) subscription
    modifyIORef' (get #subscriptions pgListener) (HashMap.adjust (deleteById id) channel)
    pure ()     

-- | Runs a @LISTEN ..;@ statements on the postgres connection, if not already listening on that channel
listenToChannelIfNeeded :: Channel -> PGListener -> IO ()
listenToChannelIfNeeded channel pgListener = MVar.modifyMVar_ (get #listeningTo pgListener) \listeningTo -> do
    let alreadyListening = channel `Set.member` listeningTo
    if alreadyListening
        then pure listeningTo
        else do
            connection <- MVar.readMVar (get #connection pgListener)

            PG.execute connection "LISTEN ?" [PG.Identifier (cs channel)]

            pure (Set.insert channel listeningTo)

-- | The main loop that is receiving events from the database and triggering callbacks
--
-- Todo: What happens when the connection dies?
notifyLoop :: (?modelContext :: ModelContext) => MVar PG.Connection -> IORef (HashMap Channel [Subscription]) -> IO ()
notifyLoop connectionVar subscriptions = do
    withDatabaseConnection \databaseConnection -> do
        MVar.putMVar connectionVar databaseConnection
        forever do
            notification <- PG.getNotification databaseConnection
            let channel = get #notificationChannel notification

            allSubscriptions <- readIORef subscriptions
            let channelSubscriptions = allSubscriptions
                    |> HashMap.lookup channel
                    |> fromMaybe []

            Async.forConcurrently channelSubscriptions \subscription -> do
                (get #callback subscription) notification