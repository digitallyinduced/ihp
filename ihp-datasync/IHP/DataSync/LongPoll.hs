{-|
Module: IHP.DataSync.LongPoll
Description: DataSync over plain HTTP requests
Copyright: (c) digitally induced GmbH, 2026

Some networks never let a WebSocket through, for example office proxies that
only forward completed HTTP responses. This module carries the DataSync
protocol over HTTP long polling instead: the client posts its messages with
one request and fetches the server's messages with another request that waits
until a message is available.

Every connection runs its own DataSync controller thread. The thread reads the
messages the client has posted and writes its responses into an outbox. A
response stays in the outbox until the client acknowledges it with its next
receive request, so a response that is lost on the way (e.g. cut off by a
proxy) is delivered again.

A connection closes when its controller stops, when the client closes it, or
when no request arrived for 'idleTimeout' seconds.
-}
module IHP.DataSync.LongPoll
    ( LongPollConnection (..)
    , LongPollBatch (..)
    , LongPollConfig (..)
    , LongPollRegistry
    , defaultLongPollConfig
    , newLongPollRegistry
    , globalLongPollRegistry
    , openLongPollConnection
    , lookupLongPollConnection
    , closeLongPollConnection
    , pushClientMessages
    , awaitServerMessages
    , encodeLongPollBatch
    ) where

import IHP.Prelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception.Safe as Exception
import Data.Foldable (toList)
import qualified Data.ByteString.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import GHC.Clock (getMonotonicTime)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Timeout as Timeout

data LongPollConfig = LongPollConfig
    { receiveTimeout :: !Int
    -- ^ Microseconds a receive request waits for a message before it returns an empty batch.
    -- Keep this below the read timeout of every proxy between client and server.
    , idleTimeout :: !Double
    -- ^ Seconds without any request after which the connection is closed.
    }

-- | Waits 20 seconds per receive request and closes connections after 60 idle seconds.
defaultLongPollConfig :: LongPollConfig
defaultLongPollConfig = LongPollConfig
    { receiveTimeout = 20_000_000
    , idleTimeout = 60
    }

data LongPollConnection = LongPollConnection
    { connectionId :: !UUID
    , ownerId :: !(Maybe Text)
    -- ^ The user who opened the connection. Every later request must come from the same user.
    , inbox :: !(TQueue ByteString)
    -- ^ Client messages the controller has not read yet
    , outbox :: !(TVar (Seq (Int, LByteString)))
    -- ^ Server messages the client has not acknowledged yet, by sequence number
    , lastSequence :: !(TVar Int)
    , lastActivity :: !(TVar Double)
    , closed :: !(TVar Bool)
    , worker :: !(Async ())
    }

-- | Server messages for one receive request
data LongPollBatch = LongPollBatch
    { messages :: ![LByteString]
    , lastSequence :: !Int
    -- ^ Sequence number of the last message. The client acknowledges it with its next receive request.
    , closed :: !Bool
    } deriving (Eq, Show)

type LongPollRegistry = TVar (HashMap UUID LongPollConnection)

newLongPollRegistry :: IO LongPollRegistry
newLongPollRegistry = newTVarIO HashMap.empty

-- | The connections of this server process. Long polling therefore needs every
-- request of a connection to reach the same process.
{-# NOINLINE globalLongPollRegistry #-}
globalLongPollRegistry :: LongPollRegistry
globalLongPollRegistry = unsafePerformIO newLongPollRegistry

-- | Starts a connection and its controller thread.
--
-- The controller gets an action that blocks until the client posts the next
-- message, and an action that queues a server message for the client.
openLongPollConnection
    :: LongPollRegistry
    -> LongPollConfig
    -> Maybe Text
    -> (IO ByteString -> (LByteString -> IO ()) -> IO ())
    -> IO LongPollConnection
openLongPollConnection registry config ownerId runController = do
    connectionId <- UUID.nextRandom
    inbox <- newTQueueIO
    outbox <- newTVarIO Seq.empty
    lastSequence <- newTVarIO 0
    lastActivity <- newTVarIO =<< getMonotonicTime
    closed <- newTVarIO False
    registered <- MVar.newEmptyMVar

    let receive = atomically (readTQueue inbox)
    let send message = atomically do
            sequenceNumber <- (+ 1) <$> readTVar lastSequence
            writeTVar lastSequence sequenceNumber
            modifyTVar' outbox (Seq.|> (sequenceNumber, message))
    let unregister = atomically do
            writeTVar closed True
            modifyTVar' registry (HashMap.delete connectionId)

    -- The thread starts masked, so a close right after opening still runs
    -- 'unregister'. It only starts the controller once the connection is in
    -- the registry, so a controller that stops at once cannot leave a closed
    -- connection behind.
    worker <- Exception.mask_ $ asyncWithUnmask \unmask ->
        unmask do
            MVar.readMVar registered
            race_ (runController receive send) (waitUntilIdle config lastActivity)
        `Exception.finally` unregister

    let connection = LongPollConnection { connectionId, ownerId, inbox, outbox, lastSequence, lastActivity, closed, worker }
    atomically (modifyTVar' registry (HashMap.insert connectionId connection))
    MVar.putMVar registered ()
    pure connection

waitUntilIdle :: LongPollConfig -> TVar Double -> IO ()
waitUntilIdle config lastActivity = loop
    where
        checkInterval = max 1 (round (config.idleTimeout * 1_000_000 / 4))
        loop = do
            threadDelay checkInterval
            now <- getMonotonicTime
            lastSeen <- readTVarIO lastActivity
            unless (now - lastSeen > config.idleTimeout) loop

-- | Finds an open connection of the given user
lookupLongPollConnection :: LongPollRegistry -> UUID -> Maybe Text -> IO (Maybe LongPollConnection)
lookupLongPollConnection registry connectionId ownerId = do
    connections <- readTVarIO registry
    pure case HashMap.lookup connectionId connections of
        Just connection | connection.ownerId == ownerId -> Just connection
        _ -> Nothing

-- | Stops the controller and removes the connection from its registry
closeLongPollConnection :: LongPollConnection -> IO ()
closeLongPollConnection connection = cancel connection.worker

-- | Hands client messages to the controller in the given order
pushClientMessages :: LongPollConnection -> [ByteString] -> IO ()
pushClientMessages connection messages = do
    touch connection
    atomically (mapM_ (writeTQueue connection.inbox) messages)

-- | Drops the messages up to the acknowledged sequence number and returns the
-- remaining ones. Waits up to 'receiveTimeout' when no message is pending.
awaitServerMessages :: LongPollConfig -> LongPollConnection -> Int -> IO LongPollBatch
awaitServerMessages config connection acknowledged = do
    touch connection
    atomically do
        modifyTVar' connection.outbox (Seq.dropWhileL (\(sequenceNumber, _) -> sequenceNumber <= acknowledged))
    _ <- Timeout.timeout config.receiveTimeout $ atomically do
        pending <- readTVar connection.outbox
        isClosed <- readTVar connection.closed
        unless (isClosed || not (Seq.null pending)) retry
    batch <- atomically do
        pending <- readTVar connection.outbox
        isClosed <- readTVar connection.closed
        pure LongPollBatch
            { messages = map snd (toList pending)
            , lastSequence = case Seq.viewr pending of
                _ Seq.:> (sequenceNumber, _) -> sequenceNumber
                Seq.EmptyR -> acknowledged
            , closed = isClosed
            }
    touch connection
    pure batch

touch :: LongPollConnection -> IO ()
touch connection = do
    now <- getMonotonicTime
    atomically (writeTVar connection.lastActivity now)

-- | Encodes a batch as @{"lastSequence":3,"closed":false,"messages":[...]}@.
-- The messages are already encoded JSON and are embedded as they are.
encodeLongPollBatch :: LongPollBatch -> LByteString
encodeLongPollBatch batch = Builder.toLazyByteString
    ( "{\"lastSequence\":" <> Builder.intDec batch.lastSequence
    <> ",\"closed\":" <> (if batch.closed then "true" else "false")
    <> ",\"messages\":[" <> mconcat (List.intersperse "," (map Builder.lazyByteString batch.messages)) <> "]}"
    )
