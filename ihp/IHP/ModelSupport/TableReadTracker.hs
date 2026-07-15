{-# LANGUAGE NamedFieldPuns #-}

-- | Internal support for attaching structured metadata to the existing
-- 'ModelContext' table-read callback without changing its public type.
module IHP.ModelSupport.TableReadTracker
    ( TrackedTableRead (..)
    , withTrackedTableReadCallback
    , hasTrackedTableReadCallback
    , trackWholeTableRead
    , trackStructuredTableRead
    ) where

import Prelude
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Data.Dynamic (Dynamic)
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)

type TableReadCallback = Text -> IO ()

-- | A read observed through the private structured tracking channel.
-- Unknown structured values are handled conservatively by the consumer.
data TrackedTableRead
    = TrackedWholeTableRead !Text
    | TrackedStructuredTableRead !Text !Dynamic

data RegisteredTracker = RegisteredTracker
    { registrationId :: !Unique
    , callbackName :: !(StableName TableReadCallback)
    , handleRead :: !(TrackedTableRead -> IO ())
    }

{-# NOINLINE registeredTrackers #-}
registeredTrackers :: MVar [RegisteredTracker]
registeredTrackers = unsafePerformIO (MVar.newMVar [])

-- | Install a structured tracker for the dynamic extent of an action.
-- The stable name associates metadata with the unchanged public callback and
-- continues to work when the model context is used from a child thread.
withTrackedTableReadCallback :: TableReadCallback -> (TrackedTableRead -> IO ()) -> IO a -> IO a
withTrackedTableReadCallback callback handleRead action =
    Exception.bracket register unregister (const action)
    where
        register = do
            registrationId <- newUnique
            callbackName <- makeStableName callback
            MVar.modifyMVar_ registeredTrackers \trackers ->
                pure (RegisteredTracker { registrationId, callbackName, handleRead } : trackers)
            pure registrationId

        unregister registrationId =
            MVar.modifyMVar_ registeredTrackers \trackers ->
                pure (filter (\RegisteredTracker { registrationId = currentId } -> currentId /= registrationId) trackers)

-- | Whether structured metadata is currently consumed for this callback.
hasTrackedTableReadCallback :: TableReadCallback -> IO Bool
hasTrackedTableReadCallback callback = do
    callbackName <- makeStableName callback
    trackers <- MVar.readMVar registeredTrackers
    pure (any (\RegisteredTracker { callbackName = currentName } -> currentName == callbackName) trackers)

trackWholeTableRead :: TableReadCallback -> Text -> IO ()
trackWholeTableRead callback tableName =
    dispatch callback (TrackedWholeTableRead tableName)

trackStructuredTableRead :: TableReadCallback -> Text -> Dynamic -> IO ()
trackStructuredTableRead callback tableName structuredRead =
    dispatch callback (TrackedStructuredTableRead tableName structuredRead)

dispatch :: TableReadCallback -> TrackedTableRead -> IO ()
dispatch callback trackedRead = do
    callbackName <- makeStableName callback
    trackers <- MVar.readMVar registeredTrackers
    let matchingTrackers = filter (\RegisteredTracker { callbackName = currentName } -> currentName == callbackName) trackers
    mapM_ (\RegisteredTracker { handleRead } -> handleRead trackedRead) matchingTrackers
