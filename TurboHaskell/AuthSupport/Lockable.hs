module TurboHaskell.AuthSupport.Lockable where

import ClassyPrelude
import           Control.Lens                         hiding ((|>))
import TurboHaskell.ModelSupport
import qualified Data.Time.Clock as Clock
import Unsafe.Coerce
import GHC.Records
import TurboHaskell.HaskellSupport

lock :: forall user modelContext. (?modelContext :: ModelContext, CanUpdate user, UpdateField "lockedAt" user user (Maybe UTCTime) (Maybe UTCTime)) => user -> IO user
lock user = do
    now <- getCurrentTime
    let currentLockedAt :: Maybe UTCTime = getField @"lockedAt" user
    let user' :: user = updateField @"lockedAt" (Just now) user
    updateRecord user'

lockDuration :: Clock.NominalDiffTime
lockDuration = let timeInSecs = 60 * 60 in Clock.secondsToNominalDiffTime timeInSecs

isLocked :: forall user. (HasField "lockedAt" user (Maybe UTCTime)) => user -> IO Bool
isLocked user = do
    now <- Clock.getCurrentTime
    let currentLockedAt :: Maybe UTCTime = getField @"lockedAt" user
    pure $! case currentLockedAt of
        Just lockedAt ->
            let diff = Clock.diffUTCTime now (unsafeCoerce lockedAt)
            in diff < lockDuration
        Nothing -> False