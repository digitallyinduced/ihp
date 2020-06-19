module IHP.AuthSupport.Lockable where

import IHP.Prelude

lock :: forall user. (?modelContext :: ModelContext, CanUpdate user, UpdateField "lockedAt" user user (Maybe UTCTime) (Maybe UTCTime)) => user -> IO user
lock user = do
    now <- getCurrentTime
    let currentLockedAt :: Maybe UTCTime = getField @"lockedAt" user
    let user' :: user = updateField @"lockedAt" (Just now) user
    updateRecord user'

lockDuration :: NominalDiffTime
lockDuration = let timeInSecs = 60 * 60 in secondsToNominalDiffTime timeInSecs

isLocked :: forall user. (HasField "lockedAt" user (Maybe UTCTime)) => user -> IO Bool
isLocked user = do
    now <- getCurrentTime
    let currentLockedAt :: Maybe UTCTime = getField @"lockedAt" user
    pure $! case currentLockedAt of
        Just lockedAt ->
            let diff = diffUTCTime now lockedAt
            in diff < lockDuration
        Nothing -> False