module IHP.AuthSupport.Lockable where

import IHP.Prelude

lock :: forall user. (?modelContext :: ModelContext, CanUpdate user, UpdateField "lockedAt" user user (Maybe UTCTime) (Maybe UTCTime)) => user -> IO user
lock user = do
    now <- getCurrentTime
    let currentLockedAt :: Maybe UTCTime = user.lockedAt
    let user' :: user = updateField @"lockedAt" (Just now) user
    updateRecord user'

lockDuration :: NominalDiffTime
lockDuration = let timeInSecs = 60 * 60 in secondsToNominalDiffTime timeInSecs

isLocked :: forall user. (HasField "lockedAt" user (Maybe UTCTime)) => user -> IO Bool
isLocked user = do
    now <- getCurrentTime
    pure (isLocked' now user)

isLocked' :: forall user. (HasField "lockedAt" user (Maybe UTCTime)) => UTCTime -> user -> Bool
isLocked' now user =
    case user.lockedAt of
        Just lockedAt ->
            let diff = diffUTCTime now lockedAt
            in diff < lockDuration
        Nothing -> False