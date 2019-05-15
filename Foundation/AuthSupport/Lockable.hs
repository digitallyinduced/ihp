module Foundation.AuthSupport.Lockable where

import ClassyPrelude
import           Control.Lens                         hiding ((|>))
import           Data.Generics.Product
import           Data.Generics.Product.Types
import Foundation.ModelSupport
import qualified Data.Time.Clock as Clock
import Unsafe.Coerce

lock :: forall user modelContext. (?modelContext :: ModelContext, CanUpdate user, HasField "lockedAt" user user (Maybe UTCTime) (Maybe UTCTime), Generic user) => user -> IO user
lock user = do
	now <- getCurrentTime
	let currentLockedAt :: Maybe UTCTime = user ^. field @"lockedAt"
	let user' :: user = user & field @"lockedAt" .~ (Just now)
	updateRecord user'

lockDuration :: Clock.NominalDiffTime
lockDuration = let timeInSecs = 60 * 60 in Clock.secondsToNominalDiffTime timeInSecs

isLocked :: forall user. (HasField "lockedAt" user user (Maybe UTCTime) (Maybe UTCTime)) => user -> IO Bool
isLocked user = do
    now <- Clock.getCurrentTime
    let currentLockedAt :: Maybe UTCTime = user ^. field @"lockedAt"
    return $! case currentLockedAt of
        Just lockedAt ->
            let diff = Clock.diffUTCTime now (unsafeCoerce lockedAt)
            in diff < lockDuration
        Nothing -> False