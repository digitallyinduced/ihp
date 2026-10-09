module IHP.DataSync.LongPoll.Types where

import IHP.Prelude

-- | HTTP long polling transport for DataSync, see "IHP.DataSync.LongPoll"
data DataSyncLongPollController
    = OpenLongPollAction -- ^ POST /DataSyncLongPoll
    | SendLongPollAction { connectionId :: !UUID } -- ^ POST /DataSyncLongPoll/:connectionId/send with a JSON array of DataSync messages
    | ReceiveLongPollAction { connectionId :: !UUID } -- ^ POST /DataSyncLongPoll/:connectionId/receive?after=:lastSequence
    | CloseLongPollAction { connectionId :: !UUID } -- ^ POST /DataSyncLongPoll/:connectionId/close
    deriving (Eq, Show, Data)
