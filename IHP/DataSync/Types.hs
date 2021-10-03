module IHP.DataSync.Types where

import IHP.Prelude
import Data.Aeson
import IHP.QueryBuilder
import IHP.DataSync.DynamicQuery

data DataSyncMessage
    = DataSyncQuery { query :: !SQLQuery, requestId :: !Int }
    | CreateDataSubscription { query :: !SQLQuery, requestId :: !Int }
    | DeleteDataSubscription { subscriptionId :: !UUID, requestId :: !Int }
    deriving (Eq, Show)

data DataSyncResponse
    = DataSyncResult { result :: [[Field]], requestId :: !Int }
    | DataSyncError { requestId :: !Int }
    | FailedToDecodeMessageError
    | DidCreateDataSubscription { requestId :: !Int, subscriptionId :: UUID, result :: [[Field]] }
    | DidDeleteDataSubscription { requestId :: !Int, subscriptionId :: UUID }
    | DidInsert { subscriptionId :: UUID, record :: [Field] }
    | DidUpdate { subscriptionId :: UUID, id :: UUID, changeSet :: Value }
    | DidDelete { subscriptionId :: UUID, id :: UUID }

data Subscription = Subscription { id :: UUID, tableWatcher :: Async () }
    deriving (Eq)

data DataSyncController
    = DataSyncController
    | DataSyncReady { subscriptions :: HashMap UUID Subscription }
    deriving (Eq)
