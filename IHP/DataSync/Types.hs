module IHP.DataSync.Types where

import IHP.Prelude
import Data.Aeson
import IHP.QueryBuilder
import IHP.DataSync.DynamicQuery
import Data.HashMap.Strict (HashMap)
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple as PG
import Control.Concurrent.MVar as MVar
import qualified IHP.GraphQL.Types as GraphQL


data DataSyncMessage
    = DataSyncQuery { query :: !DynamicSQLQuery, requestId :: !Int, transactionId :: !(Maybe UUID) }
    | CreateDataSubscription { query :: !DynamicSQLQuery, requestId :: !Int }
    | DeleteDataSubscription { subscriptionId :: !UUID, requestId :: !Int }
    | CreateRecordMessage { table :: !Text, record :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | CreateRecordsMessage { table :: !Text, records :: ![HashMap Text Value], requestId :: !Int, transactionId :: !(Maybe UUID) }
    | UpdateRecordMessage { table :: !Text, id :: !UUID, patch :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | UpdateRecordsMessage { table :: !Text, ids :: ![UUID], patch :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | DeleteRecordMessage { table :: !Text, id :: !UUID, requestId :: !Int, transactionId :: !(Maybe UUID) }
    | DeleteRecordsMessage { table :: !Text, ids :: ![UUID], requestId :: !Int, transactionId :: !(Maybe UUID) }
    | StartTransaction { requestId :: !Int }
    | RollbackTransaction { requestId :: !Int, id :: !UUID }
    | CommitTransaction { requestId :: !Int, id :: !UUID }
    deriving (Eq, Show)

data DataSyncResponse
    = DataSyncResult { result :: ![[Field]], requestId :: !Int }
    | DataSyncError { requestId :: !Int, errorMessage :: !Text }
    | FailedToDecodeMessageError { errorMessage :: !Text }
    | DidCreateDataSubscription { requestId :: !Int, subscriptionId :: !UUID, result :: ![[Field]] }
    | DidDeleteDataSubscription { requestId :: !Int, subscriptionId :: !UUID }
    | DidInsert { subscriptionId :: !UUID, record :: ![Field] }
    | DidUpdate { subscriptionId :: !UUID, id :: UUID, changeSet :: !Value }
    | DidDelete { subscriptionId :: !UUID, id :: !UUID }
    | DidCreateRecord { requestId :: !Int, record :: ![Field] } -- ^ Response to 'CreateRecordMessage'
    | DidCreateRecords { requestId :: !Int, records :: ![[Field]] } -- ^ Response to 'CreateRecordsMessage'
    | DidUpdateRecord { requestId :: !Int, record :: ![Field] } -- ^ Response to 'UpdateRecordMessage'
    | DidUpdateRecords { requestId :: !Int, records :: ![[Field]] } -- ^ Response to 'UpdateRecordsMessage'
    | DidDeleteRecord { requestId :: !Int }
    | DidDeleteRecords { requestId :: !Int }
    | DidStartTransaction { requestId :: !Int, transactionId :: !UUID }
    | DidRollbackTransaction { requestId :: !Int, transactionId :: !UUID }
    | DidCommitTransaction { requestId :: !Int, transactionId :: !UUID }

data GraphQLResult = GraphQLResult { graphQLResult :: !UndecodedJSON, requestId :: !Int }

data DataSyncTransaction
    = DataSyncTransaction
    { id :: !UUID
    , connection :: !PG.Connection
    , close :: MVar ()
    }

data DataSyncController
    = DataSyncController
    | DataSyncReady
        { subscriptions :: !(HashMap UUID (MVar.MVar ()))
        , transactions :: !(HashMap UUID DataSyncTransaction)
        , asyncs :: ![Async ()]
        }
