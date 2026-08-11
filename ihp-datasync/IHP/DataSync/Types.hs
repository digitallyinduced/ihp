module IHP.DataSync.Types where

import IHP.Prelude
import Data.Aeson
import IHP.DataSync.DynamicQuery
import qualified Hasql.Connection as Hasql
import Control.Concurrent.MVar as MVar


data DataSyncMessage
    = DataSyncQuery { query :: !DynamicSQLQuery, requestId :: !Int, transactionId :: !(Maybe UUID) }
    | CreateDataSubscription { query :: !DynamicSQLQuery, requestId :: !Int, clientSubscriptionId :: !(Maybe Int) }
    | CreateCountSubscription { query :: !DynamicSQLQuery, requestId :: !Int, clientSubscriptionId :: !(Maybe Int) }
    | DeleteDataSubscription { subscriptionId :: !Int, requestId :: !Int }
    | CreateRecordMessage { table :: !Text, record :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | CreateRecordsMessage { table :: !Text, records :: ![HashMap Text Value], requestId :: !Int, transactionId :: !(Maybe UUID) }
    | UpdateRecordMessage { table :: !Text, id :: !UUID, patch :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | UpdateRecordsMessage { table :: !Text, ids :: ![UUID], patch :: !(HashMap Text Value), requestId :: !Int, transactionId :: !(Maybe UUID) }
    | DeleteRecordMessage { table :: !Text, id :: !UUID, requestId :: !Int, transactionId :: !(Maybe UUID) }
    | DeleteRecordsMessage { table :: !Text, ids :: ![UUID], requestId :: !Int, transactionId :: !(Maybe UUID) }
    | StartTransaction { requestId :: !Int }
    | RollbackTransaction { requestId :: !Int, id :: !UUID }
    | CommitTransaction { requestId :: !Int, id :: !UUID }
    | LoginWithEmailAndPassword { requestId :: !Int, email :: !Text, password :: !Text }
    | LoginWithJWT { requestId :: !Int, jwt :: !Text }
    | CreateUser { requestId :: !Int, email :: !Text, password :: !Text }
    | ConfirmUser { requestId :: !Int, userId :: !UUID, token :: !Text }
    deriving (Eq, Show)

data DataSyncResponse
    = DataSyncResult { result :: ![[Field]], requestId :: !Int }
    | DataSyncError { requestId :: !Int, errorMessage :: !Text }
    | FailedToDecodeMessageError { errorMessage :: !Text }
    | DidCreateDataSubscription { requestId :: !Int, subscriptionId :: !Int, result :: ![[Field]] }
    | DidCreateCountSubscription { requestId :: !Int, subscriptionId :: !Int, count :: !Int }
    | DidDeleteDataSubscription { requestId :: !Int, subscriptionId :: !Int }
    | DidInsert { subscriptionId :: !Int, record :: ![Field] }
    | DidUpdate { subscriptionId :: !Int, id :: UUID, changeSet :: !(Maybe Value), appendSet :: !(Maybe Value) }
    | DidDelete { subscriptionId :: !Int, id :: !UUID }
    | DidChangeCount { subscriptionId :: !Int, count :: !Int }
    | DidCreateRecord { requestId :: !Int, record :: ![Field] } -- ^ Response to 'CreateRecordMessage'
    | DidCreateRecords { requestId :: !Int, records :: ![[Field]] } -- ^ Response to 'CreateRecordsMessage'
    | DidUpdateRecord { requestId :: !Int, record :: ![Field] } -- ^ Response to 'UpdateRecordMessage'
    | DidUpdateRecords { requestId :: !Int, records :: ![[Field]] } -- ^ Response to 'UpdateRecordsMessage'
    | DidDeleteRecord { requestId :: !Int }
    | DidDeleteRecords { requestId :: !Int }
    | DidStartTransaction { requestId :: !Int, transactionId :: !UUID }
    | DidRollbackTransaction { requestId :: !Int, transactionId :: !UUID }
    | DidCommitTransaction { requestId :: !Int, transactionId :: !UUID }

    | LoginSuccessful { requestId :: !Int, userId :: !UUID, jwt :: !Text }
    | UserLocked { requestId :: !Int }
    | UserUnconfirmed { requestId :: !Int }
    | InvalidCredentials { requestId :: !Int }

    | DidCreateUser { requestId :: !Int, userId :: !UUID, emailConfirmationRequired :: !Bool, jwt :: !Text }
    | CreateUserFailed { requestId :: !Int, validationFailures :: [(Text, Text)] }
    | DidConfirmUser { requestId :: !Int, jwt :: !Text }
    | DidConfirmUserAlready { requestId :: !Int }
    | ConfirmUserFailed { requestId :: !Int }

data GraphQLResult = GraphQLResult { graphQLResult :: !UndecodedJSON, requestId :: !Int }

data DataSyncTransaction
    = DataSyncTransaction
    { id :: !UUID
    , connection :: !Hasql.Connection
    , close :: MVar ()
    }

data DataSyncController
    = DataSyncController
    | DataSyncReady
        { subscriptions :: !(HashMap Int (MVar.MVar ()))
        , transactions :: !(HashMap UUID DataSyncTransaction)
        }
