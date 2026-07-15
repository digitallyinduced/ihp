{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import IHP.Prelude
import Wai.Request.Params.Middleware (Respond)
import Control.Concurrent.MVar (MVar)
import qualified IHP.PGListener as PGListener
import qualified Data.Map.Strict as Map
import IHP.QueryBuilder.Types (QueryBuilderRead)
import Network.Wai (Request, ResponseReceived)

data AutoRefreshState = AutoRefreshEnabled { sessionId :: !UUID }

-- | The reads of a table that an AutoRefresh session depends on.
--
-- A manual or unknown read is an absorbing whole-table dependency. Only when
-- every read of a table came from the QueryBuilder do we retain the structured
-- queries for future relevance checks.
data AutoRefreshReadDependency
    = AutoRefreshWholeTable
    | AutoRefreshQueryBuilderReads ![QueryBuilderRead]
    deriving (Show)

data AutoRefreshSession = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView :: !(Request -> Respond -> IO ResponseReceived)
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set Text)
        -- | Structured QueryBuilder reads, or a conservative whole-table
        -- dependency when raw SQL or another unknown read touched the table.
        , readDependencies :: !(Map.Map Text AutoRefreshReadDependency)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set Text)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }
