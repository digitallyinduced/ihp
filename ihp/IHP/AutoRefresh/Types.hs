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
    deriving (Eq, Show)

-- | AutoRefresh session state.
--
-- The original 'AutoRefreshSession' constructor is retained unchanged for
-- source compatibility. Sessions created by AutoRefresh use
-- 'TrackedAutoRefreshSession' to retain bounded structured dependencies.
data AutoRefreshSession
    = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView :: !(Request -> Respond -> IO ResponseReceived)
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set Text)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        }
    | TrackedAutoRefreshSession
        { id :: !UUID
        , renderView :: !(Request -> Respond -> IO ResponseReceived)
        , event :: !(MVar ())
        , tables :: !(Set Text)
        -- | Structured QueryBuilder reads, or a conservative whole-table
        -- dependency when raw SQL or another unknown read touched the table.
        , readDependencies :: !(Map.Map Text AutoRefreshReadDependency)
        , lastResponse :: !LByteString
        , lastPing :: !UTCTime
        }

-- | Read dependencies for a session. Sessions constructed through the legacy
-- constructor conservatively depend on every touched table.
getAutoRefreshReadDependencies :: AutoRefreshSession -> Map.Map Text AutoRefreshReadDependency
getAutoRefreshReadDependencies TrackedAutoRefreshSession { readDependencies } = readDependencies
getAutoRefreshReadDependencies AutoRefreshSession { tables } =
    Map.fromSet (const AutoRefreshWholeTable) tables

-- | Replace a session's dependencies and upgrade legacy sessions to the
-- tracked representation.
setAutoRefreshReadDependencies :: Map.Map Text AutoRefreshReadDependency -> AutoRefreshSession -> AutoRefreshSession
setAutoRefreshReadDependencies readDependencies session = case session of
    TrackedAutoRefreshSession {} -> session
        { tables = Map.keysSet readDependencies
        , readDependencies
        }
    AutoRefreshSession { id, renderView, event, lastResponse, lastPing } ->
        TrackedAutoRefreshSession
            { id
            , renderView
            , event
            , tables = Map.keysSet readDependencies
            , readDependencies
            , lastResponse
            , lastPing
            }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set Text)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }
