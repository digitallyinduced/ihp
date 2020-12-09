{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import IHP.Prelude
import IHP.Controller.RequestContext
import Control.Concurrent.MVar (MVar)
import Data.Set (Set)

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: UUID }
data AutoRefreshSession = AutoRefreshSession
        { id :: UUID
        -- | A callback to rerun an action within a given request context
        , renderView :: RequestContext -> IO ()
        -- | MVar that is filled whenever some table changed
        , event :: MVar ()
        -- | All tables this auto refresh session watches
        , tables :: Set ByteString
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: UTCTime
        }

data AutoRefreshServer = AutoRefreshServer { sessions :: [AutoRefreshSession], subscribedTables :: Set ByteString }

newAutoRefreshServer :: AutoRefreshServer
newAutoRefreshServer = AutoRefreshServer { sessions = [], subscribedTables = mempty }