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
import Network.Wai (Request)

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: !UUID }
data AutoRefreshSession = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView :: !(Request -> Respond -> IO ())
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set ByteString)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set ByteString)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }
