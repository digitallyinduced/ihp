module IHP.ApplicationContext where

import IHP.Prelude
import Network.Wai.Session (Session)
import IHP.AutoRefresh.Types (AutoRefreshServer)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.PGListener (PGListener)

data ApplicationContext = ApplicationContext
    { modelContext :: !ModelContext
    , autoRefreshServer :: !(IORef AutoRefreshServer)
    , frameworkConfig :: !FrameworkConfig
    , pgListener :: PGListener
    }
