module IHP.ApplicationContext where

import IHP.Prelude
import IHP.AutoRefresh.Types (AutoRefreshServer)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.PGListener (PGListener)

data ApplicationContext = ApplicationContext
    { modelContext :: !ModelContext
    , frameworkConfig :: !FrameworkConfig
    , pgListener :: PGListener
    }
