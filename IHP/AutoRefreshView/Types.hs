{-|
Module: IHP.AutoRefreshView.Types
Description: Types & Data Structures for IHP AutoRefreshView
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefreshView.Types where

import IHP.Prelude
import IHP.Controller.RequestContext
import Control.Concurrent.MVar (MVar)

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: UUID }
data AutoRefreshSession = AutoRefreshSession { id :: UUID, renderView :: RequestContext -> IO (), event :: MVar () }

