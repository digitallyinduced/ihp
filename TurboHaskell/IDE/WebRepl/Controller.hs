module TurboHaskell.IDE.WebRepl.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.WebRepl.View
import TurboHaskell.IDE.WebRepl.Types
import qualified System.Process as Process

instance Controller WebReplController where
    action ReplAction = do
        render IndexView