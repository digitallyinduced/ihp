module TurboHaskell.IDE.WebRepl.View where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.WebRepl.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal

data IndexView = IndexView
    { 
    }


instance View IndexView ViewContext where
    html IndexView = [hsx|
        <div class="container h-100">
            <div style="width: 80%; min-height: 60%; background-color: black; color: white;"></div>
        </div>
    |]