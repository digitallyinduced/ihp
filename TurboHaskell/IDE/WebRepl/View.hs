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
        <script src="/repl.js" type="text/javascript"></script> 
        <div class="container h-100">
            <div style="width: 80%; min-height: 60%; background-color: black; color: white;">
                getOutputStream
            </div>
            <form>
                <input type="text" name="replInput" oninput="parseInput(this.value)"  style="width: 80%; height: 1rem;background-color: black; border: 1px solid; border-color: white; color: white;"/>
                <button type="submit" style="visibility: hidden;"/>
            </form>
        </div>
    |]