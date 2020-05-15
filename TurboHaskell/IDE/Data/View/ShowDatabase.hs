module TurboHaskell.IDE.Data.View.ShowDatabase where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data ShowDatabaseView = ShowDatabaseView

instance View ShowDatabaseView ViewContext where
    html ShowDatabaseView = [hsx|
        <div class="container">
            <div class="alert alert-primary mt-5">
                This feature is not ready yet. Use <code>make psql</code> from the terminal to access the database.
            </div>
        </div>
    |]
