module IHP.IDE.Data.View.ShowDatabase where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data ShowDatabaseView = ShowDatabaseView

instance View ShowDatabaseView ViewContext where
    html ShowDatabaseView = [hsx|
        <div class="container">
            <div class="alert alert-primary mt-5">
                This feature is not ready yet. Use <code>make psql</code> from the terminal to access the database.
            </div>
        </div>
    |]
