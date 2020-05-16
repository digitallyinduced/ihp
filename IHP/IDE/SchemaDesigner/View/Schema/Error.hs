module IHP.IDE.SchemaDesigner.View.Schema.Error where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data ErrorView = ErrorView
    { error :: ByteString
    }

instance View ErrorView ViewContext where
    html ErrorView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <div class="row no-gutters bg-white visual-error">
                <pre>{error}</pre>
            </div>
        </div>
    |]