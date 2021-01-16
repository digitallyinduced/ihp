module IHP.IDE.SchemaDesigner.View.Schema.Error where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout

data ErrorView = ErrorView
    { error :: ByteString
    }

instance View ErrorView where
    html ErrorView { .. } = [hsx|
        <div class="bg-white visual-error">
            <pre>{error}</pre>
        </div>
    |]
