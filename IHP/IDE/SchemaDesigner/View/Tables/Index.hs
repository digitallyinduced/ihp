module IHP.IDE.SchemaDesigner.View.Tables.Index where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data IndexView = IndexView
    { statements :: [Statement]
    }

instance View IndexView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html IndexView { .. } = [hsx|
        {renderFlashMessages}
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) Nothing}
        </div>
    |]