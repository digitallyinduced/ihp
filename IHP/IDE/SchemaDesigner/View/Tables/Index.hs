module IHP.IDE.SchemaDesigner.View.Tables.Index where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout

data IndexView = IndexView
    { statements :: [Statement]
    }

instance View IndexView where
    beforeRender view = setLayout schemaDesignerLayout

    html IndexView { .. } = [hsx|
        {renderFlashMessages}
        <div class="row no-gutters bg-white mb-4">
            {renderObjectSelector (zip [0..] statements) Nothing}
        </div>
    |]