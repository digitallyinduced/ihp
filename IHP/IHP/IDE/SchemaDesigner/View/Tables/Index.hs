module IHP.IDE.SchemaDesigner.View.Tables.Index where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout

data IndexView = IndexView
    { statements :: [Statement]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderFlashMessages}
        {modal}
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) Nothing}
            {emptyColumnSelectorContainer}
        </div>
    |]