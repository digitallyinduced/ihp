module IHP.IDE.SchemaDesigner.View.Tables.Show where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data ShowView = ShowView
    { statements :: [Statement]
    , name :: Text
    , table :: Statement
    }

instance View ShowView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html ShowView { .. } = [hsx|
        {renderFlashMessages}
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just name)}
            {renderColumnSelector name (zip [0..] columns) statements}
        </div>
    |]
        where
            columns = get #columns table