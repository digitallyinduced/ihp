module IHP.IDE.SchemaDesigner.View.Enums.Show where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data ShowEnumView = ShowEnumView
    { statements :: [Statement]
    , name :: Text
    }


instance View ShowEnumView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html ShowEnumView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just name)}
            {renderEnumSelector name (zip [0..] values)}
        </div>
    |]
        where
            table = findEnumByName name statements
            values = maybe [] (get #values) table
