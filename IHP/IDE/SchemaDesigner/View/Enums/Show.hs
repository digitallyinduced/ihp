module IHP.IDE.SchemaDesigner.View.Enums.Show where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout

data ShowEnumView = ShowEnumView
    { statements :: [Statement]
    , name :: Text
    }


instance View ShowEnumView where
    html ShowEnumView { .. } = [hsx|
        {renderFlashMessages}
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just name)}
            {renderEnumSelector name (zip [0..] values)}
        </div>
    |]
        where
            table = findStatementByName name statements
            values = maybe [] (get #values) table
