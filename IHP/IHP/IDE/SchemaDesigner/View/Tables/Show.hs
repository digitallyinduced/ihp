module IHP.IDE.SchemaDesigner.View.Tables.Show where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout

data ShowView = ShowView
    { statements :: [Statement]
    , name :: Text
    , table :: Statement
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {renderFlashMessages}
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just name)}
            {renderColumnSelector name (zip [0..] columns) statements}
        </div>
        {migrationStatus}
    |]
        where
            columns = get #columns (unsafeGetCreateTable table)
