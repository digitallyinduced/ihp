module TurboHaskell.IDE.SchemaDesigner.View.Tables.Show where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data ShowView = ShowView
    { statements :: [Statement]
    , name :: Text
    , generatedHaskellCode :: Text
    , table :: Statement
    }

instance View ShowView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html ShowView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just name)}
            {renderColumnSelector name (zip [0..] columns) statements}
        </div>

        <pre class="generated-haskell-code"><code>{generatedHaskellCode}</code></pre>
    |]
        where
            columns = get #columns table