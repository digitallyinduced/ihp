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
    }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters">
                {renderObjectSelector statements (Just name)}
                {renderColumnSelector name (zip [0..] columns)}
            </div>
        </div>
    |]
        where
            table = findTableByName name statements
            columns = maybe [] (get #columns) table
