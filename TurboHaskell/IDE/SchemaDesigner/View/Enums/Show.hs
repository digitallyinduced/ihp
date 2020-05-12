module TurboHaskell.IDE.SchemaDesigner.View.Enums.Show where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data ShowEnumView = ShowEnumView
    { statements :: [Statement]
    , name :: Text
    }

instance View ShowEnumView ViewContext where
    html ShowEnumView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just name)}
                {renderEnumSelector name (zip [0..] values)}
            </div>
        </div>
    |]
        where
            table = findEnumByName name statements
            values = maybe [] (get #values) table
