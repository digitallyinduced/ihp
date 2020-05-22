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
    html ShowEnumView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <div class="d-flex justify-content-end col">
                <form class="p-2" action={pathTo DumpDbAction}>
                    <button type="submit" class="btn btn-primary">Dump DB</button>
                </form>
                <form class="p-2" style="padding-right: 0 !important;" action={pathTo PushToDbAction}>
                    <button type="submit" class="btn btn-primary">Push to DB</button>
                </form>
            </div>
        </div>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just name)}
                {renderEnumSelector name (zip [0..] values)}
            </div>
        </div>
    |]
        where
            table = findEnumByName name statements
            values = maybe [] (get #values) table
