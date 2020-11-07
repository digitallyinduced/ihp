module IHP.IDE.Data.View.ShowDatabase where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.Data.View.Layout

data ShowDatabaseView = ShowDatabaseView {
        tableNames :: [Text]
    }

instance View ShowDatabaseView where
    html ShowDatabaseView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames ""}
            </div>
            {customQuery ""}
        </div>
    |]


renderTableSelector tableNames activeTableName = [hsx|
    <div class="col-2 object-selector">
        <div class="d-flex">
            <h5>Tables</h5>
        </div>
        {forEach tableNames renderTable}
        <div class="text-muted context-menu-notice">Right click to open context menu</div>
    </div>
|]
    where
        renderTable :: Text -> Html
        renderTable name = [hsx|
            <a href={ShowTableRowsAction name} class={classes [("object object-table w-100 context-table", True), ("active", name == activeTableName)]}>
                <div class="d-flex">
                    {name}
                </div>
            </a>|]
