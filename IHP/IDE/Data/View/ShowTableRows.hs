module IHP.IDE.Data.View.ShowTableRows where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.Layout
import Data.Maybe

data ShowTableRowsView = ShowTableRowsView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    }

instance View ShowTableRowsView ViewContext where
    html ShowTableRowsView { .. } = [hsx|
        <div class="container pt-5">
            {customQuery ""}
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows rows tableBody tableName}
                </div>
            </div>
        </div>
    |]
        where

            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr oncontextmenu={"showContextMenu('" <> contextMenuId <> "');"}>{forEach fields (renderField id)}</tr>
            <div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
                <a href={EditRowAction tableName id}>Edit Row</a>
                <a href={DeleteEntryAction id tableName} class="js-delete">Delete Row</a>
                <div></div>
                <a href={NewRowAction tableName}>Add Row</a>
            </div>|]
                where
                    contextMenuId = "context-menu-column-" <> tshow id
                    id = (cs (fromMaybe "" (get #fieldValue (fromJust (headMay fields)))))
            renderField id DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) id}>{fieldValue}</a></span></td>|]

            columnNames = map (get #fieldName) (fromMaybe [] (head rows))