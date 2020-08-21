module IHP.IDE.Data.View.ShowQuery where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.Layout

data ShowQueryView = ShowQueryView
    { rows :: [[DynamicField]]
    , query :: Text
    }

instance View ShowQueryView ViewContext where
    html ShowQueryView { .. } = [hsx|
        <div class="container pt-5">
            <div class="row no-gutters bg-white">
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows}
                </div>
            </div>
            {customQuery query}
        </div>
    |]
        where
            renderRows = [hsx|
                <table class="table table-sm table-hover table-striped data-rows-table">
                    {tableHead}
                    {tableBody}
                </table>
            |]

            tableHead = [hsx|<thead><tr>{forEach columnNames renderColumnHead}</tr></thead>|]
            renderColumnHead name = [hsx|<th>{name}</th>|]

            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr>{forEach fields renderField}</tr>|]
            renderField DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}>{fieldValue}</span></td>|]

            columnNames = map (get #fieldName) (fromMaybe [] (head rows))