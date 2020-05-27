module IHP.IDE.Data.View.Layout (customQuery, tableHead, renderColumnHead, columnNames, renderRows) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout

customQuery :: Text -> Html
customQuery input = [hsx|<div class="p-2 rounded mb-2" style="background-color: #002B36;"><div id="queryInput">{input}</div></div>|]

tableHead :: [[DynamicField]] -> Text -> Html
tableHead rows tableName = [hsx|<thead><tr>{forEach (columnNames rows) renderColumnHead}
        <td><a href={NewRowAction tableName} class="btn btn-primary btn-sm">Add</a></td>
    </tr></thead>|]
renderColumnHead name = [hsx|<th>{name}</th>|]

columnNames rows = map (get #fieldName) (fromMaybe [] (head rows))

renderRows :: [[DynamicField]] -> Html -> Text -> Html
renderRows rows body tableName = [hsx|
    <table class="table table-sm table-hover table-striped data-rows-table">
        {tableHead rows tableName}
        {body}
    </table>
|]