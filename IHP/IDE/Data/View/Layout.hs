module IHP.IDE.Data.View.Layout (customQuery, tableHead, renderColumnHead, columnNames, renderRows) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout

customQuery :: Html
customQuery = [hsx|
<form method="POST" action={pathTo ShowQueryAction}>
    <input class="form-control" type="text" name="query" placeholder="SELECT * FROM table"/>
    <button class="d-none" type="submit">Query</button>
</form>|]

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