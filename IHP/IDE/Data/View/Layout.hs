module IHP.IDE.Data.View.Layout (customQuery, tableHead, renderColumnHead, columnNames, renderRows, sqlValueToText, renderId, isBoolField) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types hiding (columnNames)
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout

customQuery :: Text -> Html
customQuery input = [hsx|<div class="p-2 rounded mt-2" style="background-color: #002B36;"><div id="queryInput" style="height:16px">{input}</div></div>|]

tableHead :: [[DynamicField]] -> Text -> Html
tableHead rows tableName = [hsx|<thead><tr>{forEach (columnNames rows) renderColumnHead}</tr></thead>|]
renderColumnHead name = [hsx|<th>{name}</th>|]

columnNames rows = map (get #fieldName) (fromMaybe [] (head rows))

renderRows :: [[DynamicField]] -> Html -> Text -> Html
renderRows rows body tableName = [hsx|
    <table class="table table-sm table-hover table-striped data-rows-table">
        {tableHead rows tableName}
        {body}
    </table>
|]

sqlValueToText :: Maybe ByteString -> Text
sqlValueToText (Just value) = cs value
sqlValueToText Nothing = "NULL"

renderId id = take 4 (cs id) <> ".." <> reverse (take 4 (reverse (cs id)))

isBoolField fieldName tableCols = case (find (\c -> get #columnName c == (cs fieldName)) tableCols) of
    Just columnDef -> (get #columnType columnDef) == "boolean"
    Nothing -> False