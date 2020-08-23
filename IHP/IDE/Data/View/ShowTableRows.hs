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
    , tableCols :: [ColumnDefinition]
    }

instance View ShowTableRowsView ViewContext where
    html ShowTableRowsView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows rows tableBody tableName}
                </div>
            </div>
            {customQuery ""}
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
            renderField id DynamicField { .. } | fieldName == "id" = [hsx|<td><span data-fieldname={fieldName}><a class="no-link border rounded p-1" href={EditRowValueAction tableName (cs fieldName) id}>{renderId (cleanValue fieldValue)}</a></span></td>|]
            renderField id DynamicField { .. } | isBoolField (fieldName) && not (isNothing fieldValue) && cleanValue fieldValue == "t" = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName id} checked="checked" /></span></td>|]
            renderField id DynamicField { .. } | isBoolField (fieldName) && not (isNothing fieldValue) && cleanValue fieldValue == "f" = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName id}/></span></td>|]
            renderField id DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) id}>{cleanValue fieldValue}</a></span></td>|]

            columnNames = map (get #fieldName) (fromMaybe [] (head rows))

            
            isBoolField fieldName = case (find (\c -> get #columnName c == (cs fieldName)) tableCols) of
                Just columnDef -> if get #columnType columnDef == "boolean"
                    then True
                    else False
                Nothing -> False

            onClick tableName fieldName id = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) id)) <> ")"