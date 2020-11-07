module IHP.IDE.Data.View.ShowTableRows where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
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
    , primaryKeyFields :: [Text]
    }

instance View ShowTableRowsView where
    html ShowTableRowsView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh" oncontextmenu="showContextMenu('context-menu-data-root')">
                    {renderRows rows tableBody tableName}
                </div>
            </div>
            {customQuery ""}
        </div>
        <div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-data-root">
            <a href={NewRowAction tableName}>Add Row</a>
        </div>
    |]
        where

            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{forEach fields (renderField primaryKey)}</tr>
            <div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
                <a href={EditRowAction tableName primaryKey}>Edit Row</a>
                <a href={DeleteEntryAction primaryKey tableName} class="js-delete">Delete Row</a>
                <div></div>
                <a href={NewRowAction tableName}>Add Row</a>
            </div>|]
                where
                    contextMenuId = "context-menu-column-" <> tshow primaryKey
                    primaryKey = intercalate "---" . map (cs . fromMaybe "" . get #fieldValue) $ filter ((`elem` primaryKeyFields) . cs . get #fieldName) fields
            renderField primaryKey DynamicField { .. }
                | fieldName == "id" = [hsx|<td><span data-fieldname={fieldName}><a class="no-link border rounded p-1" href={EditRowValueAction tableName (cs fieldName) primaryKey}>{renderId (sqlValueToText fieldValue)}</a></span></td>|]
                | isBoolField fieldName tableCols && not (isNothing fieldValue) = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName primaryKey} checked={sqlValueToText fieldValue == "t"} /></span></td>|]
                | otherwise = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) primaryKey}>{sqlValueToText fieldValue}</a></span></td>|]

            columnNames = map (get #fieldName) (fromMaybe [] (head rows))

            onClick tableName fieldName primaryKey = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) primaryKey)) <> ")"
