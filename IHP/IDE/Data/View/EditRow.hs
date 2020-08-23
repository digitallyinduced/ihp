module IHP.IDE.Data.View.EditRow where

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

data EditRowView = EditRowView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    , tableCols :: [ColumnDefinition]
    , rowValues :: [DynamicField]
    , id :: Text
    }

instance View EditRowView ViewContext where
    html EditRowView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows rows tableBody tableName}
                </div>
            </div>
            {customQuery ""}
        </div>
        {Just modal}
    |]
        where
            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr>{forEach fields (renderField id)}</tr>|]
                where
                    id = (cs (fromMaybe "" (get #fieldValue (fromJust (headMay fields)))))
            renderField id DynamicField { .. } | fieldName == "id" = [hsx|<td><span data-fieldname={fieldName}><a class="no-link border rounded p-1" href={EditRowValueAction tableName (cs fieldName) id}>{renderId (cleanValue fieldValue)}</a></span></td>|]
            renderField id DynamicField { .. } | isBoolField (fieldName) && not (isNothing fieldValue) && cleanValue fieldValue == "t" = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName id} checked="checked" /></span></td>|]
            renderField id DynamicField { .. } | isBoolField (fieldName) && not (isNothing fieldValue) && cleanValue fieldValue == "f" = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName id}/></span></td>|]
            renderField id DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) id}>{cleanValue fieldValue}</a></span></td>|]


            modalContent = [hsx|
                <form method="POST" action={UpdateRowAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    {forEach (zip tableCols rowValues) renderFormField}
                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Row</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableRowsAction { tableName }
            modalTitle = "Edit Row"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

            renderFormField col = [hsx|
                    <div class="form-group">
                        <label class="row-form">{get #columnName (fst col)}</label>
                        <span style="float:right;">
                            <a class="text-muted row-form">{get #columnType (fst col)}</a>
                        </span>
                        
                        <input
                            type="text"
                            name={get #columnName (fst col)}
                            class="form-control"
                            value={"'" <> (fromMaybe "" (get #fieldValue (snd col))) <> "'"}
                            />
                    </div>|]

            isBoolField fieldName = case (find (\c -> get #columnName c == (cs fieldName)) tableCols) of
                Just columnDef -> if get #columnType columnDef == "boolean"
                    then True
                    else False
                Nothing -> False

            onClick tableName fieldName id = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) id)) <> ")"