module IHP.IDE.Data.View.NewRow where

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

data NewRowView = NewRowView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    , tableCols :: [ColumnDefinition]
    }

instance View NewRowView ViewContext where
    html NewRowView { .. } = [hsx|
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
        {fillFieldScript}
    |]
        where
            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr>{forEach fields (renderField id)}</tr>|]
                where
                    id = (cs (fromMaybe "" (get #fieldValue (fromJust (headMay fields)))))
            renderField id DynamicField { .. } | fieldName == "id" = [hsx|<td><span data-fieldname={fieldName}><a class="no-link border rounded p-1" href={EditRowValueAction tableName (cs fieldName) id}>{renderId (sqlValueToText fieldValue)}</a></span></td>|]
            renderField id DynamicField { .. } | isBoolField fieldName tableCols && not (isNothing fieldValue) = [hsx|<td><span data-fieldname={fieldName}><input type="checkbox" onclick={onClick tableName fieldName id} checked={sqlValueToText fieldValue == "t"} /></span></td>|]
            renderField id DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) id}>{sqlValueToText fieldValue}</a></span></td>|]

            modalContent = [hsx|
                <form method="POST" action={CreateRowAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    {forEach tableCols renderFormField}
                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Add Row</button>
                    </div>
                </form>
            |]
            modalFooter = mempty
            modalCloseUrl = pathTo ShowTableRowsAction { tableName }
            modalTitle = "Add Row"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

            renderFormField col = [hsx|
                    <div class="form-group">
                        <label class="row-form">{get #columnName col}</label>
                        <span style="float:right;">
                            <a class="text-muted row-form">{get #columnType col}</a>
                        </span>

                        <div class="input-group">
                            <input
                                id={get #columnName col <> "-input"}
                                type="text"
                                name={get #columnName col}
                                class={classes ["form-control", ("text-monospace text-secondary bg-light", isSqlFunction (getColDefaultValue col))]}
                                value={getColDefaultValue col}
                                />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField col "DEFAULT"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField col "NULL"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName col <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName col <> "_"}
                                            checked={isSqlFunction (getColDefaultValue col)}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName col  <> "', this)"}
                                            />
                                        <label class="form-check-label" for={get #columnName col <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName col <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            </div>
                        </div>
                    </div>|]

            onClick tableName fieldName id = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) id)) <> ")"

            fillField col value = "fillField('" <> get #columnName col <> "', '" <> value <> "');"
            fillFieldScript = [hsx|<script>
                function fillField(id, value) {
                    var inputField = document.getElementById(id + "-input");
                    var sqlModeBox = document.getElementById(id + "-sqlbox");
                    inputField.value = value;
                    sqlModeBox.checked = true;
                    setSqlMode(id, true);
                }

                function sqlModeCheckbox(id, checkbox) {
                    setSqlMode(id, checkbox.checked);
                }

                function setSqlMode(id, sqlMode) {
                    var inputField = document.getElementById(id + "-input");
                    if (sqlMode) {
                        inputField.className = "form-control text-monospace text-secondary bg-light"
                    } else {
                        inputField.className = "form-control";
                    }
                }
            </script>|]

getColDefaultValue :: ColumnDefinition -> Text
getColDefaultValue ColumnDefinition { columnDefault, isNullable } = case columnDefault of
        Just value -> value
        Nothing -> if isNullable
            then "NULL"
            else ""