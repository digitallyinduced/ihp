module IHP.IDE.Data.View.EditRow where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.Layout
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString as BS

data EditRowView = EditRowView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    , tableCols :: [ColumnDefinition]
    , rowValues :: [DynamicField]
    , primaryKeyFields :: [Text]
    , targetPrimaryKey :: Text
    }

instance View EditRowView where
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
        {renderModal modal}
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
                <form method="POST" action={UpdateRowAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    {forEach (zip tableCols rowValues) renderFormField}
                    {forEach (zip primaryKeyFields (T.splitOn "---" targetPrimaryKey)) renderPrimaryKeyInput}
                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Row</button>
                    </div>
                </form>
            |]
            modalFooter = mempty
            modalCloseUrl = pathTo ShowTableRowsAction { tableName }
            modalTitle = "Edit Row"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

            renderPrimaryKeyInput (primaryKeyField, primaryKeyValue) = [hsx|<input type="hidden" name={primaryKeyField <> "-pk"} value={primaryKeyValue}>|]

            renderFormField :: (ColumnDefinition, DynamicField) -> Html
            renderFormField (def, val) = [hsx|
                    <div class="form-group">
                        <label class="row-form">{get #columnName def}</label>
                        <span style="float:right;">
                            <a class="text-muted row-form">{get #columnType def}</a>
                        </span>

                        <div class="input-group">
                            {renderInputMethod (def, val)}
                        </div>
                    </div>|]

            onClick tableName fieldName id = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) id)) <> ")"
            renderInputMethod :: (ColumnDefinition, DynamicField) -> Html
            renderInputMethod (def, val) | (get #columnType def) == "boolean" && isNothing (get #fieldValue val) = [hsx|
                            {isBooleanParam True def}
                            <input
                                id={get #columnName def <> "-alt"}
                                type="text"
                                name={get #columnName def}
                                class="form-control text-monospace text-secondary bg-light"
                                value="NULL"
                                />
                            <div class="form-control" id={get #columnName def <> "-boxcontainer"}>
                                <input
                                    id={get #columnName def <> "-input"}
                                    type="checkbox"
                                    class="d-none"
                                    name={get #columnName def <> "-inactive"}
                                    checked={(value val) == "t"}
                                    />
                            </div>
                            <input
                                id={get #columnName def <> "-hidden"}
                                type="hidden"
                                name={get #columnName def}
                                value={inputValue False}
                                />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField def "DEFAULT" "true"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField def "NULL" "true"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName def <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName def <> "_"}
                                            checked={True}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName def <> "', this, true)"}
                                            />
                                        <label class="form-check-label" for={get #columnName def <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName def <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            </div>
                                |]
            renderInputMethod (def, val) | (get #columnType def) == "boolean" = [hsx|
                            {isBooleanParam True def}
                            <input
                                id={get #columnName def <> "-alt"}
                                type="text"
                                name={get #columnName def <> "-inactive"}
                                class="form-control text-monospace text-secondary bg-light d-none"
                                />
                            <div class="form-control" id={get #columnName def <> "-boxcontainer"}>
                                <input
                                    id={get #columnName def <> "-input"}
                                    type="checkbox"
                                    name={get #columnName def}
                                    checked={(value val) == "t"}
                                    />
                            </div>
                            <input
                                id={get #columnName def <> "-hidden"}
                                type="hidden"
                                name={get #columnName def}
                                value={inputValue False}
                                />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField def "DEFAULT" "true"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField def "NULL" "true"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName def <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName def <> "_"}
                                            checked={isSqlFunction (getColDefaultValue def)}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName def <> "', this, true)"}
                                            />
                                        <label class="form-check-label" for={get #columnName def <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName def <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            </div>
                                |]
            renderInputMethod (def, val) | sqlValueToText (get #fieldValue val) == "NULL" = [hsx|
                            {isBooleanParam False def}
                            <input
                                id={get #columnName def <> "-alt"}
                                type="text"
                                name={get #columnName def}
                                class="form-control text-monospace text-secondary bg-light"
                                value={sqlValueToText (get #fieldValue val)}
                            />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField def "DEFAULT" "false"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField def "NULL" "false"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName def <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName def <> "_"}
                                            checked={True}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName def <> "', this)"}
                                            />
                                        <label class="form-check-label" for={get #columnName def <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName def <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            </div>|]
            renderInputMethod (def, val) = [hsx|
                            {isBooleanParam False def}
                            <input
                                id={get #columnName def <> "-input"}
                                type="text"
                                name={get #columnName def}
                                class={classes ["form-control", ("text-monospace text-secondary bg-light", isSqlFunction_ (value val))]}
                                value={value val}
                                oninput={"stopSqlModeOnInput('" <> get #columnName def <> "')"}
                                />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField def "DEFAULT" "false"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField def "NULL" "false"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName def <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName def <> "_"}
                                            checked={isSqlFunction_ (value val)}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName def <> "', this)"}
                                            />
                                        <label class="form-check-label" for={get #columnName def <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName def <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            </div>|]

value val = fromMaybe BS.empty (get #fieldValue val)
