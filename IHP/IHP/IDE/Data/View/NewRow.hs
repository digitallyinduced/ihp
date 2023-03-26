module IHP.IDE.Data.View.NewRow where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.Layout
import Data.Maybe
import qualified Data.Text as Text

data NewRowView = NewRowView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    , tableCols :: [ColumnDefinition]
    }

instance View NewRowView where
    html NewRowView { .. } = [hsx|
        <div class="h-100">
            {headerNav}
            <div class="h-100 row no-gutters">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows rows tableBody tableName}
                </div>
            </div>
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
                <form method="POST" action={CreateRowAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    {forEach tableCols renderFormField}
                    {renderFlashMessages}
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

                        <div class="d-flex">
                            {renderInputMethod col}
                        </div>
                    </div>|]

            onClick tableName fieldName id = "window.location.assign(" <> tshow (pathTo (ToggleBooleanFieldAction tableName (cs fieldName) id)) <> ")"
            renderInputMethod :: ColumnDefinition -> Html 
            renderInputMethod col | (get #columnType col) == "boolean" = [hsx|
                            {isBooleanParam True col}
                            <input
                                id={get #columnName col <> "-alt"}
                                type="text"
                                name={get #columnName col <> "-inactive"}
                                class="form-control text-monospace text-secondary d-none"
                                />
                            <div class="form-control" id={get #columnName col <> "-boxcontainer"}>
                                <input
                                    id={get #columnName col <> "-input"}
                                    type="checkbox"
                                    name={get #columnName col}
                                    checked={get #columnDefault col == Just "true"}
                                    />
                            </div>
                            <input
                                id={get #columnName col <> "-hidden"}
                                type="hidden"
                                name={get #columnName col}
                                value={inputValue False}
                                />
                            <div class="input-group-append">
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField col "DEFAULT" "true"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField col "NULL" "true"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName col <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName col <> "_"}
                                            checked={isSqlFunction (getColDefaultValue col)}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName col <> "', this, true)"}
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
                                |]
            renderInputMethod col = [hsx|
                                {isBooleanParam False col}
                                {if isForeignKeyColumn
                                        then select
                                        else theInput
                                }
                                <button class="btn dropdown-toggle" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"></button>
                                <div class="dropdown-menu dropdown-menu-right custom-menu menu-for-column shadow backdrop-blur">
                                    <a class="dropdown-item" data-value="DEFAULT" data-issql="True" onclick={fillField col "DEFAULT" "false"}>DEFAULT</a>
                                    <a class="dropdown-item" data-value="NULL" data-issql="True" onclick={fillField col "NULL" "false"}>NULL</a>
                                    <a class="dropdown-item">
                                        <input
                                            id={get #columnName col <> "-sqlbox"}
                                            type="checkbox"
                                            name={get #columnName col <> "_"}
                                            checked={isSqlFunction (getColDefaultValue col)}
                                            class="mr-1"
                                            onclick={"sqlModeCheckbox('" <> get #columnName col <> "', this, false)"}
                                            />
                                        <label class="form-check-label" for={get #columnName col <> "-sqlbox"}> Parse as SQL</label>
                                    </a>
                                    <input
                                        type="hidden"
                                        name={get #columnName col <> "_"}
                                        value={inputValue False}
                                        />
                                </div>
                            |]
                                where
                                    isForeignKeyColumn :: Bool
                                    isForeignKeyColumn = "_id" `Text.isSuffixOf` (get #columnName col)


                                    theInput :: Html
                                    theInput = [hsx|
                                        <input
                                            id={get #columnName col <> "-input"}
                                            type="text"
                                            name={get #columnName col}
                                            class={classes ["form-control", ("text-monospace", isSqlFunction (getColDefaultValue col)), ("is-foreign-key-column", isForeignKeyColumn)]}
                                            value={renderDefaultWithoutType (getColDefaultValue col)}
                                            oninput={"stopSqlModeOnInput('" <> get #columnName col <> "')"}
                                        />
                                    |]

                                    select :: Html
                                    select = [hsx|
                                        <select
                                            id={get #columnName col <> "-input"}
                                            name={get #columnName col}
                                            class={classes ["form-control", ("is-foreign-key-column", isForeignKeyColumn)]}
                                            value={renderDefaultWithoutType (getColDefaultValue col)}
                                            data-select-url={selectUrl}
                                        />
                                    |]

                                    selectUrl :: Text
                                    selectUrl = pathTo AutocompleteForeignKeyColumnAction { tableName, columnName = get #columnName col, term = "" }