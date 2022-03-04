module IHP.IDE.SchemaDesigner.View.Columns.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.View.Columns.Edit (typeSelector)

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    , tableNames :: [Text]
    , enumNames :: [Text]
    }

instance View NewColumnView where
    html NewColumnView { .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName  (zip [0..] columns) statements}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            table = findStatementByName tableName statements
            columns = maybe [] (get #columns . unsafeGetCreateTable) table

            modalContent = [hsx|
                {renderFlashMessages}
                <form method="POST" action={CreateColumnAction} id="new-column">
                    <input type="hidden" name="tableName" value={tableName}/>

                    <div class="form-group">
                        <input
                            id="colName"
                            name="name"
                            type="text"
                            class="form-control"
                            autofocus="autofocus"
                            placeholder="Name"
                            data-table-name-singular={singularize tableName}
                            />
                    </div>

                    <div class="form-group">
                        {typeSelector Nothing enumNames}

                        <div class="mt-1 text-muted d-flex" id="column-options">
                            {generateReferenceCheckboxes}

                            <div class="custom-control custom-checkbox">
                                <input id="allowNull" type="checkbox" name="allowNull" class="mr-1 custom-control-input"/>
                                <label class="custom-control-label mr-2" for="allowNull">
                                    Nullable
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox">
                                <input type="checkbox" id="isUnique" name="isUnique" class="mr-1 custom-control-input"/>
                                <label class="mx-2 custom-control-label" for="isUnique">
                                    Unique
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox">
                                <input type="checkbox" name="primaryKey" id="primaryKey" class="mr-1 custom-control-input"/>
                                <label class="mx-2 custom-control-label" for="primaryKey">
                                    Primary Key
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox">
                                <input id="isArray" type="checkbox" name="isArray" class="mr-1 custom-control-input"/>
                                <label class="mx-2 custom-control-label" for="isArray">
                                    Array Type
                                </label>
                            </div>
                        </div>
                    </div>

                    <div class="form-group">
                        {defaultSelector}
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Column</button>
                    </div>

                    <input type="hidden" name="primaryKey" value={inputValue False}/>
                    <input type="hidden" name="allowNull" value={inputValue False}/>
                    <input type="hidden" name="isUnique" value={inputValue False}/>
                    <input type="hidden" name="isArray" value={inputValue False}/>
                    <input type="hidden" name="isReference" value={inputValue False}/>
                    <input type="hidden" name="referenceTable" value=""/>
                </form>
            |]
                where
                    generateReferenceCheckboxes = [hsx|<span id="checkboxes">{forEach tableNames checkbox}</span>|]
                        where checkbox tableName = [hsx|
                                    <label class="mx-2 ref" style="font-size: 12px; display: none;" data-attribute={(singularize tableName) <> "_id"} data-table={tableName}>
                                        <input id="reference" type="checkbox" name="isReference" class="mr-1"/>
                                        <a id="refText">References {tableName}</a>
                                    </label>|]
                    defaultSelector = [hsx|
                        <select id="defaultSelector" name="defaultValue" class="form-control select2">
                            <option value="" selected={True}>no default</option>
                            <option value="''">""</option>
                        </select>
                    |]
            modalFooter = mempty
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "New Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
