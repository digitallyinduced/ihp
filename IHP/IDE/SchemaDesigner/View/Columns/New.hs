module IHP.IDE.SchemaDesigner.View.Columns.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import qualified Text.Countable as Countable
import IHP.IDE.SchemaDesigner.View.Columns.Edit (typeSelector)

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    , tableNames :: [Text]
    , enumNames :: [Text]
    }

instance View NewColumnView where
    beforeRender view = setLayout schemaDesignerLayout

    html NewColumnView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName  (zip [0..] columns) statements}
        </div>
        {renderModal modal}
    |]
        where
            table = findStatementByName tableName statements
            columns = maybe [] (get #columns . unsafeGetCreateTable) table

            modalContent = [hsx|
                <form method="POST" action={CreateColumnAction} id="new-column">
                    <input type="hidden" name="tableName" value={tableName}/>

                    <div class="form-group">
                        <input
                            id="colName"
                            name="name"
                            type="text"
                            class="form-control"
                            autofocus="autofocus"
                            placeholder="Name:"
                            />
                    </div>

                    <div class="form-group">
                        {typeSelector Nothing enumNames}

                        <div class="mt-1 text-muted">
                            {generateReferenceCheckboxes}
                            <label class="mx-2" style="font-size: 12px">
                                <input id="allowNull" type="checkbox" name="allowNull" class="mr-1"/>Nullable
                            </label>
                            <label class="mx-2" style="font-size: 12px">
                                <input type="checkbox" name="isUnique" class="mr-1"/>Unique
                            </label>
                            <label class="mx-2" style="font-size: 12px">
                                <input type="checkbox" name="primaryKey" class="mr-1"/>Primary Key
                            </label>
                            <label class="ml-1" style="font-size: 12px">
                                <input type="checkbox" name="isArray" class="mr-1"/>Array Type
                            </label>
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
                                    <label class="mx-2 ref" style="font-size: 12px; display: none;" data-attribute={(Countable.singularize tableName) <> "_id"} data-table={tableName}>
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
