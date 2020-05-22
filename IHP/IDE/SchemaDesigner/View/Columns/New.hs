module IHP.IDE.SchemaDesigner.View.Columns.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout
import qualified Text.Countable as Countable

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    , primaryKeyExists :: Bool
    , tableNames :: [Text]
    }

instance View NewColumnView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html NewColumnView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName  (zip [0..] columns) statements}
        </div>
        {Just modal}
    |]
        where
            table = findTableByName tableName statements
            columns = maybe [] (get #columns) table

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
                        <select id="typeSelector" name="columnType" class="form-control select2-simple">
                            <option value="TEXT">Text</option>
                            <option value="INT">Int</option>
                            <option value="UUID">UUID</option>
                            <option value="BOOLEAN">Bool</option>
                            <option value="TIMESTAMP WITH TIME ZONE">Timestamp</option>
                            <option value="REAL">Float</option>
                            <option value="DOUBLE PRECISION">Double</option>
                        </select>

                        <div class="mt-1 text-muted">
                            {generateReferenceCheckboxes}
                            <label class="mx-2" style="font-size: 12px">
                                <input id="allowNull" type="checkbox" name="allowNull" class="mr-1"/>Nullable
                            </label>
                            <label class="mx-2" style="font-size: 12px">
                                <input type="checkbox" name="isUnique" class="mr-1"/>Unique
                            </label>
                            {primaryKeyCheckbox}
                            
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
                    primaryKeyCheckbox = if primaryKeyExists
                        then mempty
                        else [hsx|<label class="mx-2" style="font-size: 12px">
                            <input type="checkbox" name="primaryKey" class="mr-1"/>Primary Key  
                        </label>|]
                    defaultSelector = preEscapedToHtml [plain|
                        <select id="defaultSelector" name="defaultValue" class="form-control select2">
                            <option value="NODEFAULT">no default</option>
                            <option value="EMPTY">''</option>
                        </select>
                    |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "New Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
