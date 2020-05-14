module TurboHaskell.IDE.SchemaDesigner.View.Columns.New where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    , generatedHaskellCode :: Text
    , primaryKeyExists :: Bool
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
                            <label style="font-size: 12px">
                                <input id="allowNull" type="checkbox" name="allowNull" class="mr-2"/> Nullable
                            </label>
                            <label class="ml-1" style="font-size: 12px">
                                <input type="checkbox" name="isUnique" class="mr-2"/> Unique
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
                </form>
            |]
                where
                    primaryKeyCheckbox = if primaryKeyExists
                        then mempty
                        else [hsx|<label class="ml-1" style="font-size: 12px">
                            <input type="checkbox" name="primaryKey" class="mr-2"/> Primary Key  
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
