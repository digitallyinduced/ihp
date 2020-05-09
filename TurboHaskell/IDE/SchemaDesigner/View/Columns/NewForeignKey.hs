module TurboHaskell.IDE.SchemaDesigner.View.Columns.NewForeignKey where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data NewForeignKeyView = NewForeignKeyView
    { statements :: [Statement]
    , tableName :: Text
    , columnName :: Text
    , generatedHaskellCode :: Text
    , tableNames :: [Text]
    }

instance View NewForeignKeyView ViewContext where
    html NewForeignKeyView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just tableName)}
                {renderColumnSelector tableName  (zip [0..] columns) statements}
            </div>

            <pre class="generated-haskell-code"><code>{generatedHaskellCode}</code></pre>
        </div>
        {Just modal}
    |]
        where
            table = findTableByName tableName statements
            columns = maybe [] (get #columns) table

            modalContent = [hsx|
                <form method="POST" action={CreateForeignKeyAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="columnName" value={columnName}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Reference Table:</label>
                        <div class="col-sm-10">
                            <select name="referenceTable" class="form-control select2" autofocus="autofocus">
                                {forEach tableNames renderTableNameSelector}
                            </select>
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="constraintName" type="text" class="form-control" value={tableName <> "_ref_" <> columnName}/>
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">On Delete:</label>
                        <div class="col-sm-10">
                            <select name="onDelete" class="form-control select2">
                                {onDeleteSelector "NoAction"}
                                {onDeleteSelector "Restrict"}
                                {onDeleteSelector "SetNull"}
                                {onDeleteSelector "Cascade"}
                            </select>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Add Constraint</button>
                    </div>
                </form>
                {select2}
            |]
                where
                    onDeleteSelector option = if option == "NoAction"
                        then preEscapedToHtml [plain|<option selected>#{option}</option>|]
                        else preEscapedToHtml [plain|<option>#{option}</option>|]
                    renderTableNameSelector tableName = [hsx|<option>{tableName}</option>|]
                    select2 = preEscapedToHtml [plain|
                        <script>
                            $('.select2').select2();
                        </script>
                    |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "New Foreign Key Constraint"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
            