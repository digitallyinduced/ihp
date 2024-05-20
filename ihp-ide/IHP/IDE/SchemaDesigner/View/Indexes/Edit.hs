module IHP.IDE.SchemaDesigner.View.Indexes.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.Compiler as SqlCompiler
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data EditIndexView = EditIndexView
    { statements :: [Statement]
    , indexName :: Text
    , tableName :: Text
    }

instance View EditIndexView where
    html EditIndexView { .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName  (zip [0..] columns) statements}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            index :: Statement
            (Just index) = statements |> find \case
                    CreateIndex { indexName = name } | name == indexName -> True
                    otherwise -> False

            table :: Maybe Statement
            table = findStatementByName tableName statements

            columns = maybe [] ((.columns) . unsafeGetCreateTable) table

            indexColumns = index.columns
                    |> map SqlCompiler.compileIndexColumn
                    |> intercalate ", "

            modalContent = [hsx|
                <form method="POST" action={UpdateIndexAction tableName indexName}>
                    <input type="hidden" name="indexName" value={indexName}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="newIndexName" type="text" class="form-control" value={indexName}/>
                        </div>
                    </div>
                    
                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Column:</label>
                        <div class="col-sm-10">
                            <input name="indexColumns" type="text" class="form-control" value={indexColumns}/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Update Index</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "Edit Index"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
