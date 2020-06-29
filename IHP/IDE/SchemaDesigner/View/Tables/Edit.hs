module IHP.IDE.SchemaDesigner.View.Tables.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data EditTableView = EditTableView
    { statements :: [Statement]
    , tableName :: Text
    , tableId :: Int
    }

instance View EditTableView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html EditTableView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) Nothing}
        </div>
        {Just modal}
    |]
        where
            modalContent = [hsx|
                <form method="POST" action={UpdateTableAction}>
                    <input type="hidden" name="tableId" value={tshow tableId}/>
                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input id="nameInput" name="tableName" type="text" class="form-control" autofocus="autofocus" value={tableName}/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Table</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo TablesAction
            modalTitle = "Edit Table"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
