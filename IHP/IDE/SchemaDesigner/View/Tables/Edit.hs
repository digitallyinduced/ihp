module IHP.IDE.SchemaDesigner.View.Tables.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout

data EditTableView = EditTableView
    { statements :: [Statement]
    , tableName :: Text
    , tableId :: Int
    }

instance View EditTableView where
    html EditTableView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) Nothing}
        </div>
        {renderModal modal}
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
