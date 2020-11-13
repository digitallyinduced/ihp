module IHP.IDE.SchemaDesigner.View.Enums.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout

data EditEnumView = EditEnumView
    { statements :: [Statement]
    , enumName :: Text
    , enumId :: Int
    }

instance View EditEnumView where
    html EditEnumView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) Nothing}
        </div>
        {renderModal modal}
    |]
        where
            modalContent = [hsx|
                <form method="POST" action={UpdateEnumAction}>
                    <input type="hidden" name="enumId" value={tshow enumId}/>
                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input id="nameInput" name="enumName" type="text" class="form-control" autofocus="autofocus" value={enumName}/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Table</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo TablesAction
            modalTitle = "Edit Enum"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
