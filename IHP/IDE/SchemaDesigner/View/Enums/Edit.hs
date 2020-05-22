module IHP.IDE.SchemaDesigner.View.Enums.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data EditEnumView = EditEnumView
    { statements :: [Statement]
    , enumName :: Text
    , enumId :: Int
    }

instance View EditEnumView ViewContext where
    html EditEnumView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <div class="d-flex justify-content-end col">
                <form class="p-2" action={pathTo DumpDbAction}>
                    <button type="submit" class="btn btn-primary">Dump DB</button>
                </form>
                <form class="p-2" style="padding-right: 0 !important;" action={pathTo PushToDbAction}>
                    <button type="submit" class="btn btn-primary">Push to DB</button>
                </form>
            </div>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) Nothing}
            </div>
        </div>
        {Just modal}
    |]
        where
            modalContent = [hsx|
                <form method="POST" action={UpdateEnumAction}>
                    <input type="hidden" name="enumId" value={tshow enumId}/>
                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumName" type="text" class="form-control" autofocus="autofocus" value={enumName}/>
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
