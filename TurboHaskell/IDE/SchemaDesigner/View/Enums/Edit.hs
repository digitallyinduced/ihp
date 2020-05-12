module TurboHaskell.IDE.SchemaDesigner.View.Enums.Edit where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data EditEnumView = EditEnumView
    { statements :: [Statement]
    , enumName :: Text
    , enumId :: Int
    }

instance View EditEnumView ViewContext where
    html EditEnumView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
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
