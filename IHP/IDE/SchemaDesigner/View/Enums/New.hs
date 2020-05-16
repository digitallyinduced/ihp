module IHP.IDE.SchemaDesigner.View.Enums.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data NewEnumView = NewEnumView { statements :: [Statement] }

instance View NewEnumView ViewContext where
    html NewEnumView { .. } = [hsx|
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
                <form method="POST" action={CreateEnumAction}>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumName" type="text" class="form-control" autofocus="autofocus"/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Enum</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo TablesAction
            modalTitle = "New Enum"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
