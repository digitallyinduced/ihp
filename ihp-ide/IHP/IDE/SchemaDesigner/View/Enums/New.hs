module IHP.IDE.SchemaDesigner.View.Enums.New where

import IHP.ViewPrelude
import IHP.Postgres.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data NewEnumView = NewEnumView { statements :: [Statement] }

instance View NewEnumView where
    html NewEnumView { .. } = [hsx|
        <div class="row g-0 bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) Nothing}
            {emptyColumnSelectorContainer}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            modalContent = [hsx|
                <form method="POST" action={CreateEnumAction}>

                    <div class="mb-3 row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input id="nameInput" name="enumName" type="text" class="form-control" autofocus="autofocus"/>
                        </div>
                    </div>

                    <div class="text-end">
                        <button type="submit" class="btn btn-primary">Create Enum</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo TablesAction
            modalTitle = "New Enum"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
