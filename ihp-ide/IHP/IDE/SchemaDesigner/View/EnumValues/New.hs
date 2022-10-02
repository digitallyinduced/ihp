module IHP.IDE.SchemaDesigner.View.EnumValues.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data NewEnumValueView = NewEnumValueView
    { statements :: [Statement]
    , enumName :: Text
    }

instance View NewEnumValueView where
    html NewEnumValueView { .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just enumName)}
            {renderEnumSelector enumName (zip [0..] values)}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            table = findStatementByName enumName statements
            values = maybe [] (get #values) table

            modalContent = [hsx|
                <form method="POST" action={CreateEnumValueAction}>
                    <input type="hidden" name="enumName" value={enumName}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumValueName" type="text" class="form-control" autofocus="autofocus"/>
                            <div class="invalid-feedback">It's highly recommended to use snake case / underscores for naming your enum values</div>
                            <small class="text-muted">
                                Use underscores instead of camel case. E.g.: <code>public_user</code>, <code>in_progress</code>, <code>currency_eur</code>
                            </small>
                        </div>
                    </div>

                    <div class="text-right">
                        <input type="submit" name="submit" value="Save" class="btn btn-secondary mr-2">
                        <input type="submit" name="submit" value="Save & Add Another" class="btn btn-primary">
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowEnumAction { enumName }
            modalTitle = "New Enum Value"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
