module TurboHaskell.IDE.SchemaDesigner.View.EnumValues.New where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data NewEnumValueView = NewEnumValueView
    { statements :: [Statement]
    , enumName :: Text
    }

instance View NewEnumValueView ViewContext where
    html NewEnumValueView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just enumName)}
                {renderEnumSelector enumName (zip [0..] values)}
            </div>
        </div>
        {Just modal}
    |]
        where
            table = findEnumByName enumName statements
            values = maybe [] (get #values) table

            modalContent = [hsx|
                <form method="POST" action={CreateEnumValueAction}>
                    <input type="hidden" name="enumName" value={enumName}/>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumValueName" type="text" class="form-control" autofocus="autofocus"/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Enum Value</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowEnumAction { enumName }
            modalTitle = "New Enum Value"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
