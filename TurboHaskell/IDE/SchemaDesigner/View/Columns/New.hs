module TurboHaskell.IDE.SchemaDesigner.View.Columns.New where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    }

instance View NewColumnView ViewContext where
    html NewColumnView { .. } = [hsx|
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters">
                {renderObjectSelector statements (Just tableName)}
                {renderColumnSelector tableName columns}
            </div>
        </div>
        {Just modal}
    |]
        where
            table = findTableByName tableName statements
            columns = maybe [] (get #columns) table

            modalContent = [hsx|
                <form method="POST" action={CreateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName}/>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="name" type="text" class="form-control" autofocus="autofocus"/>
                        </div>
                    </div>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Type:</label>
                        <div class="col-sm-10">
                            <select name="columnType" class="form-control">
                                <option>Text</option>
                                <option>Int</option>
                            </select>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Column</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "New Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
