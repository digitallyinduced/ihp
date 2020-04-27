module TurboHaskell.IDE.SchemaDesigner.View.Columns.Edit where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data EditColumnView = EditColumnView
    { statements :: [Statement]
    , tableName :: Text
    , columnId :: Int
    , column :: Column
    }

instance View EditColumnView ViewContext where
    html EditColumnView { .. } = [hsx|
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector statements (Just tableName)}
                {renderColumnSelector tableName (zip [0..] columns)}
            </div>
        </div>
        {Just modal}
    |]
        where
            table = findTableByName tableName statements
            columns = maybe [] (get #columns) table


            
            primaryKeyCheckbox = if get #primaryKey column
                then preEscapedToHtml [plain|<input type="checkbox" name="primaryKey" class="mr-2" checked/>|]
                else preEscapedToHtml [plain|<input type="checkbox" name="primaryKey" class="mr-2"/>|]
            
            allowNullCheckbox = if get #notNull column
                then preEscapedToHtml [plain|<input type="checkbox" name="allowNull" class="mr-2"/>|]
                else preEscapedToHtml [plain|<input type="checkbox" name="allowNull" class="mr-2" checked/>|]
            
            modalContent = [hsx|
                <form method="POST" action={UpdateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="columnId" value={tshow columnId}/>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="name" type="text" class="form-control" autofocus="autofocus" value={get #name column}/>
                        </div>
                    </div>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Type:</label>
                        <div class="col-sm-10">
                            {typeSelector (get #columnType column)}
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="col col-form-label">
                            {primaryKeyCheckbox}
                            Primary Key
                        </label>
                        <label class="col col-form-label">
                            {allowNullCheckbox}
                            Allow Null
                        </label>
                    </div>

                    <div class="form-group row">
                        <label for="inputEmail3" class="col-sm-2 col-form-label">Default Value:</label>
                        {defaultSelector (get #defaultValue column)}
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Column</button>
                    </div>
                    <input type="hidden" name="primaryKey" value={inputValue False}/>
                    <input type="hidden" name="allowNull" value={inputValue False}/>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "Edit Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

typeSelector selected = preEscapedToHtml [plain|
    <select name="columnType" class="form-control">
        #{option selected "TEXT" "Text"}
        #{option selected "INT" "Int"}
        #{option selected "UUID" "UUID"}
        #{option selected "BOOLEAN" "Bool"}
        #{option selected "TIMESTAMP WITH TIME ZONE" "Timestamp"}
        #{option selected "REAL" "Float"}
        #{option selected "DOUBLE PRECISION" "Double"}
        #{option selected "POINT" "Point"}
    </select>
|]
    where
        option selected value text = if selected == value
            then [plain|<option value=#{value} selected>#{text}</option>|]
            else [plain|<option value=#{value}>#{text}</option>|]

defaultSelector selected = preEscapedToHtml [plain|
    <div class="col-sm-10">
        <select name="defaultValue" class="form-control">
            #{option (selectedType selected) "NODEFAULT" "no default"}
            #{option (selectedType selected) "EMPTY" "''"}
            #{option (selectedType selected) "NULL" "null"}
            #{option (selectedType selected) "CUSTOM" "custom"}
        </select>
    </div>
    <div class="col-sm-2"></div>
    <div class="col-sm-10">
        <input style=#{if selected == "CUSTOM" then "display: block;" else "display: none;"} name="customDefaultValue" type="text" class="form-control" value=#{fromMaybe "" selected}>    
    </div>
|]
    where
        option selected value text = if selected == value
            then [plain|<option value=#{value} selected>#{text}</option>|]
            else [plain|<option value=#{value}>#{text}</option>|]
        selectedType selection = case selection of
            Just "''" -> "EMPTY"
            Just "NULL" -> "NULL"
            Just "null" -> "NULL"
            Nothing -> "NODEFAULT"
            _ -> "CUSTOM"

