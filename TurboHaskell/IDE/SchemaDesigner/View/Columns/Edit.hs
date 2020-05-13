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
    , primaryKeyExists :: Bool
    }

instance View EditColumnView ViewContext where
    html EditColumnView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just tableName)}
                {renderColumnSelector tableName (zip [0..] columns) statements}
            </div>
        </div>
        {Just modal}
    |]
        where
            table = findTableByName tableName statements
            columns = maybe [] (get #columns) table


            
            primaryKeyCheckbox = if get #primaryKey column
                then preEscapedToHtml [plain|<label class="col col-form-label">
                        <input type="checkbox" name="primaryKey" class="mr-2" checked/>
                            Primary Key
                        </label>|]
                else if primaryKeyExists
                    then mempty
                    else preEscapedToHtml [plain|<label class="col col-form-label">
                        <input type="checkbox" name="primaryKey" class="mr-2"/>
                            Primary Key
                        </label>|]
            
            allowNullCheckbox = if get #notNull column
                then preEscapedToHtml [plain|<input type="checkbox" name="allowNull" class="mr-2"/>|]
                else preEscapedToHtml [plain|<input type="checkbox" name="allowNull" class="mr-2" checked/>|]

            isUniqueCheckbox = if get #isUnique column
                then preEscapedToHtml [plain|<input type="checkbox" name="isUnique" class="mr-2" checked/>|]
                else preEscapedToHtml [plain|<input type="checkbox" name="isUnique" class="mr-2"/>|]
            
            modalContent = [hsx|
                <form method="POST" action={UpdateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="columnId" value={tshow columnId}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="name" type="text" class="form-control" autofocus="autofocus" value={get #name column}/>
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Type:</label>
                        <div class="col-sm-10">
                            {typeSelector (get #columnType column)}
                        </div>
                    </div>
                    <div class="form-group row">
                        {primaryKeyCheckbox}
                        <label class="col col-form-label">
                            {allowNullCheckbox}
                            Allow Null
                        </label>
                        <label class="col col-form-label">
                            {isUniqueCheckbox}
                            Unique
                        </label>
                    </div>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Default Value:</label>
                        {defaultSelector (get #defaultValue column)}
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Column</button>
                    </div>
                    <input type="hidden" name="primaryKey" value={inputValue False}/>
                    <input type="hidden" name="allowNull" value={inputValue False}/>
                    <input type="hidden" name="isUnique" value={inputValue False}/>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "Edit Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

typeSelector selected = preEscapedToHtml [plain|
    <select id="typeSelector" name="columnType" class="form-control select2-simple">
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
        <select id="defaultSelector" name="defaultValue" class="form-control select2">
            #{option (selectedType selected) "NODEFAULT" "no default"}
            #{option (selectedType selected) "EMPTY" "''"}
            #{option (selectedType selected) "NULL" "null"}
            #{maybeCustom selected}
        </select>
    </div>
    <script>
        $('.select2').select2({
            placeholder: "Select a default value or type in a custom default value",
            tags: true
        });
        $('.select2-simple').select2();
        $('#typeSelector').change(function () {
            switch (this.value) {
                case "UUID":
                    $('#defaultSelector').empty()
                    .append(new Option("uuid_generate_v4()", 'uuid_generate_v4()', true, true))
                    .append(new Option("no default", "NODEFAULT", false, false))
                    .append(new Option("''", "EMPTY", false, false))
                    .append(new Option("null", "NULL", false, false))
                    .trigger('change');
                    break;
                case "TIMESTAMP WITH TIME ZONE":
                    $('#defaultSelector').empty()
                    .append(new Option("created at", 'NOW()', true, true))
                    .append(new Option("no default", "NODEFAULT", false, false))
                    .append(new Option("''", "EMPTY", false, false))
                    .append(new Option("null", "NULL", false, false))
                    .trigger('change');
                    break;
                default:
                    $('#defaultSelector').empty()
                    .append(new Option("no default", "NODEFAULT", true, true))
                    .append(new Option("''", "EMPTY", false, false))
                    .append(new Option("null", "NULL", false, false))
                    .trigger('change');
                    break;
            }
        });
    </script>
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
            custom -> custom
        maybeCustom selection = case selection of
            Just "''" -> mempty
            Just "NULL" -> mempty
            Just "null" -> mempty
            Nothing -> mempty
            Just custom -> [plain|<option value=#{custom} selected>#{custom}</option>|]