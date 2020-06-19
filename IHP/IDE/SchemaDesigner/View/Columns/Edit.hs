module IHP.IDE.SchemaDesigner.View.Columns.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.Compiler (compileExpression)
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data EditColumnView = EditColumnView
    { statements :: [Statement]
    , tableName :: Text
    , columnId :: Int
    , column :: Column
    , primaryKeyExists :: Bool
    , enumNames :: [Text]
    }

instance View EditColumnView ViewContext where
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html EditColumnView { .. } = [hsx|
        <div class="row no-gutters bg-white">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName (zip [0..] columns) statements}
        </div>
        {Just modal}
    |]
        where
            table = findStatementByName tableName statements
            columns = maybe [] (get #columns) table


            
            primaryKeyCheckbox = if get #primaryKey column
                then preEscapedToHtml [plain|<label class="ml-1" style="font-size: 12px">
                            <input type="checkbox" name="primaryKey" class="mr-2" checked> Primary Key  
                        </label>|]
                else if primaryKeyExists
                    then mempty
                    else preEscapedToHtml [plain|<label class="ml-1" style="font-size: 12px">
                            <input type="checkbox" name="primaryKey" class="mr-2"/> Primary Key  
                        </label>|]
            
            allowNullCheckbox = if get #notNull column
                then preEscapedToHtml [plain|<input id="allowNull" type="checkbox" name="allowNull" class="mr-2"/>|]
                else preEscapedToHtml [plain|<input id="allowNull" type="checkbox" name="allowNull" class="mr-2" checked/>|]

            isUniqueCheckbox = if get #isUnique column
                then preEscapedToHtml [plain|<input type="checkbox" name="isUnique" class="mr-2" checked/>|]
                else preEscapedToHtml [plain|<input type="checkbox" name="isUnique" class="mr-2"/>|]
            
            modalContent = [hsx|
                <form method="POST" action={UpdateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="columnId" value={tshow columnId}/>

                    <div class="form-group">
                        <input
                            name="name"
                            type="text"
                            class="form-control"
                            autofocus="autofocus"
                            value={get #name column}
                            />
                    </div>

                    <div class="form-group">
                        {typeSelector (Just (get #columnType column)) enumNames}

                        <div class="mt-1 text-muted">
                            <label style="font-size: 12px">
                                {allowNullCheckbox} Nullable
                            </label>
                            <label class="ml-1" style="font-size: 12px">
                                {isUniqueCheckbox} Unique
                            </label>
                            {primaryKeyCheckbox}
                        </div>
                    </div>

                    <div class="form-group row">
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

typeSelector :: Maybe Text -> [Text] -> Html
typeSelector selected enumNames = [hsx|
        <select id="typeSelector" name="columnType" class="form-control select2-simple">
            {option selected "TEXT" "Text"}
            {option selected "INT" "Int"}
            {option selected "UUID" "UUID"}
            {option selected "BOOLEAN" "Bool"}
            {option selected "'TIMESTAMP WITH TIME ZONE'" "Timestamp"}
            {option selected "REAL" "Float"}
            {option selected "DOUBLE PRECISION" "Double"}
            {option selected "DATE" "Date"}
            {option selected "BINARY" "Binary"}
            {option selected "Time" "Time"}
            {forEach enumNames renderEnumType}
        </select>
|]
    where
        renderEnumType enum = option selected enum enum
        option :: Maybe Text -> Text -> Text -> Html
        option selected value text = case selected of
            Nothing -> [hsx|<option value={value}>{text}</option>|]
            Just selection ->
                if selection == value
                    then [hsx|<option value={value} selected="selected">{text}</option>|]
                    else [hsx|<option value={value}>{text}</option>|]      

defaultSelector :: Maybe Expression -> Html
defaultSelector defValue = [hsx|
    <div class="col-sm-10">
        <select id="defaultSelector" name="defaultValue" class="form-control select2">
            {forEach values renderValue}
        </select>
    </div>
|]
    where
        suggestedValues = [Nothing, Just (TextExpression ""), Just (VarExpression "null"), Just (CallExpression "NOW" [])]
        values = if defValue `elem` suggestedValues then suggestedValues else defValue:suggestedValues

        renderValue :: Maybe Expression -> Html
        renderValue e@(Just expression) = [hsx|<option value={compileExpression expression} selected={e == defValue}>{displayedValue}</option>|]
            where
                displayedValue = case expression of
                    TextExpression "" -> "\"\""
                    _ -> compileExpression expression
        renderValue Nothing = [hsx|<option value="" selected={Nothing == defValue}>No default</option>|]