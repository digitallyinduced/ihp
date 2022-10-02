module IHP.IDE.SchemaDesigner.View.Columns.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.Compiler as Compiler
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data EditColumnView = EditColumnView
    { statements :: [Statement]
    , tableName :: Text
    , columnId :: Int
    , column :: Column
    , enumNames :: [Text]
    }

instance View EditColumnView where
    html EditColumnView { column = column@Column { name }, .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName (zip [0..] columns) statements}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            table = findStatementByName tableName statements
            columns = maybe [] (get #columns . unsafeGetCreateTable) table
            primaryKeyColumns = maybe [] (primaryKeyColumnNames . get #primaryKeyConstraint . unsafeGetCreateTable) table

            isArrayType (PArray _) = True
            isArrayType _ = False

            isPrimaryKey :: Bool
            isPrimaryKey = name `elem` primaryKeyColumns

            modalContent = [hsx|
                <form method="POST" action={UpdateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="columnId" value={tshow columnId}/>

                    <div class="form-group">
                        <input
                            id="nameInput"
                            name="name"
                            type="text"
                            class="form-control"
                            autofocus="autofocus"
                            value={get #name column}
                            data-table-name-singular={singularize tableName}
                            />
                    </div>

                    <div class="form-group">
                        {typeSelector (Just (get #columnType column)) enumNames}

                        <div class="d-flex text-muted mt-1" id="column-options">
                            <div class="custom-control custom-checkbox mr-2">
                                <input id="allowNull" type="checkbox" name="allowNull" class="custom-control-input" checked={not (get #notNull column)}/>
                                <label class="mr-1 custom-control-label" for="allowNull">
                                    Nullable
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox mr-2">
                                <input type="checkbox" id="isUnique" name="isUnique" class="custom-control-input" checked={get #isUnique column}/>
                                <label class="custom-control-label" for="isUnique">
                                    Unique
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox mr-2">
                                <input type="checkbox" id="primaryKey" name="primaryKey" class="custom-control-input" checked={isPrimaryKey}/>
                                <label class="custom-control-label" for="primaryKey">
                                    Primary Key
                                </label>
                            </div>

                            <div class="custom-control custom-checkbox mr-2">
                                <input id="isArray" type="checkbox" name="isArray" class="custom-control-input" checked={isArrayType (get #columnType column)}/>
                                <label class="custom-control-label">
                                     Array Type
                                </label>
                            </div>
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
                    <input type="hidden" name="isArray" value={inputValue False}/>
                </form>
            |]
            modalFooter = mempty
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "Edit Column"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

typeSelector :: Maybe PostgresType -> [Text] -> Html
typeSelector postgresType enumNames = [hsx|
        <select id="typeSelector" name="columnType" class="form-control select2-simple">
            <optgroup label="Common Types">
                {option isSelected "TEXT" "Text"}
                {option isSelected "INT" "Int"}
                {option isSelected "UUID" "UUID"}
                {option isSelected "BOOLEAN" "Bool"}
                {option isSelected "DATE" "Date / Day"}
                {option isSelected "TIMESTAMP WITH TIME ZONE" "Timestamp (UTCTime)"}
                {option isSelected "SERIAL" "Serial"}
            </optgroup>
            {customenums enumNames}
            <optgroup label="Other Types">
                {option isSelected "TIMESTAMP WITHOUT TIME ZONE" "Timestamp (LocalTime)"}
                {option isSelected "REAL" "Float"}
                {option isSelected "DOUBLE PRECISION" "Double"}
                {option isSelected "POINT" "Point"}
                {option isSelected "BYTEA" "Binary"}
                {option isSelected "Time" "Time"}
                {option isSelected "BIGSERIAL" "Bigserial"}
                {option isSelected "SMALLINT" "Int (16bit)"}
                {option isSelected "BIGINT" "Int (64bit)"}
                {option isSelected "JSONB" "JSON"}
                {option isSelected "INET" "IP Address"}
                {option isSelected "TSVECTOR" "TSVector"}
            </optgroup>
        </select>
|]
    where
        isSelected :: Maybe Text
        isSelected = fmap Compiler.compilePostgresType postgresType

        renderEnumType enum = option isSelected enum enum
        option :: Maybe Text -> Text -> Text -> Html
        option selected value text = case selected of
            Nothing -> [hsx|<option value={value}>{text}</option>|]
            Just selection ->
                if selection == value || selection == value <> "[]"
                    then [hsx|<option value={value} selected="selected">{text}</option>|]
                    else [hsx|<option value={value}>{text}</option>|]
        customenums [] = [hsx| |]
        customenums xs = [hsx| <optgroup label="Custom Enums">
                                {forEach xs renderEnumType}
                               </optgroup>
                         |]

defaultSelector :: Maybe Expression -> Html
defaultSelector defValue = [hsx|
    <div class="col-sm-10">
        <select id="defaultSelector" name="defaultValue" class="form-control select2">
            {forEach values renderValue}
        </select>
    </div>
|]
    where
        suggestedValues = [Nothing, Just (TextExpression ""), Just (VarExpression "NULL"), Just (CallExpression "NOW" [])]
        values = if defValue `elem` suggestedValues then suggestedValues else defValue:suggestedValues

        renderValue :: Maybe Expression -> Html
        renderValue e@(Just expression) = [hsx|<option value={Compiler.compileExpression expression} selected={e == defValue}>{displayedValue}</option>|]
            where
                displayedValue = case expression of
                    TextExpression "" -> "\"\""
                    _ -> Compiler.compileExpression expression
        renderValue Nothing = [hsx|<option value="" selected={Nothing == defValue}>No default</option>|]
