module IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout, findTableByName, findEnumByName, visualNav, renderColumnSelector, renderColumn, renderEnumSelector, renderValue, renderObjectSelector, removeQuotes, replace, getDefaultValue, databaseControls) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import qualified Data.List as List

schemaDesignerLayout :: Html -> Html
schemaDesignerLayout inner = toolServerLayout [hsx|
<div class="container">
    <div class="row pt-5">
        <div class="col" style="display: flex; align-self: center;">
            {visualNav}
        </div>

        <div class="col" style="display: flex; align-self: center; justify-content: center">
            Application/Schema.sql
        </div>

        {databaseControls}
    </div>

    {inner}
</div>
|]

databaseControls :: Html
databaseControls = [hsx|
    <div class="d-flex justify-content-end col">
        <form class="p-2" action={pathTo DumpDbAction}>
            <button type="submit" class="btn btn-primary">DB to Fixtures</button>
        </form>
        <form class="p-2" style="padding-right: 0 !important;" action={pathTo PushToDbAction}>
            <button type="submit" class="btn btn-primary">Push to DB</button>
        </form>
    </div>
|]

findTableByName tableName statements = find pred statements
    where
        pred CreateTable { name } | name == tableName = True
        pred _ = False

findEnumByName enumName statements = find pred statements
    where
        pred CreateEnumType { name } | name == enumName = True
        pred _ = False

visualNav :: Html
visualNav =
    if isActivePath ShowCodeAction
        then [hsx|<a class="custom-control custom-switch visual-switch" href={TablesAction}>
                <input type="checkbox" class="custom-control-input" id="visual-switch" checked="checked"/>
                <label class="custom-control-label" for="visual-switch">Code Editor</label>
            </a>|]
        else [hsx|<a class="custom-control custom-switch visual-switch" href={ShowCodeAction}>
                <input type="checkbox" class="custom-control-input" id="visual-switch"/>
                <label class="custom-control-label text-muted" for="visual-switch">Code Editor</label>
            </a>|]

renderColumnSelector :: Text -> [(Int, Column)] -> [Statement] -> Html
renderColumnSelector tableName columns statements = [hsx|
<div class="col-8 column-selector" oncontextmenu="showContextMenu('context-menu-column-root')">
    <div class="d-flex">
        <h5>Columns</h5>
        <div class="toolbox">
            <a href={NewColumnAction tableName} class="btn btn-sm btn-outline-primary m-1" id="new-column">New</a>
        </div>
    </div>
    <table class="table table-hover table-sm">
        <tbody>
            {forEach columns (\column -> renderColumn (snd column) (fst column) tableName statements)}
        </tbody>
    </table>
</div>
<div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-column-root">
    <a href={NewColumnAction tableName}>Add Column</a>
</div>
|]

-- <a href={NewColumnAction tableName} class="text-danger text-center d-block" id="new-column">+ New Column</a>

renderColumn :: Column -> Int -> Text -> [Statement] -> Html
renderColumn Column { name, primaryKey, columnType, defaultValue, notNull, isUnique } id tableName statements = [hsx|
<tr class="column">
    <td class="context-column column-name" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}><a href={EditColumnAction tableName id} class="d-block text-body nounderline">{name}</a></td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{columnType}{renderAllowNull}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{renderDefault}{renderIsUnique}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{renderPrimaryKey}{renderForeignKey}</td>
</tr>
<div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
    <a href={EditColumnAction tableName id}>Edit Column</a>
    <a href={DeleteColumnAction tableName id} class="js-delete">Delete Column</a>
    <div></div>
    <form action={ToggleColumnUniqueAction tableName id}><button type="submit" class="link-button">{toggleButtonText}</button></form>
    {foreignKeyOption}
    <div></div>
    <a href={NewColumnAction tableName}>Add Column</a>
</div>
|]
    where
        toggleButtonText = if isUnique then [hsx|Remove Unique|] else [hsx|Make Unique|]
        contextMenuId = "context-menu-column-" <> tshow id
        renderPrimaryKey = if primaryKey then [hsx|PRIMARY KEY|] else mempty
        renderAllowNull = if notNull then mempty else [hsx|{" | " :: Text}NULL|]
        renderIsUnique = if isUnique then [hsx|IS UNIQUE|] else mempty
        renderDefault =
            case defaultValue of
                Just value -> [hsx|default: {value} |]
                Nothing -> mempty
        renderForeignKey = case findForeignKey statements tableName name of
            Just addConstraint@AddConstraint { constraint } -> [hsx|<a href={EditForeignKeyAction tableName name (get #constraintName addConstraint) (get #referenceTable constraint)} class="d-block nounderline" style="color: #808080;">FOREIGN KEY: {get #referenceTable constraint}</a>|]
            Nothing -> mempty
        foreignKeyOption = case findForeignKey statements tableName name of
            Just addConstraint@AddConstraint { constraint } -> [hsx|<a href={EditForeignKeyAction tableName name (get #constraintName addConstraint) (get #referenceTable constraint)}>Edit Foreign Key Constraint</a>|]
            Nothing -> [hsx|<a href={NewForeignKeyAction tableName name}>Add Foreign Key Constraint</a>|]

renderEnumSelector :: Text -> [(Int, Text)] -> Html
renderEnumSelector enumName values = [hsx|
<div class="col-8 column-selector" oncontextmenu="showContextMenu('context-menu-value-root')">
    <div class="d-flex">
        <h5>Enum Values</h5>
        <div class="toolbox">
            <a href={NewEnumValueAction enumName} class="btn btn-sm btn-outline-primary m-1">New</a>
        </div>
    </div>
    <table class="table table-hover table-sm">
        <tbody>
            {forEach values (\value -> renderValue (snd value) (fst value) enumName)}
        </tbody>
    </table>
</div>
<div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-value-root">
    <a href={NewEnumValueAction enumName}>Add Value</a>
</div>|]

renderValue :: Text -> Int -> Text -> Html
renderValue value valueId enumName = [hsx|
<tr class="column">
    <td class="context-column column-name" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
        {removeQuotes (cs value)}
    </td>
</tr>
<div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
    <a href={EditEnumValueAction enumName valueId}>Edit Value</a>
    <a href={DeleteEnumValueAction enumName valueId} class="js-delete">Delete Value</a>
    <div></div>
    <a href={NewEnumValueAction enumName}>Add Value</a>
</div>
|]
    where
        contextMenuId = "context-menu-value-" <> tshow valueId

renderObjectSelector statements activeObjectName = [hsx|
    <div class="col object-selector" oncontextmenu="showContextMenu('context-menu-object-root')">
        <div class="d-flex">
            <h5>Objects</h5>
            <div class="toolbox">
                <div class="btn-group m-1">
                    <a href={NewTableAction} class="btn btn-sm btn-outline-primary">New Table</a>
                    <button type="button" class="btn btn-sm btn-outline-primary dropdown-toggle dropdown-toggle-split" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                        <span class="sr-only">Toggle Dropdown</span>
                    </button>
                    <div class="dropdown-menu">
                        <a href={NewEnumAction} class="dropdown-item">New Enum</a>
                    </div>
                </div>
            </div>
        </div>
        {forEach statements (\statement -> renderObject (snd statement) (fst statement))}
    </div>
    <div class="custom-menu menu-for-table shadow backdrop-blur" id="context-menu-object-root">
        <a href={NewTableAction}>Add Table</a>
        <a href={NewEnumAction}>Add Enum</a>
    </div>
|]
    where
        renderObject :: Statement -> Int -> Html
        renderObject CreateTable { name } id = [hsx|
        <a href={ShowTableAction name} class={classes [("object object-table w-100 context-table", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
            <div class="d-flex">
                {name}
            </div>
        </a>
        <div class="custom-menu menu-for-table shadow backdrop-blur" id={contextMenuId}>
            <a href={EditTableAction name id}>Rename Table</a>
            <a href={DeleteTableAction id} class="js-delete">Delete Table</a>
            <div></div>
            <a href={ShowGeneratedCodeAction name}>Show Generated Haskell Code</a>
            <div></div>
            <a href={NewColumnAction name}>Add Column to Table</a>
            <div></div>
            <a href={NewTableAction}>Add Table</a>
            <a href={NewEnumAction}>Add Enum</a>
        </div>
        |]
            where
                contextMenuId = "context-menu-" <> tshow id
        renderObject CreateEnumType { name } id = [hsx|
        <a href={ShowEnumAction name} class={classes [("object object-table w-100 context-enum", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
            <div class="d-flex">
                {name}
            </div>
        </a>
        <div class="custom-menu menu-for-enum shadow backdrop-blur" id={contextMenuId}>
            <a href={EditEnumAction name id}>Rename Enum</a>
            <a href={DeleteTableAction id} class="js-delete">Delete Enum</a>
            <div></div>
            <a href={NewEnumValueAction name}>Add Column to Table</a>
            <div></div>
            <a href={NewTableAction}>Add Table</a>
            <a href={NewEnumAction}>Add Enum</a>
        </div>
        |]
            where
                contextMenuId = "context-menu-" <> tshow id
        renderObject Comment {} id = mempty
        renderObject AddConstraint {} id = mempty
        renderObject CreateExtension {} id = mempty
        renderObject statement id = [hsx|<div>{statement}</div>|]

removeQuotes :: [Char] -> Text
removeQuotes (x:xs) = cs (init xs)

findForeignKey statements tableName columnName = 
    find (\statement -> statement == AddConstraint 
        { tableName = tableName
        , constraintName = (get #constraintName statement)
        , constraint = ForeignKeyConstraint
            { columnName = columnName
            , referenceTable = (get #referenceTable (get #constraint statement))
            , referenceColumn = (get #referenceColumn (get #constraint statement))
            , onDelete = (get #onDelete (get #constraint statement))  }
            } ) statements

replace :: Int -> a -> [a] -> [a]
replace i e xs = case List.splitAt i xs of
   (before, _:after) -> before ++ (e: after)

getDefaultValue :: Text -> Text -> Maybe Text
getDefaultValue columnType value = case value of
    "EMPTY" -> Just "''"
    "NULL" -> Just "NULL"
    "NODEFAULT" -> Nothing
    "NOW()" -> Just value
    "uuid_generate_v4()" -> Just value
    custom -> case columnType of
        "TEXT" -> Just ("'" <> custom <> "'")
        "INT" -> Just custom
        "UUID" -> Just ("'" <> custom <> "'")
        "BOOLEAN" -> Just custom
        "TIMESTAMP WITH TIME ZONE" -> Just ("'" <> custom <> "'")
        "REAL" -> Just custom
        "DOUBLE PRECISION" -> Just custom
        "POINT" -> Just ("'" <> custom <> "'")
        _ -> Just ("'" <> custom <> "'")