module TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, visualNav, codeNav, renderColumnSelector, renderColumn, renderEnumSelector, renderValue, renderObjectSelector) where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout

findTableByName tableName statements = find pred statements
    where
        pred CreateTable { name } | name == tableName = True
        pred _ = False

findEnumByName enumName statements = find pred statements
    where
        pred CreateEnumType { name } | name == enumName = True
        pred _ = False

visualNav = [hsx|
<ul class="nav nav-tabs bg-white" id="myTab" role="tablist">
    <li class="nav-item">
        <a class="nav-link active" href={pathTo TablesAction}>Visual Editor</a>
    </li>
    <li class="nav-item">
        <a class="nav-link" onclick={"window.location = '" <> pathTo ShowCodeAction <> "';"}>Code Editor</a>
    </li>
</ul>|]

codeNav = [hsx|
<ul class="nav nav-tabs bg-white" id="myTab" role="tablist">
    <li class="nav-item">
        <a class="nav-link" href={pathTo TablesAction}>Visual Editor</a>
    </li>
    <li class="nav-item">
        <a class="nav-link active">Code Editor</a>
    </li>
</ul>|]

renderColumnSelector :: Text -> [(Int, Column)] -> Html
renderColumnSelector tableName columns = [hsx|
<div class="col-8 column-selector" oncontextmenu="showContextMenu('context-menu-column-root')">
    <div class="d-flex">
        <h5>Columns</h5>
        <div class="toolbox">
            <a href={NewColumnAction tableName} class="btn btn-sm btn-outline-primary m-1" id="new-column">New</a>
        </div>
    </div>
    <table class="table table-hover table-sm">
        <tbody>
            {forEach columns (\column -> renderColumn (snd column) (fst column) tableName)}
        </tbody>
    </table>
</div>
<div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-column-root">
    <a href={NewColumnAction tableName}>Add Column</a>
</div>
|]

-- <a href={NewColumnAction tableName} class="text-danger text-center d-block" id="new-column">+ New Column</a>

renderColumn :: Column -> Int -> Text -> Html
renderColumn Column { name, primaryKey, columnType, defaultValue, notNull, isUnique } id tableName = [hsx|
<tr class="column">
    <td class="context-column column-name" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{name}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "')"}>{columnType}{renderAllowNull}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "')"}>{renderDefault}{renderIsUnique}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "')"}>{renderPrimaryKey}</td>
</tr>
<div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
    <a href={EditColumnAction tableName id}>Edit Column</a>
    <a href={DeleteColumnAction tableName id} class="js-delete">Delete Column</a>
    <div></div>
    <form action={ToggleColumnUniqueAction tableName id}><button type="submit" class="link-button backdrop-blur">{toggleButtonText}</button></form>
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

renderEnumSelector :: Text -> [(Int, Text)] -> Html
renderEnumSelector enumName values = [hsx|
<div class="col-8 column-selector">
    <div class="d-flex">
        <h5>Values</h5>
        <div class="toolbox">
            <a href={NewEnumValueAction enumName} class="btn btn-sm btn-outline-primary m-1">New</a>
        </div>
    </div>
    <table class="table table-hover table-sm">
        <tbody>
            {forEach values (\value -> renderValue (snd value) (fst value) enumName)}
        </tbody>
    </table>
</div>|]

renderValue :: Text -> Int -> Text -> Html
renderValue value valueId enumName = [hsx|
    <tr>
        <td>{value}</td>
        <td>
            <a href={EditEnumValueAction enumName valueId} class="btn btn-primary btn-sm m-1">Edit</a>
            <a href={DeleteEnumValueAction enumName valueId} class="btn btn-danger btn-sm m-1 js-delete">Delete</a>
        </td>

    </tr>
|]

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
            <a href={EditTableAction name id}>Edit Table</a>
            <a href={DeleteTableAction id} class="js-delete">Delete Table</a>
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
            <a href={EditEnumAction name id}>Edit Enum</a>
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
