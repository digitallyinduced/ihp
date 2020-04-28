module TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, renderColumnSelector, renderColumn, renderObjectSelector) where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout

findTableByName tableName statements = find pred statements
    where
        pred CreateTable { name } | name == tableName = True
        pred _ = False

renderColumnSelector :: Text -> [(Int, Column)] -> Html
renderColumnSelector tableName columns = [hsx|
<div class="col-8 column-selector">
    <div class="d-flex">
        <h5>Columns</h5>
        <div class="toolbox">
            <a href={NewColumnAction tableName} class="btn btn-sm btn-outline-primary m-1">New</a>
        </div>
    </div>
    <table class="table table-hover table-sm">
        <tbody>
            {forEach columns (\column -> renderColumn (snd column) (fst column) tableName)}
        </tbody>
    </table>
</div>
|]

renderColumn :: Column -> Int -> Text -> Html
renderColumn Column { name, primaryKey, columnType, defaultValue, notNull } id tableName = [hsx|
<tr>
    <td>{name}</td>
    <td>{columnType}{renderAllowNull}</td>
    <td>{renderDefault}</td>
    <td>{renderPrimaryKey}</td>
    <td>
        <a href={EditColumnAction tableName id} class="btn btn-primary btn-sm">Edit</a>
    </td>
</tr>
|]
    where
        renderPrimaryKey = if primaryKey then [hsx|PRIMARY KEY|] else mempty
        renderAllowNull = if notNull then mempty else [hsx|{" | " :: Text}NULL|]
        renderDefault =
            case defaultValue of
                Just value -> [hsx|default: {value}|]
                Nothing -> mempty

renderObjectSelector statements activeObjectName = [hsx|
    <div class="col object-selector">
    <div class="d-flex">
        <h5>Objects</h5>
        <div class="toolbox">
            <a href={NewTableAction} class="btn btn-sm btn-outline-primary m-1">New</a>
        </div>
    </div>
    {forEach statements (\statement -> renderObject (snd statement) (fst statement))}
</div>
|]
    where
        renderObject :: Statement -> Int -> Html
        renderObject CreateTable { name } id = [hsx|
        <a href={ShowTableAction name} class={classes [("object object-table w-100", True), ("active", Just name == activeObjectName)]}>
            <div class="d-flex">
                {name}
                <div class="toolbox w-50">
                    <a href={EditTableAction name id} class="btn btn-primary btn-sm m-1">Edit</a>
                    <a href={DeleteTableAction id} class="btn btn-danger btn-sm m-1 js-delete">Delete</a>
                </div>
            </div>
        </a>|]
        renderObject CreateEnumType { name } id = [hsx|
        <div class="object object-type w-100">
            <div class="d-flex">
                {name}
                <div class="toolbox w-50">
                    <a href={DeleteTableAction id} class="btn btn-danger btn-sm m-1 js-delete">Delete</a>
                </div>
            </div>
        </div>|]
        renderObject Comment {} id = mempty
        renderObject AddConstraint {} id = mempty
        renderObject CreateExtension {} id = mempty
        renderObject statement id = [hsx|<div>{statement}</div>|]
