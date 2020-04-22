module TurboHaskell.IDE.SchemaDesigner.View where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal

data IndexView = IndexView
    { statements :: [Statement]
    }

data ShowView = ShowView
    { statements :: [Statement]
    , name :: Text
    }

data NewColumnView = NewColumnView
    { statements :: [Statement]
    , tableName :: Text
    }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|

<div class="container">
    <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
        <button type="submit" class="btn btn-primary my-3">Push to DB</button>
    </form>
    <div class="row no-gutters">
        {renderObjectSelector statements Nothing}
    </div>
</div>
    |]

renderObjectSelector statements activeObjectName = [hsx|
<div class="col object-selector">
    <h5>Objects</h5>
    {forEach statements renderObject}
</div>
|]
    where
        renderObject :: Statement -> Html
        renderObject CreateTable { name } = [hsx|<a href={ShowTableAction name} class={classes [("object object-table", True), ("active", Just name == activeObjectName)]}>{name}</a>|]
        renderObject CreateEnumType { name } = [hsx|<div class="object object-type">{name}</div>|]
        renderObject Comment {} = mempty
        renderObject AddConstraint {} = mempty
        renderObject CreateExtension {} = mempty
        renderObject statement = [hsx|<div>{statement}</div>|]



instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
<div class="container">
    <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
        <button type="submit" class="btn btn-primary my-3">Push to DB</button>
    </form>
    <div class="row no-gutters">
        {renderObjectSelector statements (Just name)}
        {renderColumnSelector name columns}
    </div>
</div>
    |]
        where
            table = findTableByName name statements
            columns = maybe [] (get #columns) table



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




renderColumnSelector tableName columns = [hsx|
<div class="col-8 column-selector">
    <table class="table table-hover table-sm">
        <tbody>
            {forEach columns renderColumn}
        </tbody>
    </table>

    <a href={NewColumnAction tableName} class="btn btn-sm btn-primary">New</a>
</div>
|]


findTableByName tableName statements = find pred statements
    where
        pred CreateTable { name } | name == tableName = True
        pred _ = False

renderColumn :: Column -> Html
renderColumn Column { name, primaryKey, columnType, defaultValue, notNull } = [hsx|
<tr>
    <td>{name}</td>
    <td>{columnType}</td>
    <td>{renderDefault}</td>
    <td>{renderPrimaryKey}</td>
    <td>{renderAllowNull}</td>
</tr>
|]
    where
        renderPrimaryKey = if primaryKey then [hsx|PRIMARY KEY|] else mempty
        renderAllowNull = if notNull then mempty else [hsx|NULL ALLOWED|]
        renderDefault =
            case defaultValue of
                Just value -> [hsx|default: {value}|]
                Nothing -> mempty