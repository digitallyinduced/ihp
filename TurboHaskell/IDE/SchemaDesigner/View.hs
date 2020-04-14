module TurboHaskell.IDE.SchemaDesigner.View where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout

data IndexView = IndexView
    { statements :: [Statement]
    }

data ShowView = ShowView
    { statements :: [Statement]
    , name :: Text
    }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|

<div class="container">
    <h1 class="py-5">TurboHaskell Schema Designer</h1>
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
        renderObject CreateTable { name } = [hsx|<a href={(pathTo ShowTableAction) <> "?name=" <> name} class={classes [("object object-table", True), ("active", Just name == activeObjectName)]}>{name}</a>|]
        renderObject CreateEnumType { name } = [hsx|<div class="object object-type">{name}</div>|]
        renderObject Comment {} = mempty
        renderObject AddConstraint {} = mempty
        renderObject CreateExtension {} = mempty
        renderObject statement = [hsx|<div>{statement}</div>|]



instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
<div class="container">
    <h1 class="py-5">TurboHaskell Schema Designer</h1>
    <div class="row no-gutters">
        {renderObjectSelector statements (Just name)}
        {renderColumnSelector columns}
    </div>
</div>
    |]
        where
            table = findTableByName name statements
            columns = maybe [] (get #columns) table


renderColumnSelector columns = [hsx|
<div class="col-8 column-selector">
    <table class="table table-hover table-sm">
        <tbody>
            {forEach columns renderColumn}
        </tbody>
    </table>
</div>
|]


findTableByName tableName statements = find pred statements
    where
        pred CreateTable { name } | name == tableName = True
        pred _ = False

renderColumn :: Column -> Html
renderColumn Column { name, primaryKey, columnType, defaultValue } = [hsx|
<tr>
    <td>{name}</td>
    <td>{columnType}</td>
    <td>{renderDefault}</td>
    <td>{renderPrimaryKey}</td>
</tr>
|]
    where
        renderPrimaryKey = if primaryKey then [hsx|PRIMARY KEY|] else mempty
        renderDefault =
            case defaultValue of
                Just value -> [hsx|default: {value}|]
                Nothing -> mempty