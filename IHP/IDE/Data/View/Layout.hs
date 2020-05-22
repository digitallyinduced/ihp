module IHP.IDE.Data.View.Layout (customQuery) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout

customQuery :: Html
customQuery = [hsx|
<form method="POST" action={pathTo ShowQueryAction}>
    <input class="form-control" type="text" name="query" placeholder="SELECT * FROM table"/>
    <button class="d-none" type="submit">Query</button>
</form>|]