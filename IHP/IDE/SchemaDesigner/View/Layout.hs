module IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout, findStatementByName, visualNav, renderColumnSelector, renderColumn, renderEnumSelector, renderValue, renderObjectSelector, removeQuotes, replace, getDefaultValue, databaseControls, findForeignKey) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.Compiler (compileIdentifier, compilePostgresType, compileExpression)
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified Text.Megaparsec as Megaparsec
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
    <form method="POST" action={pathTo UpdateDbAction} id="update-db-form"/>
    <form method="POST" action={pathTo PushToDbAction} id="push-to-db-form"/>
    <form method="POST" action={pathTo DumpDbAction} id="db-to-fixtures-form"/>
    <div class="btn-group btn-group-sm mb-2">
        <button
            type="submit"
            form="update-db-form"
            class="btn btn-primary"
            data-toggle="tooltip"
            data-placement="bottom"
            data-html="true"
            title="Dumps DB to Fixtures.sql.<br><br>Delete the DB.<br><br>Recreate using Schema.sql and Fixtures.sql"
            >Update DB</button>

        <button type="button" class="btn btn-primary dropdown-toggle dropdown-toggle-split" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
            <span class="sr-only">Toggle Dropdown</span>
        </button>


        <div class="dropdown-menu dropdown-menu-right" aria-labelledby="dropdownMenuLink">
            <button
                type="submit"
                class="dropdown-item"
                form="db-to-fixtures-form"
                data-toggle="tooltip"
                data-placement="left"
                data-html="true"
                title="Saves the content of all tables to Application/Fixtures.sql"
                >Save DB to Fixtures</button>
            <button
                type="submit"
                class="dropdown-item"
                form="push-to-db-form"
                data-toggle="tooltip"
                data-placement="left"
                data-html="true"
                title="Delete the DB and recreate using Application/Schema.sql and Application/Fixture.sql<br><br><strong class=text-danger>Save DB to Fixtures before using this to avoid data loss</strong>"
                >Push to DB</button>
        </div>
    </div>
</div>
|]

findStatementByName statementName statements = find pred statements
    where
        pred (StatementCreateTable CreateTable { name }) | (toUpper name) == (toUpper statementName) = True
        pred (StatementCreateTable CreateTable { name }) | (toUpper name) == (toUpper (tshow statementName)) = True
        pred CreateEnumType { name } | (toUpper name) == (toUpper statementName) = True
        pred CreateEnumType { name } | (toUpper name) == (toUpper (tshow statementName)) = True
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
<div class="col-8 column-selector d-flex">
    <section class="flex-grow-1" oncontextmenu="showContextMenu('context-menu-column-root')">
        <div>
            <h5>Columns</h5>
        </div>
        <table class="table table-hover table-sm">
            <tbody>
                {forEach columns (\column -> renderColumn (snd column) (fst column) tableName statements)}
            </tbody>
        </table>
    </section>

    <section>
        {columnIndexes}
    </section>
</div>
<div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-column-root">
    <a href={NewColumnAction tableName}>Add Column</a>
</div>
|]
    where
        columnIndexes =
            case findTableIndex statements tableName of
                Just _ -> [hsx|
                    <div>
                        <h5>Indexes</h5>
                    </div>
                    <table class="table table-hover table-sm">
                        {renderColumnIndexes tableName statements}
                    </table>
                |]
                Nothing -> [hsx||]

-- <a href={NewColumnAction tableName} class="text-danger text-center d-block" id="new-column">+ New Column</a>

renderColumn :: Column -> Int -> Text -> [Statement] -> Html
renderColumn Column { name, columnType, defaultValue, notNull, isUnique } id tableName statements = [hsx|
<tr class="column">
    <td class="context-column column-name" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}><a href={EditColumnAction tableName id} class="d-block text-body nounderline">{name}</a></td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{compilePostgresType columnType}{renderAllowNull}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{renderDefault}{renderIsUnique}</td>
    <td class="context-column" oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>{renderPrimaryKey}{renderForeignKey}</td>
</tr>
<div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
    <a href={EditColumnAction tableName id}>Edit Column</a>
    <a href={DeleteColumnAction tableName id name} class="js-delete">Delete Column</a>
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
        renderPrimaryKey = if inPrimaryKey then [hsx|PRIMARY KEY|] else mempty
        inPrimaryKey = case findPrimaryKey statements tableName of
          Nothing -> False
          Just columnNames -> name `elem` columnNames
        renderAllowNull = if notNull then mempty else [hsx|{" | " :: Text}NULL|]
        renderIsUnique = if isUnique then [hsx|IS UNIQUE|] else mempty
        renderDefault =
            case defaultValue of
                Just value -> [hsx|default: {compileExpression value} |]
                Nothing -> mempty
        renderForeignKey = case findForeignKey statements tableName name of
            Just addConstraint@AddConstraint { constraint } -> [hsx|<a href={EditForeignKeyAction tableName name (get #constraintName addConstraint) (get #referenceTable constraint)} class="d-block nounderline" style="color: #808080;">FOREIGN KEY: {get #referenceTable constraint}</a>|]
            _ -> mempty
        foreignKeyOption = case findForeignKey statements tableName name of
            Just addConstraint@AddConstraint { constraint } ->
                [hsx|<a href={EditForeignKeyAction tableName name (get #constraintName addConstraint) (get #referenceTable constraint)}>Edit Foreign Key Constraint</a>
                <a href={DeleteForeignKeyAction (get #constraintName addConstraint) tableName} class="js-delete">Delete Foreign Key Constraint</a>|]
            _ -> [hsx|<a href={NewForeignKeyAction tableName name}>Add Foreign Key Constraint</a>|]

renderColumnIndexes tableName statements = [hsx|
<tr>
    {index}
</tr>
|]
    where
        index = case findTableIndex statements tableName of
            Just statement -> [hsx|
                    <td>{get #indexName statement}</td>
                    <td>{columns}</td>
                |] where columns = get #columnNames statement |> intercalate ", "
            Nothing -> [hsx||]

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
        {value}
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
    <div class={classes ["col", "object-selector", ("empty", isEmptySelector)]} oncontextmenu="showContextMenu('context-menu-object-root')">
        <div class="d-flex">
            <h5>Objects</h5>
        </div>
        {forEach statements (\statement -> renderObject (snd statement) (fst statement))}
        <div class="text-muted context-menu-notice">Right click to open context menu</div>
    </div>
    <div class="custom-menu menu-for-table shadow backdrop-blur" id="context-menu-object-root">
        <a href={NewTableAction}>Add Table</a>
        <a href={NewEnumAction}>Add Enum</a>
    </div>
|]
    where
        isEmptySelector :: Bool
        isEmptySelector = statements |> map snd |> filter shouldRenderObject |> isEmpty

        renderObject :: Statement -> Int -> Html
        renderObject (StatementCreateTable CreateTable { name }) id = [hsx|
        <a href={ShowTableAction name} class={classes [("object object-table w-100 context-table", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
            <div class="d-flex">
                {name}
            </div>
        </a>
        <div class="custom-menu menu-for-table shadow backdrop-blur" id={contextMenuId}>
            <a href={EditTableAction name id}>Rename Table</a>
            <a href={DeleteTableAction id name} class="js-delete">Delete Table</a>
            <div></div>
            <a href={ShowGeneratedCodeAction name}>Show Generated Haskell Code</a>
            {when controllerDoesNotExist generateControllerLink}
            {unless controllerDoesNotExist openControllerLink}
            <div></div>
            <a href={NewColumnAction name}>Add Column to Table</a>
            <div></div>
            <a href={NewTableAction}>Add Table</a>
            <a href={NewEnumAction}>Add Enum</a>
        </div>
        |]
            where
                contextMenuId = "context-menu-" <> tshow id
                generateControllerLink = [hsx|<a href={pathTo NewControllerAction <> "?name=" <> name}>Generate Controller</a>|]
                openControllerLink = [hsx|<a href={pathTo OpenControllerAction <> "?name=" <> name} target="_blank">Open Controller</a>|]
                controllerDoesNotExist = not $ (ucfirst name) `elem` webControllers
                (WebControllers webControllers) = fromFrozenContext @WebControllers

        renderObject CreateEnumType { name } id = [hsx|
        <a href={ShowEnumAction name} class={classes [("object object-table w-100 context-enum", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
            <div class="d-flex">
                {name}
            </div>
        </a>
        <div class="custom-menu menu-for-enum shadow backdrop-blur" id={contextMenuId}>
            <a href={EditEnumAction name id}>Rename Enum</a>
            <a href={DeleteEnumAction id} class="js-delete">Delete Enum</a>
            <div></div>
            <a href={ShowGeneratedCodeAction name}>Show Generated Haskell Code</a>
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
        renderObject CreateIndex {} id = mempty
        renderObject statement id = [hsx|<div>{statement}</div>|]

        shouldRenderObject (StatementCreateTable CreateTable {}) = True
        shouldRenderObject CreateEnumType {} = True
        shouldRenderObject _ = False

removeQuotes :: [Char] -> Text
removeQuotes (x:xs) = cs $ fromMaybe [] (init xs)
removeQuotes n = cs n

findForeignKey :: [Statement] -> Text -> Text -> Maybe Statement
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

findPrimaryKey :: [Statement] -> Text -> Maybe [Text]
findPrimaryKey statements tableName = do
    (StatementCreateTable createTable) <- find (isCreateTable tableName) statements
    pure . primaryKeyColumnNames $ primaryKeyConstraint createTable
    where
      isCreateTable tableName (StatementCreateTable CreateTable { name }) = name == tableName
      isCreateTable _ _ = False

findTableIndex :: [Statement] -> Text -> Maybe Statement
findTableIndex statements tableName =
    find (\case CreateIndex { tableName = tableName' } -> tableName' == tableName; otherwise -> False) statements

replace :: Int -> a -> [a] -> [a]
replace i e xs = case List.splitAt i xs of
   (before, _:after) -> before ++ (e: after)
   (a, b) -> a ++ b

getDefaultValue :: Text -> Text -> Maybe Expression
getDefaultValue columnType value = case Megaparsec.runParser Parser.expression "" value of
        Left _ -> Nothing
        Right expression -> Just expression