module IHP.IDE.SchemaDesigner.View.Layout
( schemaDesignerLayout
, findStatementByName
, visualNav
, renderColumnSelector
, renderColumn
, renderEnumSelector
, renderValue
, renderObjectSelector
, removeQuotes
, replace
, findForeignKey
, findTableIndex
, migrationStatus
, emptyColumnSelectorContainer
) where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Helper.View
import IHP.IDE.ToolServer.Layout hiding (tableIcon)
import IHP.IDE.SchemaDesigner.Compiler (compilePostgresType, compileExpression)
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified Text.Megaparsec as Megaparsec
import qualified Data.List as List

schemaDesignerLayout :: Html -> Html
schemaDesignerLayout inner = toolServerLayout [hsx|
    <div class={classes ["d-flex d-flex flex-column h-100", ("migration-status-visible", hasUnmigratedChanges)]}>
        {visualNav}
        <div class="h-100 d-flex flex-column">
            {inner}
        </div>
    </div>
|]
    where
        (DatabaseNeedsMigration hasUnmigratedChanges) = fromFrozenContext @DatabaseNeedsMigration

unmigratedChanges :: Html
unmigratedChanges = [hsx|
<div class="alert alert-primary d-flex align-items-center" role="alert">
    <div style="height: fit-content">
        <strong>Unmigrated Changes</strong>
        Your app database is not in sync with the Schema.sql
    </div>
    {databaseControls}
</div>
|]


migrationStatus :: Html
migrationStatus = fromMaybe mempty migrationStatusOrNothing

migrationStatusOrNothing :: _ => Maybe _
migrationStatusOrNothing = if hasPendingMigrations
        then Just pendingMigrations
        else if databaseNeedsMigration
            then Just unmigratedChanges
            else Nothing
    where
        (DatabaseNeedsMigration databaseNeedsMigration) = fromFrozenContext @DatabaseNeedsMigration

        hasPendingMigrations :: Bool
        hasPendingMigrations = False

        unmigratedChanges :: Html
        unmigratedChanges = [hsx|
        <div id="migration-status-container">
            <div class="alert alert-primary d-flex align-items-center shadow-lg" role="alert">
                {migrationStatusIcon}
                <div class="user-select-none">
                    <div><strong>Unmigrated Changes</strong></div>
                    Your app database is not in sync with the Schema
                </div>
                <div class="ml-auto d-flex justify-content-end">
                    <a
                        href={NewMigrationAction}
                        class="btn px-4 btn-dark"
                        >Migrate DB <span class="btn-arrow">→</span></a>
                </div>
            </div>
        </div>
        |]

        pendingMigrations :: Html
        pendingMigrations = [hsx|
        <div id="migration-status-container">
            <div class="alert alert-primary d-flex align-items-center shadow-lg" role="alert">
                {migrationStatusIcon}
                <div class="user-select-none">
                    <div><strong>Pending Changes</strong></div>
                    You have migrations that haven't been run yet
                </div>
                <div class="ml-auto d-flex justify-content-end">
                    <a
                        href="#"
                        class="btn px-4 btn-dark"
                        >Run Migrations <span class="btn-arrow">→</span></a>
                </div>
            </div>
        </div>
        |]

        migrationStatusIcon :: Html
        migrationStatusIcon = preEscapedToHtml [plain|
            <svg width="33px" height="33px" viewBox="0 0 33 33" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="mr-3">
                <g id="Schema" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                    <g id="Message" transform="translate(-383.000000, -813.000000)" fill="#FFCD1B">
                        <g id="Group" transform="translate(383.000000, 813.000000)">
                            <rect id="Rectangle" fill-opacity="0.322170017" x="0" y="0" width="33" height="33" rx="5"></rect>
                            <g id="Arrow-Double-3---24px" transform="translate(7.000000, 7.000000)">
                                <path d="M4.39179,10.2850058 C4.78668,9.89890559 5.41981,9.9060056 5.80592,10.3009058 C6.19203,10.6958061 6.18492,11.3289064 5.79003,11.7150067 L3.4531,14.0000172 L19,14.0000172 C19.5523,14.0000172 20,14.4477083 20,15.0000172 C20,15.5523089 19.5523,16.0000172 19,16.0000172 L3.4531,16.0000172 L5.79003,18.2850105 C6.18492,18.6711107 6.19203,19.3042111 5.80592,19.6991113 C5.41981,20.0940116 4.78668,20.1011116 4.39179,19.7150113 L0.30088,15.715009 C0.10847,15.5269089 0,15.2691087 0,15.0000172 C0,14.7309084 0.10847,14.4731083 0.30088,14.2850082 L4.39179,10.2850058 Z M15.6082,0.284989971 C15.2133,-0.101120255 14.5802,-0.0940002508 14.1941,0.30087998 C13.808,0.695770211 13.8151,1.32890058 14.21,1.71501081 L16.5469,4 L1,4 C0.44772,4 0,4.44771241 0,5 C0,5.55228305 0.44772,6 1,6 L16.5469,6 L14.21,8.28500465 C13.8151,8.67110488 13.808,9.30420525 14.1941,9.69910548 C14.5802,10.0940057 15.2133,10.1011057 15.6082,9.71500549 L19.6991,5.71501315 C19.8915,5.52687304 20,5.26911289 20,5 C20,4.73089257 19.8915,4.47313242 19.6991,4.28499231 L15.6082,0.284989971 Z" id="Shape"></path>
                            </g>
                        </g>
                    </g>
                </g>
            </svg>
        |]

databaseControls :: Html
databaseControls = [hsx|
<div class="d-flex justify-content-end ml-auto">
    <form method="POST" action={pathTo UpdateDbAction} id="update-db-form"/>
    <form method="POST" action={pathTo PushToDbAction} id="push-to-db-form"/>
    <form method="POST" action={pathTo DumpDbAction} id="db-to-fixtures-form"/>
    <div class="btn-group btn-group-sm">
        <a
            class="btn btn-primary"
            href={NewMigrationAction}
            onclick="checkBeforeUnload()"
            >Migrate DB →</a>

        <button type="button" class="btn btn-primary dropdown-toggle dropdown-toggle-split" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
            <span class="sr-only">Toggle Dropdown</span>
        </button>

        <div class="dropdown-menu dropdown-menu-right" aria-labelledby="dropdownMenuLink">
            <button
                type="submit"
                form="update-db-form"
                class="dropdown-item"
                data-toggle="tooltip"
                data-placement="left"
                data-html="true"
                title="Dumps DB to Fixtures.sql.<br><br>Delete the DB.<br><br>Recreate using Schema.sql and Fixtures.sql"
                onclick="checkBeforeUnload()"
                >Update DB</button>

            <button
                type="submit"
                class="dropdown-item"
                form="db-to-fixtures-form"
                data-toggle="tooltip"
                data-placement="left"
                data-html="true"
                title="Saves the content of all tables to Application/Fixtures.sql"
                onclick="checkBeforeUnload()"
                >Save DB to Fixtures</button>
            <button
                type="submit"
                class="dropdown-item"
                form="push-to-db-form"
                data-toggle="tooltip"
                data-placement="left"
                data-html="true"
                title="Delete the DB and recreate using Application/Schema.sql and Application/Fixture.sql<br><br><strong class=text-danger>Save DB to Fixtures before using this to avoid data loss</strong>"
                onclick="checkBeforeUnload()"
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
visualNav = [hsx|
    <div class="view-selector">
        <div class="container-fluid">
            <a href={TablesAction} class={classes [("active", tableViewActive)]}>
                Schema Designer
            </a>

            <a href={ShowCodeAction} class={classes [("active", codeEditorActive)]}>
                Code Editor
            </a>
            <a href={MigrationsAction} class={classes [("active", migrationsActive)]}>
                Migrations
            </a>
        </div>
    </div>
|]
    where
        codeEditorActive = isActivePath ShowCodeAction
        tableViewActive = not codeEditorActive && not migrationsActive
        migrationsActive = isActiveController @MigrationsController

emptyColumnSelectorContainer = [hsx|
<div class="col-md-8 col-lg-10 column-selector d-flex">
</div>
|]

renderColumnSelector :: Text -> [(Int, Column)] -> [Statement] -> Html
renderColumnSelector tableName columns statements = [hsx|
<div class="col-md-8 col-lg-10 column-selector d-flex">
    <section class="flex-grow-1" oncontextmenu="showContextMenu('context-menu-column-root')">
        <div class="d-flex align-items-center">
            <h5>Columns</h5>
            <div class="toolbox">
                <a
                    href={NewColumnAction tableName}
                    class="btn btn-link btn-add"
                    data-toggle="tooltip"
                    data-placement="bottom"
                    title="Add Column"
                >{addIcon}</a>
            </div>
        </div>
        <table class="table table-hover table-sm">
            <tbody>
                {forEach columns (\column -> renderColumn (snd column) (fst column) tableName statements)}
            </tbody>
        </table>
        {suggestedColumnsSection tableName columns}
    </section>
    {auth}

    <section>
        {columnIndexes}
    </section>
</div>
<div class="custom-menu menu-for-column shadow backdrop-blur" id="context-menu-column-root">
    <a href={NewColumnAction tableName}>Add Column</a>
    <a href={NewPolicyAction tableName}>Add Policy</a>
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

        auth :: Html
        auth = renderPolicies tableName statements

suggestedColumnsSection :: Text -> [(Int, Column)] -> Html
suggestedColumnsSection tableName indexAndColumns = unless isUsersTable [hsx|
    <div class="mt-5">
        {mconcat suggestions}
    </div>
|]
    where
        columns :: [Column]
        columns = map snd indexAndColumns

        hasColumn :: Text -> Bool
        hasColumn name = columns |> find (\column -> get #name column == name) |> isJust

        isUsersTable = tableName == "users"

        suggestions = [createdAt, updatedAt, userId] |> catMaybes

        createdAt = if hasColumn "created_at"
                then Nothing
                else Just [hsx|
                    <form method="POST" action={CreateColumnAction} class="mb-2">
                        <input type="hidden" name="tableName" value={tableName} />
                        <input type="hidden" name="name" value="created_at"/>
                        <input type="hidden" name="columnType" value="TIMESTAMP WITH TIME ZONE"/>
                        <input type="hidden" name="primaryKey" value={inputValue False}/>
                        <input type="hidden" name="isArray" value={inputValue False}/>
                        <input type="hidden" name="defaultValue" value="NOW()"/>
                        <input type="hidden" name="allowNull" value={inputValue False}/>
                        <input type="hidden" name="isUnique" value={inputValue False}/>
                        <input type="hidden" name="isReference" value={inputValue False}/>
                        <input type="hidden" name="withIndex" value={inputValue True}/>

                        <button type="submit" class="btn btn-suggested-table">
                            <table class="table table-sm mb-0">
                                <tbody>
                                    <tr class="column">
                                        <td style="width: 40px">+</td>
                                        <td class="context-column column-name">created_at</td>
                                        <td class="context-column">TIMESTAMP WITH TIME ZONE</td>
                                        <td class="context-column">NOW()</td>
                                        <td class="context-column"></td>
                                    </tr>
                                </tbody>
                            </table>
                        </button>
                    </form>
                |]

        updatedAt = if hasColumn "updated_at"
                then Nothing
                else Just [hsx|
                    <form method="POST" action={CreateColumnAction} class="mb-2">
                        <input type="hidden" name="tableName" value={tableName} />
                        <input type="hidden" name="name" value="updated_at"/>
                        <input type="hidden" name="columnType" value="TIMESTAMP WITH TIME ZONE"/>
                        <input type="hidden" name="primaryKey" value={inputValue False}/>
                        <input type="hidden" name="isArray" value={inputValue False}/>
                        <input type="hidden" name="defaultValue" value="NOW()"/>
                        <input type="hidden" name="allowNull" value={inputValue False}/>
                        <input type="hidden" name="isUnique" value={inputValue False}/>
                        <input type="hidden" name="isReference" value={inputValue False}/>
                        <input type="hidden" name="withIndex" value={inputValue False}/>

                        <button type="submit" class="btn btn-suggested-table">
                            <table class="table table-sm mb-0">
                                <tbody>
                                    <tr class="column">
                                        <td style="width: 40px">+</td>
                                        <td class="context-column column-name">updated_at</td>
                                        <td class="context-column">TIMESTAMP WITH TIME ZONE</td>
                                        <td class="context-column">NOW()</td>
                                        <td class="context-column"></td>
                                    </tr>
                                </tbody>
                            </table>
                        </button>
                    </form>
                |]

        userId = if hasColumn "user_id"
            then Nothing
            else Just [hsx|
                <form method="POST" action={CreateColumnAction}>
                    <input type="hidden" name="tableName" value={tableName} />
                    <input type="hidden" name="name" value="user_id"/>
                    <input type="hidden" name="columnType" value="UUID"/>
                    <input type="hidden" name="primaryKey" value={inputValue False}/>
                    <input type="hidden" name="isArray" value={inputValue False}/>
                    <input type="hidden" name="defaultValue" value="ihp_user_id()"/>
                    <input type="hidden" name="allowNull" value={inputValue False}/>
                    <input type="hidden" name="isUnique" value={inputValue False}/>
                    <input type="hidden" name="isReference" value={inputValue True}/>
                    <input type="hidden" name="referenceTable" value="users"/>

                    <button type="submit" class="btn btn-suggested-table">
                        <table class="table table-sm mb-0">
                            <tbody>
                                <tr class="column">
                                    <td style="width: 40px">+</td>
                                    <td class="context-column column-name">user_id</td>
                                    <td class="context-column">UUID</td>
                                    <td class="context-column"></td>
                                    <td class="context-column">FOREIGN KEY: users</td>
                                </tr>
                            </tbody>
                        </table>
                    </button>
                </form>
            |]


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
            Just addConstraint@AddConstraint { constraint = ForeignKeyConstraint { name = Just constraintName, referenceTable } } -> [hsx|<a href={EditForeignKeyAction tableName name constraintName referenceTable} class="d-block nounderline">FOREIGN KEY: {referenceTable}</a>|]
            _ -> mempty
        foreignKeyOption = case findForeignKey statements tableName name of
            Just addConstraint@AddConstraint { constraint = ForeignKeyConstraint { name = Just constraintName, referenceTable } } ->
                [hsx|<a href={EditForeignKeyAction tableName name constraintName referenceTable}>Edit Foreign Key Constraint</a>
                <a href={DeleteForeignKeyAction constraintName tableName} class="js-delete">Delete Foreign Key Constraint</a>|]
            _ -> [hsx|<a href={NewForeignKeyAction tableName name}>Add Foreign Key Constraint</a>|]

renderColumnIndexes tableName statements = forEach (findTableIndexes statements tableName) renderIndex
    where
        renderIndex index = [hsx|
            <tr class="index">
                <td class="index-name">{get #indexName index}</td>
                <td class="index-expressions">{unique}</td>
                <td class="index-expressions">{expressions}</td>
            </tr>
        |]
            where
                unique = when (get #unique index) [hsx|UNIQUE|]
                showColumnOrder columnOrder =
                    columnOrder
                        |> map (\case { Asc -> "ASC"; Desc -> "DESC"; NullsFirst -> "NULLS FIRST"; NullsLast -> "NULLS LAST" })
                        |> unwords
                expressions = index
                    |> get #columns
                    |> map (\column -> (compileExpression $ get #column column) <> " " <> (showColumnOrder $ get #columnOrder column))
                    |> intercalate ", "



renderPolicies :: Text -> [Statement] -> Html
renderPolicies tableName statements = whenNonEmpty tablePolicies policiesTable
    where
        policiesTable = [hsx|
            <section>
                <h5>Policies</h5>
                <table class="table table-hover table-sm">
                    {forEach tablePolicies renderPolicy}
                </table>
            </section>
        |]
        tablePolicies :: [Statement]
        tablePolicies = statements
                |> filter \case
                    CreatePolicy { tableName = policyTable } -> policyTable == tableName
                    otherwise -> False

        renderPolicy policy = [hsx|
            <tr class="policy">
                <td class="policy-name" oncontextmenu={"showContextMenu('" <> contextMenuId <> "')"}>
                    <a href={EditPolicyAction tableName policyName} class="text-body nounderline">
                        {get #name policy}
                    </a>
                </td>
                {renderExpressions policy}
            </tr>
            <div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
                <a href={EditPolicyAction tableName policyName}>Edit Policy</a>
                <a href={DeletePolicyAction tableName policyName} class="js-delete">Delete Policy</a>
                <div></div>
                <a href={NewPolicyAction tableName}>Add Policy</a>
            </div>
        |]
            where
                policyName = get #name policy
                contextMenuId = "policy-" <> toSlug policyName

        renderExpressions policy = case (get #using policy, get #check policy) of
                (Just using, Just check) | using == check ->
                    [hsx|
                        <td class="policy-expression">
                            <small>read & write if</small>
                            {compileExpression using}
                        </td>
                    |]
                (using, check) ->
                    [hsx|
                        <td class="policy-expression">
                            <small>read if</small>
                            {maybe "" compileExpression using}
                        </td>
                        <td class="policy-expression">
                            <small>write if</small>
                            {maybe "" compileExpression check}
                        </td>
                    |]

renderEnumSelector :: Text -> [(Int, Text)] -> Html
renderEnumSelector enumName values = [hsx|
<div class="col-8 column-selector" oncontextmenu="showContextMenu('context-menu-value-root')">
    <div class="d-flex">
        <h5>Enum Values</h5>
        <div class="toolbox">
            <a
                href={NewEnumValueAction enumName}
                class="btn btn-link btn-add mr-1"
                data-toggle="tooltip"
                data-placement="bottom"
                title="Add Table"
                >{addIcon}</a>
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
        <div class="d-flex align-items-center pl-2">
            <h5>Tables</h5>
            <div class="toolbox">
                <a
                    href={NewTableAction}
                    class="btn btn-link btn-add mr-1"
                    data-toggle="tooltip"
                    data-placement="bottom"
                    title="Add Table"
                    >{addIcon}</a>
            </div>
        </div>
        {forEach tableStatements (\statement -> renderObject (snd statement) (fst statement))}
        {enums}

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

        tableStatements :: [(Int, Statement)]
        tableStatements = statements |> filter \case
            (_, StatementCreateTable CreateTable {}) -> True
            otherwise -> False

        enumStatements :: [(Int, Statement)]
        enumStatements = statements |> filter \case
            (_, CreateEnumType {}) -> True
            otherwise -> False

        enums = whenNonEmpty enumStatements [hsx|
            <div class="d-flex pl-2">
                <h5>Enums</h5>
            </div>
            {forEach enumStatements (\statement -> renderObject (snd statement) (fst statement))}
        |]

        renderObject :: Statement -> Int -> Html
        renderObject (StatementCreateTable CreateTable { name }) id = [hsx|
            <div class={classes [("object object-table w-100 context-table pl-3", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
                <div class="d-flex justify-content-between">
                    <a href={ShowTableAction name} class="flex-grow-1">{name}</a>

                    {when rlsEnabled rlsIcon}
                </div>
            </div>
            <div class="custom-menu menu-for-table shadow backdrop-blur" id={contextMenuId}>
                <a href={EditTableAction name id}>Rename Table</a>
                <a href={DeleteTableAction id name} class="js-delete">Delete Table</a>
                <div></div>
                <a href={ShowGeneratedCodeAction name}>Show Generated Haskell Code</a>
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

                rlsEnabled = statements
                        |> map snd
                        |> find \case
                            EnableRowLevelSecurity { tableName = rlsTable } -> rlsTable == name
                            otherwise                                       -> False
                        |> isJust

                rlsIcon = [hsx|
                        <span
                            class="rls-enabled"
                            data-toggle="tooltip"
                            data-placement="right"
                            data-html="true"
                            title="Row Level Security enabled"
                            >{shieldIcon}</span>
                        |]

        renderObject CreateEnumType { name } id = [hsx|
            <a href={ShowEnumAction name} class={classes [("object object-table w-100 context-enum pl-3", True), ("active", Just name == activeObjectName)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
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
        renderObject CreateFunction {} id = mempty
        renderObject UnknownStatement {} id = mempty
        renderObject EnableRowLevelSecurity {} id = mempty
        renderObject CreatePolicy {} id = mempty

        shouldRenderObject (StatementCreateTable CreateTable {}) = True
        shouldRenderObject CreateEnumType {} = True
        shouldRenderObject _ = False

removeQuotes :: [Char] -> Text
removeQuotes (x:xs) = cs $ fromMaybe [] (init xs)
removeQuotes n = cs n

findForeignKey :: [Statement] -> Text -> Text -> Maybe Statement
findForeignKey statements tableName columnName =
    find
        (\case
            AddConstraint { tableName = fkTable, constraint = ForeignKeyConstraint { columnName = fkColumn } } -> tableName == fkTable && columnName == fkColumn
            otherwise -> False
        )
        statements

findPrimaryKey :: [Statement] -> Text -> Maybe [Text]
findPrimaryKey statements tableName = do
    (StatementCreateTable createTable) <- find (isCreateTable tableName) statements
    pure . primaryKeyColumnNames $ get #primaryKeyConstraint createTable
    where
      isCreateTable tableName (StatementCreateTable CreateTable { name }) = name == tableName
      isCreateTable _ _ = False

findTableIndex :: [Statement] -> Text -> Maybe Statement
findTableIndex statements tableName =
    find (\case CreateIndex { tableName = tableName' } -> tableName' == tableName; otherwise -> False) statements

findTableIndexes :: [Statement] -> Text -> [Statement]
findTableIndexes statements tableName =
    filter (\case CreateIndex { tableName = tableName' } -> tableName' == tableName; otherwise -> False) statements


replace :: Int -> a -> [a] -> [a]
replace i e xs = case List.splitAt i xs of
   (before, _:after) -> before ++ (e: after)
   (a, b) -> a ++ b

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/tables/static/img/table.svg
tableIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="24" height="24"><defs><style>.cls-table-1{fill:#f2f2f2;}.cls-table-2{fill:#2195e7;}.cls-table-3{fill:none;stroke:#c1cbd5;stroke-linejoin:round;}.cls-table-3,.cls-table-4{stroke-width:0.75px;}.cls-table-4{fill:#def4fd;stroke:#2195e7;stroke-miterlimit:1;}</style></defs><title>table</title><g id="_2" data-name="2"><rect class="cls-table-1" x="2.92" y="3.65" width="10.15" height="8.71" rx="0.53" ry="0.53"/><path class="cls-table-2" d="M12.55,4a.15.15,0,0,1,.15.15v7.66a.15.15,0,0,1-.15.15H3.45a.15.15,0,0,1-.15-.15V4.17A.15.15,0,0,1,3.45,4h9.1m0-.75H3.45a.9.9,0,0,0-.9.9v7.66a.9.9,0,0,0,.9.9h9.1a.9.9,0,0,0,.9-.9V4.17a.9.9,0,0,0-.9-.9Z"/><line class="cls-table-3" x1="3.32" y1="9.43" x2="12.69" y2="9.43"/><line class="cls-table-3" x1="8.01" y1="7.09" x2="8" y2="11.97"/><line class="cls-table-4" x1="8.01" y1="4.03" x2="8" y2="6.58"/><line class="cls-table-4" x1="12.68" y1="6.74" x2="3.32" y2="6.74"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/types/static/img/type.svg
enumIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="24" height="24"><defs><style>.cls-enum-1{fill:#d6ffea;}.cls-enum-2{fill:#5acb9a;}</style></defs><title>type</title><g id="_2" data-name="2"><path class="cls-enum-1" d="M3.9,3.47h8.19a.44.44,0,0,1,.44.44V12.1a.43.43,0,0,1-.43.43H3.91a.44.44,0,0,1-.44-.44V3.9A.43.43,0,0,1,3.9,3.47Z"/><path class="cls-enum-2" d="M12.1,3.85a.06.06,0,0,1,.06.06V12.1a.06.06,0,0,1-.06.06H3.9a.06.06,0,0,1-.06-.06V3.91a.06.06,0,0,1,.06-.06H12.1m0-.75H3.9a.81.81,0,0,0-.81.81V12.1a.81.81,0,0,0,.81.81H12.1a.81.81,0,0,0,.81-.81V3.91a.81.81,0,0,0-.81-.81Z"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/tables/indexes/static/img/index.svg
indexIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-index-1,.cls-index-3{fill:#def4fd;}.cls-index-2{fill:#357fd3;}.cls-index-3,.cls-index-4{stroke:#357fd3;stroke-linecap:round;stroke-linejoin:round;stroke-width:0.75px;}.cls-index-4{fill:none;}</style></defs><title>index</title><g id="_8" data-name="8"><path class="cls-index-1" d="M5.81,3.37h4.39a.44.44,0,0,1,.44.44V5.49a.44.44,0,0,1-.44.44H5.8a.43.43,0,0,1-.43-.43V3.81A.44.44,0,0,1,5.81,3.37Z"/><path class="cls-index-2" d="M10.2,3.75a.06.06,0,0,1,.06.06V5.49a.06.06,0,0,1-.06.06H5.8a.06.06,0,0,1-.06-.06V3.81a.06.06,0,0,1,.06-.06H10.2m0-.75H5.8A.81.81,0,0,0,5,3.81V5.49a.81.81,0,0,0,.81.81H10.2A.81.81,0,0,0,11,5.49V3.81A.81.81,0,0,0,10.2,3Z"/><rect class="cls-index-1" x="11.38" y="10.57" width="2.24" height="2.06" rx="0.44" ry="0.44"/><path class="cls-index-2" d="M13.19,10.94a.06.06,0,0,1,.06.06v1.19a.06.06,0,0,1-.06.06H11.81a.06.06,0,0,1-.06-.06V11a.06.06,0,0,1,.06-.06h1.37m0-.75H11.81A.81.81,0,0,0,11,11v1.19a.81.81,0,0,0,.81.81h1.37a.81.81,0,0,0,.81-.81V11a.81.81,0,0,0-.81-.81Z"/><rect class="cls-index-1" x="2.38" y="10.57" width="2.24" height="2.06" rx="0.44" ry="0.44"/><path class="cls-index-2" d="M4.19,10.94a.06.06,0,0,1,.06.06v1.19a.06.06,0,0,1-.06.06H2.81a.06.06,0,0,1-.06-.06V11a.06.06,0,0,1,.06-.06H4.19m0-.75H2.81A.81.81,0,0,0,2,11v1.19a.81.81,0,0,0,.81.81H4.19A.81.81,0,0,0,5,12.19V11a.81.81,0,0,0-.81-.81Z"/><rect class="cls-index-1" x="6.88" y="10.57" width="2.24" height="2.06" rx="0.44" ry="0.44"/><path class="cls-index-2" d="M8.69,10.94a.06.06,0,0,1,.06.06v1.19a.06.06,0,0,1-.06.06H7.32a.06.06,0,0,1-.06-.06V11a.06.06,0,0,1,.06-.06H8.69m0-.75H7.32a.81.81,0,0,0-.81.81v1.19a.81.81,0,0,0,.81.81H8.69a.81.81,0,0,0,.81-.81V11a.81.81,0,0,0-.81-.81Z"/><line class="cls-index-3" x1="7.98" y1="6.3" x2="7.98" y2="8.54"/><line class="cls-index-3" x1="7.98" y1="8.54" x2="7.98" y2="10.19"/><polyline class="cls-index-4" points="12.5 10.19 12.5 8.17 3.5 8.17 3.5 10.19"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/tables/constraints/index_constraint/static/img/unique_constraint.svg
uniqueIndexIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-unique-index-1{fill:#cbe7f6;}.cls-unique-index-1,.cls-unique-index-2{stroke:#2c66bd;stroke-linecap:round;stroke-linejoin:round;stroke-width:0.75px;}.cls-unique-index-2{font-size:8px;fill:#2c66bd;font-family:Georgia, Georgia;}</style></defs><title>unique index</title><g id="_2" data-name="2"><circle class="cls-unique-index-1" cx="8" cy="8" r="5.4"/><text class="cls-unique-index-2" transform="translate(6.28 10.17)">1</text></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/tables/constraints/check_constraint/static/img/check-constraint.svg
constraintIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-constraint-1{fill:none;stroke:#719317;stroke-linecap:round;stroke-linejoin:round;stroke-width:2px;}</style></defs><title>constraint</title><g id="_2" data-name="2"><polyline class="cls-constraint-1" points="4 9.25 6.5 11.75 12 4.25"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/languages/static/img/language.svg
commentIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-comment-1{fill:#f8f8f8;stroke:#d5d4d5;stroke-linejoin:round;stroke-width:0.75px;}</style></defs><title>comment</title><g id="_2" data-name="2"><path class="cls-comment-1" d="M13.78,7c0,.62-.57,1.19-1.49,1.61-.39.18.07,2.59-.43,2.72s-2-2-2.53-2c-.42,0-.86.06-1.32.06C4.81,9.44,2.23,8.36,2.23,7S4.81,4.63,8,4.63,13.78,5.71,13.78,7Z"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/extensions/static/img/extension.svg
extensionIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-extension-1,.cls-extension-4{fill:#c9edd2;}.cls-extension-2{fill:#1a6016;}.cls-extension-3{fill:none;}.cls-extension-3,.cls-extension-4{stroke:#1a6016;stroke-linejoin:round;stroke-width:0.75px;}.cls-extension-4{stroke-linecap:round;}</style></defs><title>extension</title><g id="_2" data-name="2"><path class="cls-extension-1" d="M5.62,13.13A.62.62,0,0,1,5,12.51v-9a.62.62,0,0,1,.62-.62h6.61a.62.62,0,0,1,.62.62v6.66l-3,3Z"/><path class="cls-extension-2" d="M12.23,3.25a.24.24,0,0,1,.24.24V10L9.72,12.75H5.62a.24.24,0,0,1-.24-.24v-9a.24.24,0,0,1,.24-.24h6.61m0-.75H5.62a1,1,0,0,0-1,1v9a1,1,0,0,0,1,1H10l3.19-3.19V3.49a1,1,0,0,0-1-1Z"/><polyline class="cls-extension-3" points="13 9.52 9.49 9.54 9.49 13.5"/><rect class="cls-extension-4" x="2.78" y="5.13" width="6.01" height="2.01"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/browser/server_groups/servers/databases/schemas/functions/static/img/function.svg
functionIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><defs><style>.cls-function-1{fill:#edfffd;}.cls-function-2{fill:#34495e;stroke-miterlimit:10;stroke-width:0.5px;}.cls-function-2,.cls-function-3{stroke:#2f91a3;}.cls-function-3{fill:#2980b9;stroke-linecap:round;stroke-linejoin:round;stroke-width:0.75px;}</style></defs><title>function</title><g id="_2" data-name="2"><polygon class="cls-function-1" points="10.99 13.41 12.26 12.74 12.61 11.24 12.61 10.54 12.8 9.04 13.24 8.48 14.37 7.92 13.24 7.92 13.24 7.92 13.24 7.92 14.02 7.78 12.85 6.91 12.78 6.32 12.78 5.24 12.61 3.76 12.14 3.21 11.39 2.88 10.99 2.84 4.93 2.72 4.06 3.33 3.52 3.77 3.52 4.38 3.61 4.21 3.42 6.23 3.52 6.93 2 7.92 1.78 8.11 2 7.92 2.98 8.63 3.12 9.04 3.7 10.56 3.67 11.22 3.65 11.68 3.67 10.88 3.67 11.27 3.89 12.23 4.24 12.71 4.87 13.09 5.37 13.41 10.99 13.41"/><path class="cls-function-2" d="M10.57,13.2a2.46,2.46,0,0,0,1.14-.28,1.56,1.56,0,0,0,.59-.6,2,2,0,0,0,.22-.78q0-.42,0-.81v.2c0-.38,0-.7,0-1a5.15,5.15,0,0,1,.06-.67,1.53,1.53,0,0,1,.14-.46A1.32,1.32,0,0,1,13,8.51a1.4,1.4,0,0,1,.34-.26,2.21,2.21,0,0,1,.36-.15A2,2,0,0,1,14,8c.1,0,.1-.05,0-.07a2,2,0,0,1-.33-.07,2,2,0,0,1-.36-.15A1.4,1.4,0,0,1,13,7.49a1.34,1.34,0,0,1-.24-.33,1.54,1.54,0,0,1-.14-.46A5.17,5.17,0,0,1,12.56,6q0-.39,0-1v.2c0-.26,0-.53,0-.81a2,2,0,0,0-.22-.78,1.57,1.57,0,0,0-.59-.6,2.48,2.48,0,0,0-1.14-.28V2.55l.3,0a2.29,2.29,0,0,1,.37.06,2.84,2.84,0,0,1,.4.13A1.61,1.61,0,0,1,12,3a1.85,1.85,0,0,1,.46.5,2,2,0,0,1,.24.59,3.87,3.87,0,0,1,.09.72c0,.26,0,1,0,1a4.39,4.39,0,0,0,.07.83,1.32,1.32,0,0,0,.26.59,1.47,1.47,0,0,0,.55.41.38.38,0,0,1,0,.65,1.48,1.48,0,0,0-.55.4,1.33,1.33,0,0,0-.26.59,4.39,4.39,0,0,0-.07.83s-.05,1.54-.1,1.76a2,2,0,0,1-.24.59A1.84,1.84,0,0,1,12,13a1.61,1.61,0,0,1-.39.23,3,3,0,0,1-.4.13,2.41,2.41,0,0,1-.37.06l-.3,0Z"/><path class="cls-function-2" d="M5.66,2.8a2.46,2.46,0,0,0-1.14.28,1.56,1.56,0,0,0-.59.6,2,2,0,0,0-.22.78q0,.42,0,.81v-.2c0,.38,0,.7,0,1a5.15,5.15,0,0,1-.06.67,1.53,1.53,0,0,1-.14.46,1.32,1.32,0,0,1-.24.33,1.4,1.4,0,0,1-.34.26,2.21,2.21,0,0,1-.36.15A2,2,0,0,1,2.18,8c-.1,0-.1.05,0,.07a2,2,0,0,1,.33.07,2,2,0,0,1,.36.15,1.4,1.4,0,0,1,.34.26,1.34,1.34,0,0,1,.24.33,1.54,1.54,0,0,1,.14.46,5.17,5.17,0,0,1,.06.67q0,.39,0,1v-.2c0,.26,0,.53,0,.81a2,2,0,0,0,.22.78,1.57,1.57,0,0,0,.59.6,2.48,2.48,0,0,0,1.14.28v.24l-.3,0A2.29,2.29,0,0,1,5,13.36a2.84,2.84,0,0,1-.4-.13A1.61,1.61,0,0,1,4.2,13a1.85,1.85,0,0,1-.46-.5,2,2,0,0,1-.24-.59,3.87,3.87,0,0,1-.09-.72c0-.26,0-1,0-1a4.39,4.39,0,0,0-.07-.83,1.32,1.32,0,0,0-.26-.59,1.47,1.47,0,0,0-.55-.41.38.38,0,0,1,0-.65,1.48,1.48,0,0,0,.55-.4,1.33,1.33,0,0,0,.26-.59,4.39,4.39,0,0,0,.07-.83s.05-1.54.1-1.76a2,2,0,0,1,.24-.59A1.84,1.84,0,0,1,4.2,3a1.61,1.61,0,0,1,.39-.23A3,3,0,0,1,5,2.64a2.41,2.41,0,0,1,.37-.06l.3,0Z"/><line class="cls-function-3" x1="6.32" y1="5.41" x2="10.07" y2="5.41"/><line class="cls-function-3" x1="6.32" y1="7.91" x2="10.07" y2="7.91"/><line class="cls-function-3" x1="6.32" y1="10.41" x2="8.82" y2="10.41"/></g></svg>|]

-- | https://github.com/postgres/pgadmin4/blob/master/web/pgadmin/misc/static/explain/img/ex_unknown.svg
unknownIcon = preEscapedToHtml [plain|<svg id="_1" data-name="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"><defs><style>.cls-unknown-1{fill:#1cafe4;}</style></defs><title>unknown</title><path class="cls-unknown-1" d="M23.34,13.2a14.76,14.76,0,0,1,8.16-2.14,17.57,17.57,0,0,1,10.56,3q4.2,3,4.2,9a10.18,10.18,0,0,1-1.82,6.16,21,21,0,0,1-4.09,3.88l-2,1.55a6,6,0,0,0-2.16,3A12.5,12.5,0,0,0,35.83,41H28.2a22.11,22.11,0,0,1,.9-6.57,11.57,11.57,0,0,1,3.76-4.18l2-1.6A7.17,7.17,0,0,0,36.53,27a5.67,5.67,0,0,0,1.12-3.4,6.58,6.58,0,0,0-1.25-3.9q-1.25-1.76-4.56-1.76t-4.62,2.17a8.34,8.34,0,0,0-1.36,4.5H17.74Q18.08,16.55,23.34,13.2ZM28,44.81h8.41v8.13H28Z"/></svg>|]

-- | https://fonts.google.com/icons?icon.query=shield
shieldIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" enable-background="new 0 0 24 24" height="1rem" viewBox="0 0 24 24" width="1rem" fill="currentColor"><g><rect fill="none" height="24" width="24"/></g><g><path d="M12,2L4,5v6.09c0,5.05,3.41,9.76,8,10.91c4.59-1.15,8-5.86,8-10.91V5L12,2z M18,11.09c0,4-2.55,7.7-6,8.83 c-3.45-1.13-6-4.82-6-8.83v-4.7l6-2.25l6,2.25V11.09z"/></g></svg>|]
