module IHP.IDE.SchemaDesigner.View.Migrations.Index where
import IHP.ViewPrelude
import IHP.IDE.ToolServer.Helper.View
import IHP.SchemaMigration
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes

import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Clock

type Revision = Int
data IndexView = IndexView
    { migrationsWithSql :: ![(Migration, Text)]
    , migratedRevisions :: ![Int]
    , lastError :: !(Maybe Text)
    }

instance View IndexView where
    html IndexView { migrationsWithSql = [] } = emptyState
    html IndexView { .. } = [hsx|
        <div class="pt-3 flex-grow-1" oncontextmenu="showContextMenu('context-menu-migrations')">
            <div class="d-flex align-items-center mb-3 pr-2">
                <a
                    href={NewMigrationAction}
                    class="ml-auto btn btn-link btn-add"
                    data-toggle="tooltip"
                    data-placement="bottom"
                    title="Add Migration"
                    >
                    {addIcon}
                </a>
            </div>
            <div class="mx-3">{renderFlashMessages}</div>
            {forEachWithIndex migrationsWithSql renderMigration}
        </div>

        {migrationsContextMenu}
    |]
        where
            renderMigration :: (Int, (Migration, Text)) -> Html
            renderMigration (index, (migration, sqlStatements)) = [hsx|
                <div class={classes ["migration mx-3", ("is-completed", not pending), ("pending", pending)]} oncontextmenu={"showContextMenu('" <> contextMenuId <> "'); event.stopPropagation();"}>
                    <div class="migration-actions d-flex justify-content-end align-items-center">
                        {runOrStatus}
                        {when pending editAndDelete}
                    </div>

                    {code "sql" sqlStatements}
                </div>
                {migrationContextMenu migration contextMenuId pending}
            |]
                where
                    pending = (get #revision migration) `notElem` migratedRevisions
                    currentError = lastError

                    contextMenuId :: Text
                    contextMenuId = "context-menu-migration-" <> tshow (get #revision migration)

                    editAndDelete = [hsx|
                        <div class="d-flex">
                            <a
                                href={EditMigrationAction (get #revision migration)}
                                class="btn btn-link btn-add"
                                data-toggle="tooltip"
                                data-placement="bottom"
                                title="Edit Migration"
                            >{editIcon}</a>

                            <a
                                href={DeleteMigrationAction (get #revision migration)}
                                class="btn btn-link btn-add js-delete"
                                data-toggle="tooltip"
                                data-placement="bottom"
                                title="Delete Migration"
                            >{deleteIcon}</a>
                        </div>
                    |]


                    currentErrorHtml = unless (isNothing currentError) [hsx|
                        <div class="text-danger font-weight-bold mr-2">
                            {currentError}
                        </div>
                    |]

                    runOrStatus :: Html
                    runOrStatus =
                        if pending
                                then [hsx|
                                <form method="POST" action={RunMigrationAction (get #revision migration)} class="mr-2 d-flex align-items-center">
                                    {currentErrorHtml}
                                    <button
                                        class="btn btn-secondary migration-run-button"
                                    >{if isJust currentError then "Retry" else "Run" :: Text}</button>
                                </form>
                            |]
                            else [hsx|
                                <div class="d-flex justify-content-end mb-2">
                                    <span class="text-muted mr-2 user-select-none" style="opacity: 0.5">{timeAgo revisionTime}</span>
                                    <strong class="text-success">
                                        {checkmark}
                                    </strong>
                                </div>
                            |]

                    revisionTime :: UTCTime
                    revisionTime = Clock.posixSecondsToUTCTime $ (fromInteger (fromIntegral (get #revision migration)))


code :: Text -> Text -> Html
code _ src = [hsx|
    <div class="source-code">
        <pre><code class="language-sql">{src}</code></pre>
    </div>
    
|]

checkmark = preEscapedToHtml [plain|
<svg width="16px" height="12px" viewBox="0 0 16 12" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
    <g id="Migrations" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
        <g id="Mig-Redesign-with-messages" transform="translate(-1401.000000, -607.000000)" fill="#059053">
            <g id="Task-10---24px" transform="translate(1401.000000, 607.000000)">
                <path d="M15.6987017,0.284560762 C16.0938017,0.670430762 16.1013017,1.30355076 15.7155017,1.69867076 L5.94984168,11.6987008 C5.75306168,11.9002008 5.48044168,12.0093008 5.19897168,11.9994008 C4.91751168,11.9894008 4.65330168,11.8612008 4.47128168,11.6463008 L0.23691168,6.64630076 C-0.12001832,6.22480076 -0.0676983198,5.59380076 0.35376168,5.23690076 C0.77521168,4.88000076 1.40621168,4.93230076 1.76314168,5.35370076 L5.28704168,9.51480076 L14.2846017,0.301330762 C14.6704017,-0.0937992379 15.3036017,-0.101309238 15.6987017,0.284560762 Z" id="Path"></path>
            </g>
        </g>
    </g>
</svg>
|]

emptyState :: Html
emptyState = [hsx|
    <div class="d-flex flex-column w-100 h-100 justify-content-center user-select-none" oncontextmenu="showContextMenu('context-menu-migrations')">
        <div class="text-center">
            <p class="text-muted">
                No Migration yet.
            </p>

            <p>
                <a href={NewMigrationAction} class="btn btn-secondary">+ New Migration</a>
            </p>
        </div>
    </div>
    {migrationsContextMenu}
|]


migrationsContextMenu :: Html
migrationsContextMenu = [hsx|
    <div class="custom-menu menu-for-table shadow backdrop-blur" id="context-menu-migrations">
        <a href={NewMigrationAction}>Add Migration</a>
    </div>
|]


migrationContextMenu migration contextMenuId pending = [hsx|
    <div class="custom-menu menu-for-table shadow backdrop-blur" id={contextMenuId}>
        {currentMigrationActions}
        <a href={NewMigrationAction}>Add Migration</a>
    </div>
|]
    where
        migrationId :: Int
        migrationId = get #revision migration

        currentMigrationActions = when pending [hsx|
            <a href={EditMigrationAction migrationId}>Edit Migration</a>
            <a href={DeleteMigrationAction migrationId} class="js-delete">Delete Migration</a>
            <div></div>
        |]