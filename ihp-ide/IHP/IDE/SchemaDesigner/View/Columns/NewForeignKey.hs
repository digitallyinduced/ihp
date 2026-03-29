module IHP.IDE.SchemaDesigner.View.Columns.NewForeignKey (NewForeignKeyView (..), foreignKeyFormModal) where

import IHP.ViewPrelude
import IHP.Postgres.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data NewForeignKeyView = NewForeignKeyView
    { statements :: [Statement]
    , tableName :: Text
    , columnName :: Text
    , tableNames :: [Text]
    }

instance View NewForeignKeyView where
    html NewForeignKeyView { .. } = [hsx|
        <div class="row g-0 bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName  (zip [0..] columns) statements}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            columns = getTableColumns tableName statements
            modal = foreignKeyFormModal
                (pathTo CreateForeignKeyAction)
                tableName
                columnName
                tableNames
                (tableName <> "_ref_" <> columnName)
                Nothing
                "NoAction"
                "Add Constraint"
                "New Foreign Key Constraint"

-- | Shared form modal for creating and editing foreign key constraints.
foreignKeyFormModal
    :: (?context :: ControllerContext, ?request :: Request)
    => Text           -- ^ Form action URL
    -> Text           -- ^ Table name
    -> Text           -- ^ Column name
    -> [Text]         -- ^ Available table names
    -> Text           -- ^ Constraint name value
    -> Maybe Text     -- ^ Selected reference table (Nothing = no preselection)
    -> Text           -- ^ Selected onDelete value
    -> Text           -- ^ Button text
    -> Text           -- ^ Modal title
    -> Modal
foreignKeyFormModal formAction tableName columnName tableNames constraintNameValue selectedReferenceTable selectedOnDelete buttonText modalTitle =
    Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
    where
        modalContent = [hsx|
            <form method="POST" action={formAction}>
                <input type="hidden" name="tableName" value={tableName}/>
                <input type="hidden" name="columnName" value={columnName}/>

                <div class="mb-3 row">
                    <label class="col-sm-2 col-form-label">Reference Table:</label>
                    <div class="col-sm-10">
                        <select name="referenceTable" class="form-control select2" autofocus="autofocus">
                            {forEach tableNames renderTableNameSelector}
                        </select>
                    </div>
                </div>

                <div class="mb-3 row">
                    <label class="col-sm-2 col-form-label">Name:</label>
                    <div class="col-sm-10">
                        <input name="constraintName" type="text" class="form-control" value={constraintNameValue}/>
                    </div>
                </div>

                <div class="mb-3 row">
                    <label class="col-sm-2 col-form-label">On Delete:</label>
                    <div class="col-sm-10">
                        <select name="onDelete" class="form-control select2">
                            {onDeleteSelector "NoAction"}
                            {onDeleteSelector "Restrict"}
                            {onDeleteSelector "SetNull"}
                            {onDeleteSelector "SetDefault"}
                            {onDeleteSelector "Cascade"}
                        </select>
                    </div>
                </div>

                <div class="text-end">
                    <button type="submit" class="btn btn-primary">{buttonText}</button>
                </div>
            </form>
            {select2}
        |]
            where
                renderTableNameSelector name = case selectedReferenceTable of
                    Just selected | name == selected ->
                        preEscapedToHtml [plain|<option selected>#{name}</option>|]
                    _ -> preEscapedToHtml [plain|<option>#{name}</option>|]
                onDeleteSelector option = if option == selectedOnDelete
                    then preEscapedToHtml [plain|<option selected>#{option}</option>|]
                    else preEscapedToHtml [plain|<option>#{option}</option>|]
                select2 = preEscapedToHtml [plain|
                    <script>
                        $('.select2').select2();
                    </script>
                |]
        modalFooter = mempty
        modalCloseUrl = pathTo ShowTableAction { tableName }
