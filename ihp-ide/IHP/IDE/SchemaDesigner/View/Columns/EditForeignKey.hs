module IHP.IDE.SchemaDesigner.View.Columns.EditForeignKey where

import IHP.ViewPrelude
import IHP.Postgres.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.View.Columns.NewForeignKey (foreignKeyFormModal)

data EditForeignKeyView = EditForeignKeyView
    { statements :: [Statement]
    , tableName :: Text
    , columnName :: Text
    , tableNames :: [Text]
    , referenceTable :: Text
    , constraintName :: Text
    , onDelete :: Text
    }

instance View EditForeignKeyView where
    html EditForeignKeyView { .. } = [hsx|
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
                (pathTo UpdateForeignKeyAction)
                tableName
                columnName
                tableNames
                constraintName
                (Just referenceTable)
                onDelete
                "Edit Constraint"
                "Edit Foreign Key Constraint"
