module TurboHaskell.IDE.SchemaDesigner.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.SchemaDesigner.View
import TurboHaskell.IDE.SchemaDesigner.View
import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types

instance Controller SchemaDesignerController where
    action TablesAction = do
        statements <- readSchema
        render IndexView { .. }

    action ShowTableAction { tableName } = do
        let name = tableName
        statements <- readSchema
        render ShowView { .. }

    action NewColumnAction { tableName } = do
        statements <- readSchema
        render NewColumnView { .. }

    action CreateColumnAction = do
        let tableName = param "tableName"
        let column = Column
                    { name = param "name"
                    , columnType = param "columnType"
                    , primaryKey = (param "primaryKey")
                    , defaultValue = Nothing
                    , notNull = (not (param "allowNull"))
                    }
        updateSchema (map (addColumnToTable tableName column))

        redirectTo ShowTableAction { .. }

readSchema :: _ => _
readSchema = parseSchemaSql >>= \case
        Left error -> do renderPlain error; pure []
        Right statements -> pure statements

updateSchema :: _ => _
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    writeSchema statements'

addColumnToTable :: Text -> Column -> Statement -> Statement
addColumnToTable tableName column (table@CreateTable { name, columns }) | name == tableName =
    table { columns = columns <> [column] }
addColumnToTable tableName column statement = statement