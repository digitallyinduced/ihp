module IHP.IDE.SchemaDesigner.Controller.Indexes where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Indexes.Edit

import IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout)
import IHP.IDE.SchemaDesigner.Controller.Helper

import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

instance Controller IndexesController where
    beforeAction = setLayout schemaDesignerLayout

    action EditIndexAction { tableName, indexName } = do
        statements <- readSchema
        render EditIndexView { .. }
    
    action UpdateIndexAction { tableName, indexName } = do
        statements <- readSchema
        let options = SchemaOperations.UpdateIndexOptions
                { indexName = indexName
                , newIndexName = param "newIndexName"
                , indexColumns = param "indexColumns"
                }
        updateSchema (SchemaOperations.updateIndex options)
        redirectTo ShowTableAction { .. }

    action CreateIndexAction { tableName, columnName } = do
        statements <- readSchema
        let options = SchemaOperations.AddIndexOptions { tableName, columnName }
        updateSchema (SchemaOperations.addIndex options)
        redirectTo ShowTableAction { .. }

    action DeleteIndexAction { tableName, indexName } = do
        statements <- readSchema
        updateSchema (SchemaOperations.deleteIndex indexName)
        redirectTo ShowTableAction { .. }
