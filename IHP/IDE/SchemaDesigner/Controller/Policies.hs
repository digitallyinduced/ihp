module IHP.IDE.SchemaDesigner.Controller.Policies where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Policies.New
import IHP.IDE.SchemaDesigner.View.Policies.Edit

import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (schemaDesignerLayout, findStatementByName, replace, findForeignKey, findTableIndex)
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Controller.Validation

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

instance Controller PoliciesController where
    beforeAction = setLayout schemaDesignerLayout

    action NewPolicyAction { tableName } = do
        statements <- readSchema
        let (Just table) = findStatementByName tableName statements
        let policy = SchemaOperations.suggestPolicy statements table
        let columns = get #columns $ unsafeGetCreateTable table
        render NewPolicyView { .. }

    action EditPolicyAction { tableName, policyName } = do
        statements <- readSchema
        let (Just policy) = statements
                |> find \case
                    CreatePolicy { name = policyName', tableName = tableName' } -> policyName' == policyName && tableName' == tableName
                    otherwise                                                   -> False
        let table = findStatementByName tableName statements
        let columns = maybe [] (get #columns . unsafeGetCreateTable) table

        render EditPolicyView { .. }

    action UpdatePolicyAction = do
        statements <- readSchema
        let tableName = param "tableName"
        let name = param "name"

        
        updateSchema $ SchemaOperations.updatePolicy SchemaOperations.UpdatePolicyOptions
                { currentName = name
                , tableName = tableName
                , name = param "policyName"
                , using = param "using"
                , check = param "check"
                }

        redirectTo ShowTableAction { .. }
    
    action CreatePolicyAction = do
        statements <- readSchema
        let tableName = param "tableName"
        
        let addPolicy = SchemaOperations.addPolicy SchemaOperations.AddPolicyOptions
                { tableName = tableName
                , name = param "policyName"
                , using = param "using"
                , check = param "check"
                }
        let enableRLS = SchemaOperations.enableRowLevelSecurity tableName

        updateSchema (enableRLS . addPolicy)

        redirectTo ShowTableAction { .. }

    action DeletePolicyAction { tableName, policyName } = do
        let deletePolicy = SchemaOperations.deletePolicy SchemaOperations.DeletePolicyOptions { tableName, policyName }
        let disableRLSIfNoPolicies = SchemaOperations.disableRowLevelSecurityIfNoPolicies tableName
        updateSchema (disableRLSIfNoPolicies . deletePolicy)

        redirectTo ShowTableAction { .. }