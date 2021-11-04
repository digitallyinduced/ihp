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
import qualified IHP.IDE.SchemaDesigner.MigrationChangeTracker as MigrationChangeTracker

instance Controller PoliciesController where
    beforeAction = setLayout schemaDesignerLayout

    action NewPolicyAction { tableName } = do
        statements <- readSchema
        let (Just table) = findStatementByName tableName statements
        let policy = suggestPolicy table
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
        
        updateSchema $ SchemaOperations.addPolicy SchemaOperations.AddPolicyOptions
                { tableName = tableName
                , name = param "policyName"
                , using = param "using"
                , check = param "check"
                }

        redirectTo ShowTableAction { .. }

    action DeletePolicyAction { tableName, policyName } = do
        updateSchema $ SchemaOperations.deletePolicy SchemaOperations.DeletePolicyOptions { tableName, policyName }

        redirectTo ShowTableAction { .. }


-- | Checks if there exists a @user_id@ column, and returns a policy based on that.
-- If there's no @user_id@ field on the table it will return an empty policy
--
-- TODO: In the future this function should follow foreign keys to find the shortest path to a user_id.
-- E.g. when having a schema post_meta_tags (no user_id column) <-> posts (has a user_id) <-> users:
--
-- >                                                 post_id
-- >                            posts_meta_infos ────────────────►  posts
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │ user_id
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                          users  ◄───────────────┘
--
suggestPolicy :: Statement -> Statement
suggestPolicy (StatementCreateTable CreateTable { name = tableName, columns })
    | isJust (find isUserIdColumn columns)  = CreatePolicy
        { name = "Users can manage their " <> tableName
        , tableName
        , using = Just compareUserId
        , check = Just compareUserId
        }
    where
        compareUserId = EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" [])
suggestPolicy (StatementCreateTable CreateTable { name = tableName }) = CreatePolicy { name = "", tableName, using = Nothing, check = Nothing }

isUserIdColumn :: Column -> Bool
isUserIdColumn Column { name = "user_id" } = True
isUserIdColumn otherwise                   = False