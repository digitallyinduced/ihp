module Test.IDE.SchemaDesigner.SchemaOperationsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified Text.Megaparsec as Megaparsec

tests = do
    describe "IHP.IDE.SchemaDesigner.SchemaOperations" do
        let tableA = StatementCreateTable CreateTable { name = "a", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        let tableB = StatementCreateTable CreateTable { name = "b", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        let enumA = CreateEnumType { name = "enumA", values = [] }
        let enumB = CreateEnumType { name = "enumB", values = [] }
        let comment = Comment { content = "comment" }
        describe "addEnum" do
            it "should prepend the enum" do
                let inputSchema = [tableA, tableB]
                let expectedSchema = [enumA, tableA, tableB ]

                (SchemaOperations.addEnum "enumA" inputSchema) `shouldBe` expectedSchema

            it "should prepend the enum, but after existing enums" do

                let inputSchema = [enumA, tableA]
                let expectedSchema = [enumA, enumB, tableA]

                (SchemaOperations.addEnum "enumB" inputSchema) `shouldBe` expectedSchema
            
            it "should deal with the empty case" do
                let inputSchema = []
                let expectedSchema = [enumA]

                (SchemaOperations.addEnum "enumA" inputSchema) `shouldBe` expectedSchema

            it "should ignore comments" do
                let inputSchema = [comment, enumA, tableA]
                let expectedSchema = [comment, enumA, enumB, tableA]

                (SchemaOperations.addEnum "enumB" inputSchema) `shouldBe` expectedSchema

        describe "enableRowLevelSecurity" do
            it "should enable row level security if not enabled yet" do
                let inputSchema = [tableA]
                let expectedSchema = [tableA, EnableRowLevelSecurity { tableName = "a"} ]

                (SchemaOperations.enableRowLevelSecurity "a" inputSchema) `shouldBe` expectedSchema
            
            it "should not do anything if already enabled" do
                let inputSchema = [tableA, EnableRowLevelSecurity { tableName = "a"} ]
                let expectedSchema = [tableA, EnableRowLevelSecurity { tableName = "a"} ]

                (SchemaOperations.enableRowLevelSecurity "a" inputSchema) `shouldBe` expectedSchema
        
        describe "disableRowLevelSecurity" do
            it "should disable row level security if enabled" do
                let inputSchema = [tableA, EnableRowLevelSecurity { tableName = "a"}]
                let expectedSchema = [tableA]

                (SchemaOperations.disableRowLevelSecurity "a" inputSchema) `shouldBe` expectedSchema
            
            it "should not do anything if the row level security is not enabled" do
                let inputSchema = [tableA]
                let expectedSchema = [tableA]

                (SchemaOperations.disableRowLevelSecurity "a" inputSchema) `shouldBe` expectedSchema
        
        describe "disableRowLevelSecurityIfNoPolicies" do
            it "should disable row level security if there's no policy" do
                let inputSchema = [tableA, EnableRowLevelSecurity { tableName = "a"}]
                let expectedSchema = [tableA]

                (SchemaOperations.disableRowLevelSecurityIfNoPolicies "a" inputSchema) `shouldBe` expectedSchema
            
            it "should not do anything if the row level security is not enabled" do
                let inputSchema = [tableA]

                (SchemaOperations.disableRowLevelSecurityIfNoPolicies "a" inputSchema) `shouldBe` inputSchema
            
            it "should not do anything if there's a policy" do
                let policy = CreatePolicy { tableName = "a", action = Nothing, name = "p", check = Nothing, using = Nothing }
                let inputSchema = [tableA, EnableRowLevelSecurity { tableName = "a"}, policy]

                (SchemaOperations.disableRowLevelSecurityIfNoPolicies "a" inputSchema) `shouldBe` inputSchema

        describe "deleteTable" do
            it "delete a table with all it's indices, constraints, policies, enable RLS statements, triggers" do
                let inputSchema = parseSqlStatements [trimming|
                    CREATE TABLE users ();
                    CREATE TABLE tasks ();
                    CREATE INDEX tasks_user_id_index ON tasks (user_id);
                    ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                    CREATE POLICY "Users can manage their tasks" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                    ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
                    CREATE TRIGGER update_tasks_updated_at BEFORE UPDATE ON tasks FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
                |]
                let outputSchema = parseSqlStatements [trimming|
                    CREATE TABLE users ();
                |]

                SchemaOperations.deleteTable "tasks" inputSchema `shouldBe` outputSchema

        describe "suggestPolicy" do
            it "should suggest a policy if a user_id column exists" do
                let table = StatementCreateTable CreateTable
                                {
                                name = "posts"
                                , columns =
                                    [ Column { name = "user_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema = [table]
                let expectedPolicy = CreatePolicy
                        { name = "Users can manage their posts"
                        , action = Nothing
                        , tableName = "posts"
                        , using = Just (EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" []))
                        , check = Just (EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" []))
                        }

                SchemaOperations.suggestPolicy schema table `shouldBe` expectedPolicy

            it "should suggest an empty policy if no user_id column exists" do
                let table = StatementCreateTable CreateTable
                                {
                                name = "posts"
                                , columns =
                                    [ Column { name = "title", columnType = PText, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema = [table]
                let expectedPolicy = CreatePolicy
                        { name = ""
                        , action = Nothing
                        , tableName = "posts"
                        , using = Nothing
                        , check = Nothing
                        }

                SchemaOperations.suggestPolicy schema table `shouldBe` expectedPolicy

            it "should suggest a policy if it can find a one hop path to a user_id column" do
                let tasksTable = StatementCreateTable CreateTable
                                { name = "tasks"
                                , columns =
                                    [ Column { name = "task_list_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let taskListsTable = StatementCreateTable CreateTable
                                { name = "task_lists"
                                , columns =
                                    [ Column { name = "user_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema =
                            [ tasksTable
                            , taskListsTable
                            , AddConstraint { tableName = "tasks", constraint = ForeignKeyConstraint { name = "tasks_ref_task_lists", columnName = "task_list_id", referenceTable = "task_lists", referenceColumn = Nothing, onDelete = Nothing }, deferrable = Nothing, deferrableType = Nothing }
                            ]
                let expectedPolicy = CreatePolicy
                        { name = "Users can manage the tasks if they can see the TaskList"
                        , action = Nothing
                        , tableName = "tasks"
                        , using = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "task_lists", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "task_lists") "id") (DotExpression (VarExpression "tasks") "task_list_id")})))
                        , check = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "task_lists", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "task_lists") "id") (DotExpression (VarExpression "tasks") "task_list_id")})))
                        }

                SchemaOperations.suggestPolicy schema tasksTable `shouldBe` expectedPolicy
        describe "addColumn" do
            it "should add an index if withIndex = true" do
                let inputSchema = [tableA]

                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "created_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }
                let index = CreateIndex { indexName = "a_created_at_index", unique = False, tableName = "a", columns = [IndexColumn { column =  VarExpression "created_at", columnOrder = [] }], whereClause = Nothing, indexType = Nothing }

                let expectedSchema = [tableAWithCreatedAt, index]
                
                let options = SchemaOperations.AddColumnOptions
                        { tableName = "a"
                        , columnName = "created_at"
                        , columnType = PTimestampWithTimezone
                        , defaultValue = Just (CallExpression "NOW" [])
                        , isArray = False
                        , allowNull = False
                        , isUnique = False
                        , isReference = False
                        , referenceTable = Nothing
                        , primaryKey = False
                        , withIndex = True
                        , autoPolicy = False
                        }

                (SchemaOperations.addColumn options inputSchema) `shouldBe` expectedSchema
            
            it "should add a trigger to updated_at columns" do
                let inputSchema = [tableA]

                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "updated_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let function = CreateFunction
                            { functionName = "set_updated_at_to_now"
                            , functionArguments = []
                            , functionBody = "\nBEGIN\n    NEW.updated_at = NOW();\n    RETURN NEW;\nEND;\n"
                            , orReplace = False
                            , returns = PTrigger
                            , language = "plpgsql"
                            }
                let trigger = CreateTrigger
                            { name = "update_a_updated_at"
                            , eventWhen = Before
                            , event = TriggerOnUpdate
                            , tableName = "a"
                            , for = ForEachRow
                            , whenCondition = Nothing
                            , functionName = "set_updated_at_to_now"
                            , arguments = []
                            }

                let expectedSchema = [function, tableAWithCreatedAt, trigger]
                
                let options = SchemaOperations.AddColumnOptions
                        { tableName = "a"
                        , columnName = "updated_at"
                        , columnType = PTimestampWithTimezone
                        , defaultValue = Just (CallExpression "NOW" [])
                        , isArray = False
                        , allowNull = False
                        , isUnique = False
                        , isReference = False
                        , referenceTable = Nothing
                        , primaryKey = False
                        , withIndex = False
                        , autoPolicy = False
                        }

                (SchemaOperations.addColumn options inputSchema) `shouldBe` expectedSchema
            
            it "should add a policy if autoPolicy = true" do
                let inputSchema = [tableA]

                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "user_id"
                                        , columnType = PUUID
                                        , defaultValue = Nothing
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let index = CreateIndex
                        { indexName = "a_user_id_index"
                        , unique = False
                        , tableName = "a"
                        , columns = [IndexColumn { column = VarExpression "user_id", columnOrder = [] }]
                        , whereClause = Nothing
                        , indexType = Nothing
                        }
                let constraint = AddConstraint
                        { tableName = "a"
                        , constraint = ForeignKeyConstraint { name = Just "a_ref_user_id", columnName = "user_id", referenceTable = "users", referenceColumn = Just "id", onDelete = Just NoAction }
                        , deferrable = Nothing
                        , deferrableType = Nothing
                        }
                let enableRLS = EnableRowLevelSecurity { tableName = "a" }
                let policy = CreatePolicy
                        { name = "Users can manage their a"
                        , tableName = "a"
                        , action = Nothing
                        , using = Just (EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" []))
                        , check = Just (EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" []))
                        }

                let expectedSchema = [tableAWithCreatedAt, index, constraint, enableRLS, policy]
                
                let options = SchemaOperations.AddColumnOptions
                        { tableName = "a"
                        , columnName = "user_id"
                        , columnType = PUUID
                        , defaultValue = Nothing
                        , isArray = False
                        , allowNull = False
                        , isUnique = False
                        , isReference = True
                        , referenceTable = Just "users"
                        , primaryKey = False
                        , withIndex = False
                        , autoPolicy = True
                        }

                (SchemaOperations.addColumn options inputSchema) `shouldBe` expectedSchema

        describe "deleteColumn" do
            it "should delete an referenced index" do
                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "created_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }
                let index = CreateIndex { indexName = "a_created_at_index", unique = False, tableName = "a", columns = [IndexColumn { column =  VarExpression "created_at", columnOrder = [] }], whereClause = Nothing, indexType = Nothing }

                let inputSchema = [tableAWithCreatedAt, index]
                let expectedSchema = [tableA]
                
                let options = SchemaOperations.DeleteColumnOptions
                        { tableName = "a"
                        , columnName = "created_at"
                        , columnId = 0
                        }

                (SchemaOperations.deleteColumn options inputSchema) `shouldBe` expectedSchema
            
            it "should delete a updated_at trigger" do
                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "updated_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let function = CreateFunction
                            { functionName = "set_updated_at_to_now"
                            , functionArguments = []
                            , functionBody = "\nBEGIN\n    NEW.updated_at = NOW();\n    RETURN NEW;\nEND;\n"
                            , orReplace = False
                            , returns = PTrigger
                            , language = "plpgsql"
                            }
                let trigger = CreateTrigger
                            { name = "update_a_updated_at"
                            , eventWhen = Before
                            , event = TriggerOnUpdate
                            , tableName = "a"
                            , for = ForEachRow
                            , whenCondition = Nothing
                            , functionName = "set_updated_at_to_now"
                            , arguments = []
                            }

                let inputSchema = [function, tableAWithCreatedAt, trigger]
                let expectedSchema = [function, tableA]
                
                let options = SchemaOperations.DeleteColumnOptions
                        { tableName = "a"
                        , columnName = "updated_at"
                        , columnId = 0
                        }

                (SchemaOperations.deleteColumn options inputSchema) `shouldBe` expectedSchema
        describe "update" do
            it "update a column's name, type, default value and not null" do
                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "updated_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let tableAWithUpdatedColumn = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "created_at2"
                                        , columnType = PText
                                        , defaultValue = Nothing
                                        , notNull = False
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let inputSchema = [tableAWithCreatedAt]
                let expectedSchema = [tableAWithUpdatedColumn]
                
                let options = SchemaOperations.UpdateColumnOptions
                        { tableName = "a"
                        , columnName = "created_at2"
                        , columnType = PText
                        , defaultValue = Nothing
                        , isArray = False
                        , allowNull = True
                        , isUnique = False
                        , primaryKey = False
                        , columnId = 0
                        }

                (SchemaOperations.updateColumn options inputSchema) `shouldBe` expectedSchema
            it "updates a primary key" do
                let tableWithPK = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "id2"
                                        , columnType = PUUID
                                        , defaultValue = Nothing
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                            , constraints = []
                            }

                let tableWithoutPK = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "id"
                                        , columnType = PUUID
                                        , defaultValue = Nothing
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }

                let inputSchema = [tableWithoutPK]
                let expectedSchema = [tableWithPK]
                
                let options = SchemaOperations.UpdateColumnOptions
                        { tableName = "a"
                        , columnName = "id2"
                        , columnType = PUUID
                        , defaultValue = Nothing
                        , isArray = False
                        , allowNull = False
                        , isUnique = False
                        , primaryKey = True
                        , columnId = 0
                        }

                (SchemaOperations.updateColumn options inputSchema) `shouldBe` expectedSchema
            it "updates referenced foreign key constraints" do
                let tasksTable = StatementCreateTable CreateTable
                                { name = "tasks"
                                , columns =
                                    [ Column { name = "task_list_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let taskListsTable = StatementCreateTable CreateTable
                                { name = "task_lists"
                                , columns =
                                    [ Column { name = "user_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let inputSchema =
                            [ tasksTable
                            , taskListsTable
                            , AddConstraint { tableName = "tasks", constraint = ForeignKeyConstraint { name = "tasks_ref_task_lists", columnName = "task_list_id", referenceTable = "task_lists", referenceColumn = Nothing, onDelete = Nothing }, deferrable = Nothing, deferrableType = Nothing }
                            ]

                let tasksTable' = StatementCreateTable CreateTable
                                { name = "tasks"
                                , columns =
                                    [ Column { name = "list_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let expectedSchema =
                            [ tasksTable'
                            , taskListsTable
                            , AddConstraint { tableName = "tasks", constraint = ForeignKeyConstraint { name = "tasks_ref_task_lists", columnName = "list_id", referenceTable = "task_lists", referenceColumn = Nothing, onDelete = Nothing }, deferrable = Nothing, deferrableType = Nothing }
                            ]
                
                let options = SchemaOperations.UpdateColumnOptions
                        { tableName = "tasks"
                        , columnName = "list_id"
                        , columnType = PUUID
                        , defaultValue = Nothing
                        , isArray = False
                        , allowNull = False
                        , isUnique = False
                        , primaryKey = False
                        , columnId = 0
                        }

                (SchemaOperations.updateColumn options inputSchema) `shouldBe` expectedSchema
            it "update a column's indexes" do
                let tableAWithCreatedAt = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "updated_at"
                                        , columnType = PTimestampWithTimezone
                                        , defaultValue = Just (CallExpression "NOW" [])
                                        , notNull = True
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }
                let index = CreateIndex { indexName = "a_updated_at_index", unique = False, tableName = "a", columns = [IndexColumn { column = VarExpression "updated_at", columnOrder = [] }], whereClause = Nothing, indexType = Nothing }

                let tableAWithUpdatedColumn = StatementCreateTable CreateTable
                            { name = "a"
                            , columns = [
                                    Column
                                        { name = "created_at"
                                        , columnType = PText
                                        , defaultValue = Nothing
                                        , notNull = False
                                        , isUnique = False
                                        , generator = Nothing
                                        }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint []
                            , constraints = []
                            }
                let indexUpdated = CreateIndex { indexName = "a_created_at_index", unique = False, tableName = "a", columns = [IndexColumn { column = VarExpression "created_at", columnOrder = [] }], whereClause = Nothing, indexType = Nothing }

                let inputSchema = [tableAWithCreatedAt, index]
                let expectedSchema = [tableAWithUpdatedColumn, indexUpdated]
                
                let options = SchemaOperations.UpdateColumnOptions
                        { tableName = "a"
                        , columnName = "created_at"
                        , columnType = PText
                        , defaultValue = Nothing
                        , isArray = False
                        , allowNull = True
                        , isUnique = False
                        , primaryKey = False
                        , columnId = 0
                        }

                (SchemaOperations.updateColumn options inputSchema) `shouldBe` expectedSchema
        describe "updateTable" do
            it "renames a table with all it's indices, constraints, policies, enable RLS statements, triggers" do
                let inputSchema = parseSqlStatements [trimming|
                    CREATE TABLE tasks ();
                    CREATE INDEX tasks_user_id_index ON tasks (user_id);
                    ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                    CREATE POLICY "Users can manage their tasks" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                    ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
                    CREATE TRIGGER update_tasks_updated_at BEFORE UPDATE ON tasks FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
                |]
                let outputSchema = parseSqlStatements [trimming|
                    CREATE TABLE todos ();
                    CREATE INDEX todos_user_id_index ON todos (user_id);
                    ALTER TABLE todos ADD CONSTRAINT todos_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                    CREATE POLICY "Users can manage their todos" ON todos USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                    ALTER TABLE todos ENABLE ROW LEVEL SECURITY;
                    CREATE TRIGGER update_todos_updated_at BEFORE UPDATE ON todos FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
                |]

                SchemaOperations.updateTable 0 "todos" inputSchema `shouldBe` outputSchema

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements
