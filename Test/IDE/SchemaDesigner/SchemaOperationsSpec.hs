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
                let policy = CreatePolicy { tableName = "a", name = "p", check = Nothing, using = Nothing }
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
                                    [ Column { name = "user_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema = [table]
                let expectedPolicy = CreatePolicy
                        { name = "Users can manage their posts"
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
                                    [ Column { name = "title", columnType = PText, defaultValue = Nothing, notNull = True, isUnique = False }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema = [table]
                let expectedPolicy = CreatePolicy
                        { name = ""
                        , tableName = "posts"
                        , using = Nothing
                        , check = Nothing
                        }

                SchemaOperations.suggestPolicy schema table `shouldBe` expectedPolicy

            it "should suggest a policy if it can find a one hop path to a user_id column" do
                let tasksTable = StatementCreateTable CreateTable
                                { name = "tasks"
                                , columns =
                                    [ Column { name = "task_list_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let taskListsTable = StatementCreateTable CreateTable
                                { name = "task_lists"
                                , columns =
                                    [ Column { name = "user_id", columnType = PUUID, defaultValue = Nothing, notNull = True, isUnique = False }
                                    ]
                                , primaryKeyConstraint = PrimaryKeyConstraint []
                                , constraints = []
                                }
                let schema =
                            [ tasksTable
                            , taskListsTable
                            , AddConstraint { tableName = "tasks", constraint = ForeignKeyConstraint { name = "tasks_ref_task_lists", columnName = "task_list_id", referenceTable = "task_lists", referenceColumn = Nothing, onDelete = Nothing } }
                            ]
                let expectedPolicy = CreatePolicy
                        { name = "Users can manage the tasks if they can see the TaskList"
                        , tableName = "tasks"
                        , using = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "task_lists", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "task_lists") "id") (DotExpression (VarExpression "tasks") "task_list_id")})))
                        , check = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "task_lists", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "task_lists") "id") (DotExpression (VarExpression "tasks") "task_list_id")})))
                        }

                SchemaOperations.suggestPolicy schema tasksTable `shouldBe` expectedPolicy

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements
