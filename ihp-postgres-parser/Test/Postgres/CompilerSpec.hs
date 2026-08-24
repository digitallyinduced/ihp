{-|
Module: Postgres.CompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Postgres.CompilerSpec where

import Prelude
import Test.Hspec
import IHP.Postgres.Compiler (compileExpression, compilePostgresType, compileSql)
import IHP.Postgres.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Megaparsec as Megaparsec
import IHP.Postgres.Parser (parseDDL)

spec :: Spec
spec = do
    describe "The Schema.sql Compiler" do
        it "should compile an empty CREATE TABLE statement" do
            compileSql [StatementCreateTable (table "users")] `shouldBe` "CREATE TABLE users (\n\n);\n"

        it "should compile a CREATE EXTENSION for the UUID extension" do
            compileSql [CreateExtension { name = "uuid-ossp", ifNotExists = True, extensionOptions = [] }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

        it "should quote punctuation in extension schema names" do
            compileSql [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionSchema "geo.data"] }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA \"geo.data\";\n"

        it "should escape quotes in extension schema names" do
            compileSql [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionSchema "geo\"data"] }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA \"geo\"\"data\";\n"

        it "should emit WITH before arbitrarily ordered extension options" do
            compileSql [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionCascade, ExtensionSchema "geo"] }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS postgis WITH CASCADE SCHEMA geo;\n"

        it "should compile a line comment" do
            compileSql [Comment { content = " Comment value" }] `shouldBe` "-- Comment value\n"

        it "should compile a empty line comments" do
            compileSql [Comment { content = "" }, Comment { content = "" }] `shouldBe` "--\n--\n"

        it "should round-trip executable SQL COMMENT statements" do
            let sql = "COMMENT ON TABLE users IS 'owner records';"
            compileSql [parseSql sql] `shouldBe` (sql <> "\n")

        it "should compile a CREATE TABLE with columns" do
            let sql = "CREATE TABLE users (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    firstname TEXT NOT NULL,\n    lastname TEXT NOT NULL,\n    password_hash TEXT NOT NULL,\n    email TEXT NOT NULL,\n    company_id UUID NOT NULL,\n    picture_url TEXT,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"
            let statement = StatementCreateTable (table "users")
                    { columns = [
                        (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , (col "firstname" PText) { notNull = True }
                        , (col "lastname" PText) { notNull = True }
                        , (col "password_hash" PText) { notNull = True }
                        , (col "email" PText) { notNull = True }
                        , (col "company_id" PUUID) { notNull = True }
                        , col "picture_url" PText
                        , (col "created_at" PTimestampWithTimezone) { defaultValue = Just (CallExpression "NOW" []), notNull = True }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE with quoted identifiers" do
            compileSql [StatementCreateTable (table "quoted name")] `shouldBe` "CREATE TABLE \"quoted name\" (\n\n);\n"

        it "should quote punctuation and escape quotes in returned column identifiers" do
            compilePostgresType (PTable [("result.code", PText), ("result\"code", PText)]) `shouldBe`
                "TABLE (\"result.code\" TEXT, \"result\"\"code\" TEXT)"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = Just "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = Just "id"
                        , onDelete = Just Cascade
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = Just "check_title_length"
                        , checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');\n"

        -- See https://github.com/digitallyinduced/ihp/issues/2613: CHECK with ANY(ARRAY[...])
        -- is what pg_dump emits for IN constraints, so the compiler must round-trip it.
        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK with ANY(ARRAY[...])" do
            let statement = AddConstraint
                    { tableName = "foo"
                    , constraint = CheckConstraint
                        { name = Just "foo_kind_valid"
                        , checkExpression =
                            EqExpression
                                (VarExpression "kind")
                                (CallExpression "ANY"
                                    [ ArrayLiteralExpression
                                        [ TypeCastExpression (TextExpression "a") PText
                                        , TypeCastExpression (TextExpression "b") PText
                                        ]
                                    ])
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE foo ADD CONSTRAINT foo_kind_valid CHECK (kind = ANY(ARRAY['a'::TEXT, 'b'::TEXT]));\n"

        it "should compile a CREATE TYPE .. AS ENUM" do
            let sql = "CREATE TYPE colors AS ENUM ('yellow', 'red', 'blue');\n"
            let statement = CreateEnumType
                    { name = "colors"
                    , values = ["yellow", "red", "blue"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a serial id" do
            let sql = "CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n"
            let statement = StatementCreateTable (table "orders")
                    { columns = [ (col "id" PSerial) { notNull = True } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX statement" do
            let sql = "CREATE INDEX users_index ON users (user_name);\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX statement" do
            let sql = "CREATE UNIQUE INDEX users_index ON users (user_name);\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX with NULLS NOT DISTINCT" do
            let sql = "CREATE UNIQUE INDEX users_index ON users (user_name) NULLS NOT DISTINCT;\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = False
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector column types with dimensions" do
            let sql = "ALTER TABLE knowledge_chunks ADD COLUMN embedding VECTOR(1536) DEFAULT NULL;\n"
            let statement = AddColumn
                    { tableName = "knowledge_chunks"
                    , column = (col "embedding" (PCustomType "VECTOR(1536)")) { defaultValue = Just (VarExpression "NULL") }
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector HNSW indexes with operator classes" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_hnsw_idx ON knowledge_chunks USING HNSW (embedding vector_cosine_ops) WHERE embedding IS NOT NULL;\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_hnsw_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_cosine_ops", columnOrder = [] }]
                    , whereClause = Just (IsExpression (VarExpression "embedding") (NotExpression (VarExpression "NULL")))
                    , indexType = Just Hnsw
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile pgvector IVFFLAT indexes with operator classes" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_ivfflat_idx ON knowledge_chunks USING IVFFLAT (embedding vector_l2_ops);\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_ivfflat_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_l2_ops", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Ivfflat
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile additional PostgreSQL index methods" do
            let compileMethod indexType = compileSql [CreateIndex
                    { indexName = "users_email_idx"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "email")]
                    , whereClause = Nothing
                    , indexType = Just indexType
                    , nullsDistinct = True
                    }]
            compileMethod Hash `shouldBe` "CREATE INDEX users_email_idx ON users USING HASH (email);\n"
            compileMethod Spgist `shouldBe` "CREATE INDEX users_email_idx ON users USING SPGIST (email);\n"
            compileMethod Brin `shouldBe` "CREATE INDEX users_email_idx ON users USING BRIN (email);\n"

        it "should quote index operator class identifiers" do
            let sql = "CREATE INDEX knowledge_chunks_embedding_idx ON knowledge_chunks USING HNSW (embedding \"VectorOps\");\n"
            let statement = CreateIndex
                    { indexName = "knowledge_chunks_embedding_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "VectorOps", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Hnsw
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE FUNCTION with SET options" do
            let sql = "CREATE OR REPLACE FUNCTION sync_access() RETURNS TRIGGER SECURITY DEFINER SET search_path = public, private, pg_temp AS $$BEGIN\n    RETURN NEW;\nEND;$$ language plpgsql;\n"
            let statement = CreateFunction
                    { functionName = "sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }
            compileSql [statement] `shouldBe` sql

        it "should choose a safe dollar quote for function bodies" do
            let statement = (function "uses_dollars") { functionBody = "SELECT '$$' || $1;", returns = PText, language = "sql" }
            parseSql (compileSql [statement]) `shouldBe` statement
            let boundaryStatement = (function "boundary_dollars") { functionBody = "$_$", returns = PText, language = "sql" }
            parseSql (compileSql [boundaryStatement]) `shouldBe` boundaryStatement
            let trailingDollarStatement = (function "trailing_dollar") { functionBody = "SELECT '$", returns = PText, language = "sql" }
            parseSql (compileSql [trailingDollarStatement]) `shouldBe` trailingDollarStatement

        it "should round-trip CREATE FUNCTION attributes" do
            let statement = CreateFunction
                    { functionName = "current_organization_id"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = True
                    , functionAttributes = ["STABLE", "PARALLEL SAFE", "COST 2.5"]
                    , functionSettings = []
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip set-returning function signatures" do
            let statement = CreateFunction
                    { functionName = "estimated"
                    , functionArguments = []
                    , functionBody = "SELECT NULL, NULL;"
                    , orReplace = False
                    , returns = PTable [("id", PUUID), ("label", PText)]
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = ["ROWS 10"]
                    , functionSettings = []
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should re-quote decoded input argument names" do
            let statement = parseSql "CREATE FUNCTION quoted_arg(\"arg\"\"name\" text) RETURNS text LANGUAGE sql AS $$SELECT NULL;$$;"

            compileSql [statement] `shouldBe`
                "CREATE FUNCTION quoted_arg(\"arg\"\"name\" TEXT) RETURNS TEXT AS $$SELECT NULL;$$ language sql;\n"
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip TRANSFORM attributes for qualified custom types" do
            let statement = CreateFunction
                    { functionName = "transformed"
                    , functionArguments = [("value", PCustomType "private.widget")]
                    , functionBody = "BEGIN RETURN value; END;"
                    , orReplace = False
                    , returns = PCustomType "private.widget"
                    , language = "plpgsql"
                    , securityDefiner = False
                    , functionAttributes = ["TRANSFORM FOR TYPE private.widget"]
                    , functionSettings = []
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip a quoted SUPPORT function identifier" do
            let statement = CreateFunction
                    { functionName = "supported"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = ["SUPPORT \"MySupport\""]
                    , functionSettings = []
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip function-only return types" do
            let setReturning = (function "search_ids") { returns = PSetOf PUUID, language = "sql" }
            let tableReturning = (function "search_rows") { returns = PTable [("id", PUUID), ("label", PText)], language = "sql" }
            parseSql (compileSql [setReturning]) `shouldBe` setReturning
            parseSql (compileSql [tableReturning]) `shouldBe` tableReturning

        it "should keep boolean IS expressions grouped inside equality" do
            let sql = "ALTER TABLE t ADD CONSTRAINT t_pair CHECK ((a IS NULL) = (b IS NULL));"
            compileSql [parseSql sql] `shouldBe` (sql <> "\n")

        it "should round-trip PostgreSQL 18 named NOT NULL constraints" do
            let sql = "CREATE TABLE users (\n    email TEXT CONSTRAINT users_email_not_null NOT NULL\n);"
            compileSql [parseSql sql] `shouldBe` (sql <> "\n")

        it "should round-trip non-public schema-qualified table names" do
            let statement = StatementCreateTable (table "private.users")
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should quote qualified identifier components independently" do
            let statement = StatementCreateTable (table "tenant-a.MixedUsers")
            compileSql [statement] `shouldBe` "CREATE TABLE \"tenant-a\".\"MixedUsers\" (\n\n);\n"
            parseSql (compileSql [statement]) `shouldBe` statement

        it "does not split dots in ordinary quoted identifiers" do
            let statement = StatementCreateTable (table "users")
                    { columns = [(col "A.b" PText)] }
            compileSql [statement] `shouldBe` "CREATE TABLE users (\n    \"A.b\" TEXT\n);\n"

        it "round-trips schema-qualified enum types" do
            let statement = CreateEnumType { name = "private.status", values = ["active"] }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "keeps dotted CREATE INDEX names as single identifiers" do
            let statement = CreateIndex
                    { indexName = "audit.v1"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "id")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` "CREATE INDEX \"audit.v1\" ON users (id);\n"

        it "should round-trip a schema-qualified DROP TABLE" do
            let statement = DropTable { tableName = "private.users" }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should compile LIKE escape clauses without changing their grouping" do
            let expression = BinaryOperatorExpression "ESCAPE"
                    (BinaryOperatorExpression "LIKE" (VarExpression "code") (TextExpression "A!_%"))
                    (TextExpression "!")
            compileExpression expression `shouldBe` "code LIKE 'A!_%' ESCAPE '!'"

        it "should parenthesize comparisons used by generic operators" do
            let expression = BinaryOperatorExpression "##" (EqExpression (VarExpression "a") (VarExpression "b")) (VarExpression "flag")
            compileExpression expression `shouldBe` "(a = b) ## flag"

        it "should round-trip a schema-qualified CREATE FUNCTION" do
            -- parse -> compile -> parse must preserve a non-public schema like `private.`
            let statement = CreateFunction
                    { functionName = "private.sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should round-trip a schema-qualified DROP FUNCTION" do
            -- Guards against the CREATE/DROP asymmetry: both must accept `private.` names
            let statement = DropFunction { functionName = "private.sync_access" }
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should compile a CREATE INDEX with VARIADIC function arguments" do
            let sql = "CREATE INDEX agent_runs_ingest_gmail_message_latest_idx ON agent_runs USING BTREE (organization_id, jsonb_extract_path_text(input, VARIADIC ARRAY['gmailMessageId'::TEXT]), COALESCE(completed_at, last_event_at, started_at, created_at) DESC, id DESC) WHERE type = ('ingest'::agent_run_type) AND jsonb_extract_path_text(input, VARIADIC ARRAY['source'::TEXT]) = ('gmail_email_ingest'::TEXT);\n"
            let statement = CreateIndex
                    { indexName = "agent_runs_ingest_gmail_message_latest_idx"
                    , unique = False
                    , tableName = "agent_runs"
                    , columns =
                            [ indexCol (VarExpression "organization_id")
                            , indexCol (CallExpression "jsonb_extract_path_text"
                                [ VarExpression "input"
                                , VariadicExpression (ArrayLiteralExpression [TypeCastExpression (TextExpression "gmailMessageId") PText])
                                ])
                            , IndexColumn
                                { column = CallExpression "COALESCE"
                                    [ VarExpression "completed_at"
                                    , VarExpression "last_event_at"
                                    , VarExpression "started_at"
                                    , VarExpression "created_at"
                                    ]
                                , columnOperatorClass = Nothing
                                , columnOrder = [Desc]
                                }
                            , IndexColumn { column = VarExpression "id", columnOperatorClass = Nothing, columnOrder = [Desc] }
                            ]
                    , whereClause = Just
                        (AndExpression
                            (EqExpression
                                (VarExpression "type")
                                (TypeCastExpression (TextExpression "ingest") (PCustomType "agent_run_type")))
                            (EqExpression
                                (CallExpression "jsonb_extract_path_text"
                                    [ VarExpression "input"
                                    , VariadicExpression (ArrayLiteralExpression [TypeCastExpression (TextExpression "source") PText])
                                    ])
                                (TypeCastExpression (TextExpression "gmail_email_ingest") PText)))
                    , indexType = Just Btree
                    , nullsDistinct = True
                    }
            compileSql [statement] `shouldBe` sql

        it "should preserve grouping for inequality predicate operands" do
            compileExpression
                (NotEqExpression
                    (IsExpression (VarExpression "a") (VarExpression "NULL"))
                    (IsExpression (VarExpression "b") (VarExpression "NULL")))
                `shouldBe` "(a IS NULL) <> (b IS NULL)"

        it "should compile 'ENABLE ROW LEVEL SECURITY' statements" do
            let sql = "ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;\n"
            let statements = [EnableRowLevelSecurity { tableName = "tasks" }]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE POLICY' statements" do
            let sql = "CREATE POLICY \"Users can manage their tasks\" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());\n"
            let p = (policy "Users can manage their tasks" "tasks")
                    { using = Just (
                        EqExpression
                            (VarExpression "user_id")
                            (CallExpression "ihp_user_id" [])
                        )
                    , check = Just (
                        EqExpression
                            (VarExpression "user_id")
                            (CallExpression "ihp_user_id" [])
                        )
                    }
            compileSql [p] `shouldBe` sql

        it "should compile 'DROP TABLE ..' statements" do
            let sql = "DROP TABLE tasks;\n"
            let statements = [ DropTable { tableName = "tasks" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE SEQUENCE ..' statements" do
            let sql = "CREATE SEQUENCE a;\n"
            let statements = [ CreateSequence { name = "a", sequenceOptions = [] } ]
            compileSql statements `shouldBe` sql

        it "should escape quotes in extension versions" do
            let sql = "CREATE EXTENSION extension_name VERSION '1''beta';\n"
            let statements = [ CreateExtension { name = "extension_name", ifNotExists = False, extensionOptions = [ExtensionVersion "1'beta"] } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER SEQUENCE ..' statements" do
            let sql = "ALTER SEQUENCE a INCREMENT BY 3 CACHE 10;\n"
            let statements = [ AlterSequence { name = "a", sequenceOptions = [SequenceIncrement (IntExpression 3), SequenceCache (IntExpression 10)] } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP TYPE ..;' statements" do
            let sql = "DROP TYPE colors;\n"
            let statements = [ DropEnumType { name = "colors" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'BEGIN;' statements" do
            let sql = "BEGIN;\n"
            let statements = [ Begin ]
            compileSql statements `shouldBe` sql

        it "should compile 'COMMIT;' statements" do
            let sql = "COMMIT;\n"
            let statements = [ Commit ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE TABLE .. INHERITS (..)' statements" do
            let sql = "CREATE TABLE post_revisions (\n    revision_content TEXT NOT NULL\n) INHERITS (posts);\n"
            let statements = [
                        StatementCreateTable (table "post_revisions")
                            { columns = [(col "revision_content" PText) { notNull = True }]
                            , inherits = Just "posts"
                            }
                        ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE UNLOGGED TABLE' statements" do
            let sql = "CREATE UNLOGGED TABLE pg_large_notifications (\n\n);\n"
            let statements = [
                        StatementCreateTable (table "pg_large_notifications")
                            { unlogged = True, inherits = Nothing
                            }
                        ]
            compileSql statements `shouldBe` sql

        it "should parenthesize binary expressions before type casts" do
            compileExpression
                (TypeCastExpression
                    (BinaryOperatorExpression "+" (VarExpression "price") (VarExpression "tax"))
                    (PNumeric Nothing Nothing))
                `shouldBe` "(price + tax)::NUMERIC"

        describe "literal and type round trips" do
            let roundTrip sql = compileSql [parseSql sql] `shouldBe` (sql <> "\n")

            it "keeps numeric scale" do
                roundTrip "CREATE TABLE fees (\n    vat NUMERIC(7,4) DEFAULT 20.0000 NOT NULL\n);"

            it "keeps PostGIS geometry modifiers" do
                roundTrip "CREATE TABLE locations (\n    geom GEOMETRY(Point, 4326)\n);"

            it "escapes apostrophes in string literals" do
                roundTrip "ALTER TABLE fees ADD CONSTRAINT fees_label_check CHECK (label <> 'owner''s fee');"

            it "keeps POSITION's SQL-standard IN syntax" do
                roundTrip "ALTER TABLE users ADD CONSTRAINT users_email_position_check CHECK (POSITION('@' IN email) > 1);"

parseSql :: Text -> Statement
parseSql sql =
    case Megaparsec.runParser parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError)
            Right [statement] -> statement
            Right statements -> error $ "Expected single statement but got: " <> show (length statements)
