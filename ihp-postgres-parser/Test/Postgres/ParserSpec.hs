{-|
Module: Postgres.ParserSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Postgres.ParserSpec where

import Prelude
import Test.Hspec
import IHP.Postgres.Parser
import IHP.Postgres.Compiler (compileSql)
import IHP.Postgres.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import Data.Either (isLeft)
import qualified Text.Megaparsec as Megaparsec
import GHC.IO (evaluate)

spec :: Spec
spec = do
    describe "The Schema.sql Parser" do
        it "should parse an empty CREATE TABLE statement" do
            parseSql "CREATE TABLE users ();"  `shouldBe` StatementCreateTable (table "users")

        it "should parse an CREATE EXTENSION for the UUID extension" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

        it "should preserve a missing IF NOT EXISTS clause" do
            parseSql "CREATE EXTENSION \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = False }

        it "should parse an CREATE EXTENSION with schema suffix" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" WITH SCHEMA public;" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

        describe "parseCreateExtensionMigration" do
            it "accepts one or more extension statements and comments" do
                parseCreateExtensionMigration "-- Required for earthdistance\nCREATE EXTENSION IF NOT EXISTS cube;\nCREATE EXTENSION IF NOT EXISTS \"earthdistance\" WITH SCHEMA public;"
                    `shouldBe` Right
                        [ CreateExtension { name = "cube", ifNotExists = True }
                        , CreateExtension { name = "earthdistance", ifNotExists = True }
                        ]

            it "is case insensitive" do
                parseCreateExtensionMigration "create extension if not exists PG_TRGM;"
                    `shouldBe` Right [CreateExtension { name = "pg_trgm", ifNotExists = True }]

            it "accepts PostgreSQL extension options" do
                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS PostGIS WITH SCHEMA public VERSION '3.4.2' CASCADE;"
                    `shouldBe` Right [CreateExtension { name = "postgis", ifNotExists = True }]

                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS postgis WITH VERSION stable CASCADE;"
                    `shouldBe` Right [CreateExtension { name = "postgis", ifNotExists = True }]

            it "preserves quoted extension names" do
                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS \"MixedCase\";"
                    `shouldBe` Right [CreateExtension { name = "MixedCase", ifNotExists = True }]

            it "rejects a mixed migration" do
                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS pg_trgm; CREATE TABLE users ();"
                    `shouldSatisfy` isLeft

        describe "containsCreateExtensionStatement" do
            it "detects extension statements split by comments and whitespace" do
                containsCreateExtensionStatement "CREATE /* reason */\nEXTENSION IF NOT EXISTS postgis;" `shouldBe` True

            it "ignores extension keywords inside strings, identifiers, comments, and function bodies" do
                containsCreateExtensionStatement "SELECT 'CREATE EXTENSION postgis'; -- CREATE EXTENSION cube\nSELECT $$ CREATE EXTENSION earthdistance $$; SELECT \"CREATE\";"
                    `shouldBe` False

            it "only matches CREATE at the start of a statement" do
                containsCreateExtensionStatement "SELECT create extension; SELECT create, extension;"
                    `shouldBe` False

                containsCreateExtensionStatement "CREATE TABLE places (); CREATE /* privileged */ EXTENSION postgis;"
                    `shouldBe` True

        it "should parse a line comment" do
            parseSql "-- Comment value" `shouldBe` Comment { content = " Comment value" }

        it "should parse an empty comment" do
            parseSqlStatements "--\n--" `shouldBe` [ Comment { content = "" }, Comment { content = "" } ]

        it "should parse a CREATE TABLE with columns" do
            let sql = "CREATE TABLE users (\n                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n                    firstname TEXT NOT NULL,\n                    lastname TEXT NOT NULL,\n                    password_hash TEXT NOT NULL,\n                    email TEXT NOT NULL,\n                    company_id UUID NOT NULL,\n                    picture_url TEXT,\n                    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n                ); "
            parseSql sql `shouldBe` StatementCreateTable (table "users")
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
                    , constraints = []
                    , unlogged = False
                    , inherits = Nothing
                    }

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` StatementCreateTable (table "quoted name")

        it "should preserve non-public schema-qualified table names" do
            parseSql "CREATE TABLE private.users ();" `shouldBe`
                StatementCreateTable (table "private.users")
            parseSql "CREATE TABLE public.users ();" `shouldBe`
                StatementCreateTable (table "users")

        it "should preserve non-public schemas across foreign keys" do
            parseSql "ALTER TABLE private.tokens ADD CONSTRAINT tokens_user_fk FOREIGN KEY (user_id) REFERENCES auth.users (id);" `shouldBe`
                AddConstraint
                    { tableName = "private.tokens"
                    , constraint = ForeignKeyConstraint
                        { name = Just "tokens_user_fk"
                        , columnName = "user_id"
                        , referenceTable = "auth.users"
                        , referenceColumn = Just "id"
                        , onDelete = Nothing
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse a CREATE TABLE with public schema prefix" do
            parseSql "CREATE TABLE public.users ();" `shouldBe` StatementCreateTable (table "users")

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;" `shouldBe` AddConstraint
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

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = Just "check_title_length"
                        , checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse CREATE TYPE .. AS ENUM" do
            parseSql "CREATE TYPE colors AS ENUM ('yellow', 'red', 'green');" `shouldBe` CreateEnumType { name = "colors", values = ["yellow", "red", "green"] }

        it "should parse ALTER TYPE .. ADD VALUE .." do
            parseSql "ALTER TYPE colors ADD VALUE 'blue';" `shouldBe` AddValueToEnumType { enumName = "colors", newValue = "blue", ifNotExists = False }

        it "should parse a CREATE TABLE statement with a serial id" do
            parseSql "CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n" `shouldBe` StatementCreateTable (table "orders")
                    { columns = [ (col "id" PSerial) { notNull = True} ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }

        it "should parse a column with NOT NULL before DEFAULT" do
            parseSql "CREATE TABLE tasks (is_completed BOOLEAN NOT NULL DEFAULT false);" `shouldBe` StatementCreateTable (table "tasks")
                    { columns = [ (col "is_completed" PBoolean) { defaultValue = Just (VarExpression "false"), notNull = True } ]
                    }

        it "should parse column modifiers in mixed order" do
            parseSql "CREATE TABLE orders (id UUID PRIMARY KEY DEFAULT uuid_generate_v4() NOT NULL);" `shouldBe` StatementCreateTable (table "orders")
                    { columns = [ (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }

        it "should parse a CREATE INDEX statement" do
            parseSql "CREATE INDEX users_index ON users (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }

        it "should parse a CREATE UNIQUE INDEX statement" do
            parseSql "CREATE UNIQUE INDEX users_index ON users (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }

        it "should parse a CREATE UNIQUE INDEX with NULLS NOT DISTINCT" do
            parseSql "CREATE UNIQUE INDEX travel_days_trip_date_contact_unique ON public.travel_days USING btree (trip_id, day_date, meal_contact_id) NULLS NOT DISTINCT;\n" `shouldBe` CreateIndex
                    { indexName = "travel_days_trip_date_contact_unique"
                    , unique = True
                    , tableName = "travel_days"
                    , columns =
                        [ indexCol (VarExpression "trip_id")
                        , indexCol (VarExpression "day_date")
                        , indexCol (VarExpression "meal_contact_id")
                        ]
                    , whereClause = Nothing
                    , indexType = Just Btree
                    , nullsDistinct = False
                    }

        it "should parse a CREATE UNIQUE INDEX with explicit NULLS DISTINCT" do
            parseSql "CREATE UNIQUE INDEX users_index ON users (user_name) NULLS DISTINCT;\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [indexCol (VarExpression "user_name")]
                    , whereClause = Nothing
                    , indexType = Nothing
                    , nullsDistinct = True
                    }

        it "should parse pgvector column types with dimensions" do
            parseSql "ALTER TABLE knowledge_chunks ADD COLUMN embedding VECTOR(1536) DEFAULT NULL;" `shouldBe` AddColumn
                    { tableName = "knowledge_chunks"
                    , column = (col "embedding" (PCustomType "VECTOR(1536)")) { defaultValue = Just (VarExpression "NULL") }
                    }

        it "should preserve custom type modifier contents" do
            parseSql "ALTER TABLE knowledge_chunks ADD COLUMN embedding VECTOR( 1536 ) DEFAULT NULL;" `shouldBe` AddColumn
                    { tableName = "knowledge_chunks"
                    , column = (col "embedding" (PCustomType "VECTOR( 1536 )")) { defaultValue = Just (VarExpression "NULL") }
                    }

        it "should parse pgvector HNSW indexes with operator classes" do
            parseSql "CREATE INDEX knowledge_chunks_embedding_hnsw_idx ON knowledge_chunks USING hnsw (embedding vector_cosine_ops) WHERE embedding IS NOT NULL;" `shouldBe` CreateIndex
                    { indexName = "knowledge_chunks_embedding_hnsw_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_cosine_ops", columnOrder = [] }]
                    , whereClause = Just (IsExpression (VarExpression "embedding") (NotExpression (VarExpression "NULL")))
                    , indexType = Just Hnsw
                    , nullsDistinct = True
                    }

        it "should parse pgvector IVFFLAT indexes with operator classes" do
            parseSql "CREATE INDEX knowledge_chunks_embedding_ivfflat_idx ON knowledge_chunks USING ivfflat (embedding vector_l2_ops);" `shouldBe` CreateIndex
                    { indexName = "knowledge_chunks_embedding_ivfflat_idx"
                    , unique = False
                    , tableName = "knowledge_chunks"
                    , columns = [IndexColumn { column = VarExpression "embedding", columnOperatorClass = Just "vector_l2_ops", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Ivfflat
                    , nullsDistinct = True
                    }

        it "should parse additional PostgreSQL index methods" do
            let parseMethod method = case parseSql ("CREATE INDEX users_email_idx ON users USING " <> method <> " (email);") of
                    CreateIndex { indexType } -> indexType
                    _ -> error "Expected CreateIndex"
            parseMethod "hash" `shouldBe` Just Hash
            parseMethod "spgist" `shouldBe` Just Spgist
            parseMethod "brin" `shouldBe` Just Brin

        it "should parse CREATE FUNCTION with SET options before AS" do
            let sql = "CREATE OR REPLACE FUNCTION sync_access()\nRETURNS TRIGGER\nLANGUAGE plpgsql\nSECURITY DEFINER\nSET search_path = public, private, pg_temp\nAS $$BEGIN\n    RETURN NEW;\nEND;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }

        it "should not stop CREATE FUNCTION SET values at keyword prefixes" do
            let sql = "CREATE OR REPLACE FUNCTION set_tz()\nRETURNS TRIGGER\nSET TimeZone = 'Asia/Tokyo'\nAS $$BEGIN\n    RETURN NEW;\nEND;$$ language plpgsql;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "set_tz"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = False
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "TimeZone"
                            , settingValue = "'Asia/Tokyo'"
                            }
                        ]
                    }

        it "should parse pg_dump CREATE FUNCTION SET options with TO" do
            let sql = "CREATE OR REPLACE FUNCTION private.sync_access()\nRETURNS TRIGGER\nLANGUAGE plpgsql\nSECURITY DEFINER\nSET search_path TO 'public', 'private', 'pg_temp'\nAS $$BEGIN\n    RETURN NEW;\nEND;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "private.sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "'public', 'private', 'pg_temp'"
                            }
                        ]
                    }

        it "should parse CREATE FUNCTION SET options with TO and an unqualified name" do
            -- Isolates the `SET ... TO ...` change from the schema-qualified name change
            let sql = "CREATE OR REPLACE FUNCTION sync_access()\nRETURNS TRIGGER\nLANGUAGE plpgsql\nSECURITY DEFINER\nSET search_path TO 'public'\nAS $$BEGIN\n    RETURN NEW;\nEND;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "'public'"
                            }
                        ]
                    }

        it "should preserve a non-public schema on CREATE FUNCTION with = style settings" do
            -- Isolates the schema-qualified name change from the `SET ... TO ...` change
            let sql = "CREATE OR REPLACE FUNCTION private.sync_access()\nRETURNS TRIGGER\nLANGUAGE plpgsql\nSECURITY DEFINER\nSET search_path = public, private, pg_temp\nAS $$BEGIN\n    RETURN NEW;\nEND;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "private.sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = True
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }

        it "should normalize the default public schema away on CREATE FUNCTION" do
            -- Keeps function names comparable regardless of an explicit `public.` prefix,
            -- matching how `qualifiedIdentifier` treats every other identifier.
            let sql = "CREATE OR REPLACE FUNCTION public.sync_access()\nRETURNS TRIGGER\nAS $$BEGIN\n    RETURN NEW;\nEND;$$ language plpgsql;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "sync_access"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    RETURN NEW;\nEND;"
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    , securityDefiner = False
                    , functionSettings = []
                    }

        it "should parse DROP FUNCTION with a non-public schema-qualified name" do
            -- DROP FUNCTION must accept the same schema-qualified names as CREATE FUNCTION
            parseSql "DROP FUNCTION private.sync_access;" `shouldBe` DropFunction { functionName = "private.sync_access" }

        it "should normalize the default public schema away on DROP FUNCTION" do
            parseSql "DROP FUNCTION public.sync_access;" `shouldBe` DropFunction { functionName = "sync_access" }

        it "should parse a pg_dump CREATE INDEX with VARIADIC function arguments" do
            let sql = "CREATE INDEX agent_runs_ingest_gmail_message_latest_idx ON public.agent_runs USING btree (organization_id, jsonb_extract_path_text(input, VARIADIC ARRAY['gmailMessageId'::text]), COALESCE(completed_at, last_event_at, started_at, created_at) DESC, id DESC) WHERE ((type = 'ingest'::public.agent_run_type) AND (jsonb_extract_path_text(input, VARIADIC ARRAY['source'::text]) = 'gmail_email_ingest'::text));"
            parseSql sql `shouldBe` CreateIndex
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

        it "should parse 'ENABLE ROW LEVEL SECURITY' statements" do
            parseSql "ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;" `shouldBe` EnableRowLevelSecurity { tableName = "tasks" }

        it "should parse 'CREATE POLICY' statements" do
            parseSql "CREATE POLICY \"Users can manage their tasks\" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());" `shouldBe`
                    (policy "Users can manage their tasks" "tasks")
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

        -- pg_dump qualifies every column with its table name, so policies
        -- exporting `col IN (SELECT …)` come back as `tab.col IN (SELECT …)`.
        -- Both `dot` and `IN` are postfix at the same precedence; without
        -- chaining, only `dot` would apply and `IN` would be left dangling.
        it "should parse 'CREATE POLICY' with qualified column and IN (SELECT …)" do
            parseSql "CREATE POLICY \"p\" ON tasks USING (tasks.user_id IN (SELECT users.id FROM users WHERE users.active));" `shouldBe`
                    (policy "p" "tasks")
                    { using = Just (
                        InExpression
                            (DotExpression (VarExpression "tasks") "user_id")
                            (InArrayExpression
                                [ SelectExpression Select
                                    { columns = [DotExpression (VarExpression "users") "id"]
                                    , from = VarExpression "users"
                                    , alias = Nothing
                                    , whereClause = DotExpression (VarExpression "users") "active"
                                    }
                                ]
                            )
                        )
                    , check = Nothing
                    }

        it "should parse 'DROP TABLE ..' statements" do
            parseSql "DROP TABLE tasks;" `shouldBe` DropTable { tableName = "tasks" }

        it "should parse a schema-qualified DROP TABLE" do
            parseSql "DROP TABLE private.tasks;" `shouldBe` DropTable { tableName = "private.tasks" }

        it "should parse 'DROP TYPE ..' statements" do
            parseSql "DROP TYPE colors;" `shouldBe` DropEnumType { name = "colors" }

        it "should parse 'CREATE SEQUENCE ..' statements" do
            parseSql "CREATE SEQUENCE a;" `shouldBe` CreateSequence { name = "a" }

        it "should parse 'BEGIN' statements" do
            parseSql "BEGIN;" `shouldBe` Begin

        it "should parse 'COMMIT' statements" do
            parseSql "COMMIT;" `shouldBe` Commit

        it "should parse 'CREATE UNLOGGED TABLE' statement" do
            parseSql "CREATE UNLOGGED TABLE pg_large_notifications ();"  `shouldBe` StatementCreateTable (table "pg_large_notifications") { unlogged = True }

        it "should parse 'CREATE TABLE .. INHERITS (..)' statement" do
            parseSql "CREATE TABLE post_revisions (revision_content TEXT NOT NULL) INHERITS (posts);"  `shouldBe` StatementCreateTable (table "post_revisions") { columns = [(col "revision_content" PText) { notNull = True }], inherits = Just "posts" }

        it "should parse positive IntExpression's" do
            parseExpression "1" `shouldBe` (IntExpression 1)

        it "should parse negative IntExpression's" do
            parseExpression "-1" `shouldBe` (IntExpression (-1))

        it "should preserve positive numeric literals exactly" do
            parseExpression "1.337" `shouldBe` NumericExpression "1.337"

        it "should preserve negative numeric literals exactly" do
            parseExpression "-1.337" `shouldBe` NumericExpression "-1.337"

        it "should preserve PostGIS geometry modifiers" do
            parseSql "CREATE TABLE locations (geom geometry(Point, 4326));"
                `shouldBe` StatementCreateTable (table "locations") { columns = [col "geom" (PGeometryWithModifier "Point, 4326")] }

        it "should preserve GRANT and REVOKE statements" do
            parseSql "GRANT SELECT ON TABLE users TO ihp_authenticated;" `shouldBe`
                UnknownStatement { raw = "GRANT SELECT ON TABLE users TO ihp_authenticated" }
            parseSql "REVOKE ALL ON FUNCTION public.touch_updated_at() FROM PUBLIC;" `shouldBe`
                UnknownStatement { raw = "REVOKE ALL ON FUNCTION public.touch_updated_at() FROM PUBLIC" }

        it "should preserve executable SQL COMMENT statements" do
            parseSql "COMMENT ON TABLE users IS 'owner records';" `shouldBe`
                UnknownStatement { raw = "COMMENT ON TABLE users IS 'owner records'" }

        it "should normalize equivalent COMMENT literal forms" do
            normalizeComment "COMMENT ON TABLE users IS $$application users$$" `shouldBe`
                Just "COMMENT ON TABLE users IS 'application users'"
            normalizeComment "COMMENT ON TABLE users IS 'owner''s records'" `shouldBe`
                Just "COMMENT ON TABLE users IS 'owner''s records'"

        it "should normalize COMMENT function signature spacing" do
            normalizeComment "COMMENT ON FUNCTION f(integer,text) IS 'x'" `shouldBe`
                Just "COMMENT ON FUNCTION f(integer, text) IS 'x'"

        it "should preserve a newline after a trailing opaque line comment" do
            let statements = parseSqlStatements "GRANT SELECT ON users TO reader -- rationale\n;"
            statements `shouldBe` [UnknownStatement { raw = "GRANT SELECT ON users TO reader -- rationale\n" }]
            compileSql statements `shouldBe` "GRANT SELECT ON users TO reader -- rationale\n;\n"

        it "should preserve a newline before an indented opaque terminator" do
            let statements = parseSqlStatements "GRANT SELECT ON users TO reader -- rationale\n    ;"
            compileSql statements `shouldBe` "GRANT SELECT ON users TO reader -- rationale\n;\n"

        it "should preserve nested block comments after opaque keywords" do
            parseSql "GRANT /* outer /* inner */ outer */ SELECT ON users TO reader;" `shouldBe`
                UnknownStatement { raw = "GRANT /* outer /* inner */ outer */ SELECT ON users TO reader" }

        it "should preserve semicolons inside opaque statement literals" do
            parseSql "COMMENT ON TABLE users IS 'internal; only';" `shouldBe`
                UnknownStatement { raw = "COMMENT ON TABLE users IS 'internal; only'" }
            parseSql "GRANT SELECT ON TABLE users TO \"report;reader\";" `shouldBe`
                UnknownStatement { raw = "GRANT SELECT ON TABLE users TO \"report;reader\"" }

        it "should treat backslashes literally in standard SQL strings" do
            parseSql "COMMENT ON TABLE users IS 'path\\';" `shouldBe`
                UnknownStatement { raw = "COMMENT ON TABLE users IS 'path\\'" }

        it "should preserve a DO block whose body contains semicolons" do
            parseSql "DO $$\nBEGIN\n    PERFORM 1;\nEND\n$$;" `shouldBe`
                UnknownStatement { raw = "DO $$\nBEGIN\n    PERFORM 1;\nEND\n$$" }

        it "should preserve a DO block with a standard string body" do
            parseSql "DO 'BEGIN PERFORM 1; END';" `shouldBe`
                UnknownStatement { raw = "DO 'BEGIN PERFORM 1; END'" }

        it "should preserve newline-concatenated DO string bodies" do
            parseSql "DO 'BEGIN NULL;'\n' END';" `shouldBe`
                UnknownStatement { raw = "DO 'BEGIN NULL;'\n' END'" }

        it "should allow comments between newline-concatenated DO string bodies" do
            parseSql "DO 'BEGIN'\n/* explanation */ ' END';" `shouldBe`
                UnknownStatement { raw = "DO 'BEGIN'\n/* explanation */ ' END'" }

        it "should locate COMMENT values outside quoted text" do
            unsetComment "COMMENT ON TABLE \"records IS active\" IS 'this IS documented'" `shouldBe`
                Just "COMMENT ON TABLE \"records IS active\" IS NULL"

        it "should preserve a DO block with an escape-string body" do
            parseSql "DO E'BEGIN PERFORM 1; END';" `shouldBe`
                UnknownStatement { raw = "DO E'BEGIN PERFORM 1; END'" }

        it "should preserve a DO block with a Unicode-escape string body" do
            parseSql "DO U&'BEGIN PERFORM \\0061; END' UESCAPE '\\';" `shouldBe`
                UnknownStatement { raw = "DO U&'BEGIN PERFORM \\0061; END' UESCAPE '\\'" }

        it "should preserve a DO block with a national-character string body" do
            parseSql "DO N'BEGIN NULL; END';" `shouldBe`
                UnknownStatement { raw = "DO N'BEGIN NULL; END'" }

        it "should accept PostgreSQL comments around DO clauses" do
            parseSql "DO -- rationale\n/* outer /* inner */ outer */ $$ BEGIN NULL; END $$;" `shouldBe`
                UnknownStatement { raw = "DO $$ BEGIN NULL; END $$" }

        it "should preserve a quoted DO language identifier" do
            parseSql "DO LANGUAGE \"MyLang\" $$ BEGIN NULL; END $$;" `shouldBe`
                UnknownStatement { raw = "DO LANGUAGE \"MyLang\" $$ BEGIN NULL; END $$" }

        it "should preserve a Unicode-delimited DO language identifier" do
            parseSql "DO LANGUAGE U&\"plpg\\0073ql\" $$ BEGIN NULL; END $$;" `shouldBe`
                UnknownStatement { raw = "DO LANGUAGE U&\"plpg\\0073ql\" $$ BEGIN NULL; END $$" }

        it "should not treat dollar signs inside identifiers as quote delimiters" do
            parseSqlStatements "GRANT SELECT ON foo$tag$ TO role; CREATE FUNCTION f() RETURNS trigger AS $tag$BEGIN RETURN NEW; END;$tag$ language plpgsql;" `shouldBe`
                [ UnknownStatement { raw = "GRANT SELECT ON foo$tag$ TO role" }
                , (function "f") { functionBody = "BEGIN RETURN NEW; END;" }
                ]

        it "should parse tagged function dollar quotes" do
            parseSql "CREATE FUNCTION f() RETURNS trigger AS $_$ BEGIN RETURN NEW; END; $_$ language plpgsql;" `shouldBe`
                (function "f") { functionBody = " BEGIN RETURN NEW; END; " }

        it "should parse dollar signs inside a function body" do
            parseSql "CREATE FUNCTION f(a TEXT) RETURNS text AS $$ SELECT $1; $$ language sql;" `shouldBe`
                (function "f") { functionArguments = [("a", PText)], returns = PText, functionBody = " SELECT $1; ", language = "sql" }
        it "should ignore a comment inside a statement" do
            parseSql "CREATE TABLE users (\n    id UUID PRIMARY KEY, -- surrogate key\n    email TEXT NOT NULL /* the login */\n);" `shouldBe`
                StatementCreateTable (table "users")
                    { columns = [col "id" PUUID, (col "email" PText) { notNull = True }]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }

        it "should keep a comment between two statements as its own statement" do
            parseSqlStatements "CREATE TABLE a ();\n-- about b\nCREATE TABLE b ();" `shouldBe`
                [ StatementCreateTable (table "a")
                , Comment { content = " about b" }
                , StatementCreateTable (table "b")
                ]

        it "should keep the comment behind a pg_dump restrict fence" do
            parseSqlStatements "\\restrict aBcD1\n-- kept\nCREATE TABLE a ();" `shouldBe`
                [ Comment { content = "" }
                , Comment { content = " kept" }
                , StatementCreateTable (table "a")
                ]

parseSql :: Text -> Statement
parseSql sql = let [statement] = parseSqlStatements sql in statement

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError)
            Right statements -> statements

parseExpression :: Text -> Expression
parseExpression sql =
    case Megaparsec.runParser expression "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError)
            Right expr -> expr
