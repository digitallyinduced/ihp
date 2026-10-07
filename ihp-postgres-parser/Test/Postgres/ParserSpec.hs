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
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True, extensionOptions = [] }

        it "should preserve a missing IF NOT EXISTS clause" do
            parseSql "CREATE EXTENSION \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = False, extensionOptions = [] }

        it "should parse an CREATE EXTENSION with schema suffix" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" WITH SCHEMA public;" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True, extensionOptions = [ExtensionSchema "public"] }

        it "should round-trip quoted extension schema names" do
            let statement = parseSql "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA \"geo.data\";"
            parseSql (compileSql [statement]) `shouldBe` statement

        it "should fold an unquoted extension schema to lowercase" do
            parseSql "CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA Geo;" `shouldBe` CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionSchema "geo"] }

        it "should parse CREATE EXTENSION version and cascade options" do
            parseSql "CREATE EXTENSION pg_trgm VERSION '1.6' CASCADE;" `shouldBe`
                CreateExtension { name = "pg_trgm", ifNotExists = False, extensionOptions = [ExtensionVersion "1.6", ExtensionCascade] }

        it "should decode doubled quotes in extension versions" do
            parseSql "CREATE EXTENSION extension_name VERSION '1''beta';" `shouldBe`
                CreateExtension { name = "extension_name", ifNotExists = False, extensionOptions = [ExtensionVersion "1'beta"] }

        describe "parseCreateExtensionMigration" do
            it "accepts one or more extension statements and comments" do
                parseCreateExtensionMigration "-- Required for earthdistance\nCREATE EXTENSION IF NOT EXISTS cube;\nCREATE EXTENSION IF NOT EXISTS \"earthdistance\" WITH SCHEMA public;"
                    `shouldBe` Right
                        [ CreateExtension { name = "cube", ifNotExists = True, extensionOptions = [] }
                        , CreateExtension { name = "earthdistance", ifNotExists = True, extensionOptions = [ExtensionSchema "public"] }
                        ]

            it "is case insensitive" do
                parseCreateExtensionMigration "create extension if not exists PG_TRGM;"
                    `shouldBe` Right [CreateExtension { name = "pg_trgm", ifNotExists = True, extensionOptions = [] }]

            it "accepts PostgreSQL extension options" do
                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS PostGIS WITH SCHEMA public VERSION '3.4.2' CASCADE;"
                    `shouldBe` Right [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionSchema "public", ExtensionVersion "3.4.2", ExtensionCascade] }]

                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS postgis WITH VERSION stable CASCADE;"
                    `shouldBe` Right [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionVersion "stable", ExtensionCascade] }]

                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS postgis CASCADE VERSION '3.4.2' SCHEMA geo;"
                    `shouldBe` Right [CreateExtension { name = "postgis", ifNotExists = True, extensionOptions = [ExtensionCascade, ExtensionVersion "3.4.2", ExtensionSchema "geo"] }]

            it "preserves quoted extension names" do
                parseCreateExtensionMigration "CREATE EXTENSION IF NOT EXISTS \"MixedCase\";"
                    `shouldBe` Right [CreateExtension { name = "MixedCase", ifNotExists = True, extensionOptions = [] }]

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

        it "should parse PostgreSQL 18 named NOT NULL constraints" do
            let sql = "CREATE TABLE context_search_email_binary_signatures (\n                    embedding_provider text CONSTRAINT context_search_email_binary_signatu_embedding_provider_not_null NOT NULL,\n                    embedding_dimensions integer CONSTRAINT context_search_email_binary_signa_embedding_dimensions_not_null NOT NULL,\n                    embedding_model text\n                );"
            parseSql sql `shouldBe` StatementCreateTable (table "context_search_email_binary_signatures")
                    { columns =
                        [ (col "embedding_provider" PText) { notNull = True, notNullConstraintName = Just "context_search_email_binary_signatu_embedding_provider_not_null" }
                        , (col "embedding_dimensions" PInt) { notNull = True, notNullConstraintName = Just "context_search_email_binary_signa_embedding_dimensions_not_null" }
                        , col "embedding_model" PText
                        ]
                    }

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
                        , onUpdate = Nothing
                        , constraintDeferrable = Nothing
                        , constraintDeferrableType = Nothing
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
                        , onUpdate = Nothing
                        , constraintDeferrable = Nothing
                        , constraintDeferrableType = Nothing
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
                    , column = (col "embedding" (PCustomType "vector(1536)")) { defaultValue = Just (VarExpression "NULL") }
                    }

        it "should preserve custom type modifier contents" do
            parseSql "ALTER TABLE knowledge_chunks ADD COLUMN embedding VECTOR( 1536 ) DEFAULT NULL;" `shouldBe` AddColumn
                    { tableName = "knowledge_chunks"
                    , column = (col "embedding" (PCustomType "vector( 1536 )")) { defaultValue = Just (VarExpression "NULL") }
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
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, private, pg_temp"
                            }
                        ]
                    }

        it "should preserve pg_dump function attributes" do
            let sql = "CREATE FUNCTION current_organization_id() RETURNS uuid\n    LANGUAGE sql STABLE PARALLEL SAFE SECURITY DEFINER COST 2.5\n    SET search_path = public, pg_temp\n    AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "current_organization_id"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = True
                    , functionAttributes = ["STABLE", "PARALLEL SAFE", "COST 2.5"]
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, pg_temp"
                            }
                        ]
                    }

        it "should parse CREATE FUNCTION returning SETOF" do
            let sql = "CREATE FUNCTION search_ids(query text) RETURNS setof uuid LANGUAGE sql AS $$SELECT 1;$$;"
            parseSql sql `shouldBe`
                (function "search_ids")
                    { functionArguments = [("query", PText)]
                    , functionBody = "SELECT 1;"
                    , returns = PSetOf PUUID
                    , language = "sql"
                    }

        it "should parse CREATE FUNCTION returning TABLE" do
            let sql = "CREATE FUNCTION search_rows() RETURNS table(id uuid, label text) LANGUAGE sql AS $$SELECT 1;$$;"
            parseSql sql `shouldBe`
                (function "search_rows")
                    { functionBody = "SELECT 1;"
                    , returns = PTable [("id", PUUID), ("label", PText)]
                    , language = "sql"
                    }

        it "should parse qualified types in function return shapes" do
            let setReturning = parseSql "CREATE FUNCTION widgets() RETURNS SETOF private.users LANGUAGE sql AS $$SELECT NULL;$$;"
            let tableReturning = parseSql "CREATE FUNCTION widgets() RETURNS TABLE (status private.status) LANGUAGE sql AS $$SELECT NULL;$$;"
            setReturning.returns `shouldBe` PSetOf (PCustomType "private.users")
            tableReturning.returns `shouldBe` PTable [("status", PCustomType "private.status")]
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
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "TimeZone"
                            , settingValue = "'Asia/Tokyo'"
                            }
                        ]
                    }

        it "should keep function attribute keywords after SET commas" do
            let sql = "CREATE FUNCTION uses_stable_schema() RETURNS uuid LANGUAGE sql SET search_path = public, stable AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "uses_stable_schema"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "search_path"
                            , settingValue = "public, stable"
                            }
                        ]
                    }

        it "should parse SQL-escaped quotes in function settings" do
            let sql = "CREATE FUNCTION configured() RETURNS uuid LANGUAGE sql SET application_name = 'it''s enabled' AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "configured"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "application_name"
                            , settingValue = "'it''s enabled'"
                            }
                        ]
                    }

        it "should parse exponent notation in numeric function attributes" do
            let sql = "CREATE FUNCTION estimated() RETURNS SETOF uuid LANGUAGE sql COST 1e-6 ROWS 1E6 AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "estimated"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PSetOf PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = ["COST 1e-6", "ROWS 1E6"]
                    , functionSettings = []
                    }

        it "should parse leading-dot numeric function attributes" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS SETOF uuid LANGUAGE sql COST .5 ROWS .25 AS $$SELECT 1;$$;"
            parsed.functionAttributes `shouldBe` ["COST .5", "ROWS .25"]

        it "should parse trailing-dot numeric function attributes" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS SETOF uuid LANGUAGE sql COST 5. ROWS 10. AS $$SELECT 1;$$;"
            parsed.functionAttributes `shouldBe` ["COST 5.", "ROWS 10."]

        it "should parse table-returning function signatures" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS TABLE (id uuid, label text) LANGUAGE sql ROWS 10 AS $$SELECT NULL, NULL;$$;"
            parsed.returns `shouldBe` PTable [("id", PUUID), ("label", PText)]

        it "should fold only unquoted RETURNS TABLE column names" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS TABLE (Result text, \"ExactResult\" text) LANGUAGE sql AS $$SELECT NULL, NULL;$$;"
            parsed.returns `shouldBe` PTable [("result", PText), ("ExactResult", PText)]

        it "should decode doubled quotes in RETURNS TABLE column names" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS TABLE (\"result\"\"code\" text) LANGUAGE sql AS $$SELECT NULL;$$;"
            parsed.returns `shouldBe` PTable [("result\"code", PText)]

        it "should parse function attributes after the body" do
            let parsed = parseSql "CREATE FUNCTION estimated() RETURNS integer AS $$SELECT 1$$ LANGUAGE sql IMMUTABLE;"
            parsed.language `shouldBe` "sql"
            parsed.functionAttributes `shouldBe` ["IMMUTABLE"]

        it "should parse schema-qualified set-returning custom types" do
            let parsed = parseSql "CREATE FUNCTION widgets() RETURNS SETOF private.widget LANGUAGE sql AS $$SELECT NULL;$$;"
            parsed.returns `shouldBe` PSetOf (PCustomType "private.widget")

        it "should fold only unquoted custom type identifiers" do
            let unquoted = parseSql "CREATE FUNCTION widgets() RETURNS SETOF Private.Widget(CustomCase) LANGUAGE sql AS $$SELECT NULL;$$;"
            let quoted = parseSql "CREATE FUNCTION widgets() RETURNS SETOF private.\"Widget\" LANGUAGE sql AS $$SELECT NULL;$$;"
            unquoted.returns `shouldBe` PSetOf (PCustomType "private.widget(CustomCase)")
            quoted.returns `shouldBe` PSetOf (PCustomType "private.\"Widget\"")

        it "should parse schema-qualified custom types in RETURNS TABLE" do
            let parsed = parseSql "CREATE FUNCTION widgets() RETURNS TABLE (widget private.widget) LANGUAGE sql AS $$SELECT NULL;$$;"
            parsed.returns `shouldBe` PTable [("widget", PCustomType "private.widget")]

        it "should preserve TRANSFORM function attributes" do
            let parsed = parseSql "CREATE FUNCTION transformed(value private.widget) RETURNS private.widget LANGUAGE plpgsql TRANSFORM FOR TYPE private.widget AS $$BEGIN RETURN value; END;$$;"
            parsed.functionAttributes `shouldBe` ["TRANSFORM FOR TYPE private.widget"]

        it "should preserve every type in a TRANSFORM list" do
            let parsed = parseSql "CREATE FUNCTION transformed() RETURNS uuid LANGUAGE plpgsql TRANSFORM FOR TYPE private.widget, FOR TYPE private.gadget AS $$BEGIN RETURN NULL; END;$$;"
            parsed.functionAttributes `shouldBe` ["TRANSFORM FOR TYPE private.widget, FOR TYPE private.gadget"]

        it "should preserve complete types in TRANSFORM lists" do
            let parsed = parseSql "CREATE FUNCTION transformed() RETURNS uuid LANGUAGE plpgsql TRANSFORM FOR TYPE double precision, FOR TYPE hstore[] AS $$BEGIN RETURN NULL; END;$$;"
            parsed.functionAttributes `shouldBe` ["TRANSFORM FOR TYPE DOUBLE PRECISION, FOR TYPE hstore[]"]

        it "should parse dollar-quoted function settings containing whitespace" do
            let parsed = parseSql "CREATE FUNCTION configured() RETURNS uuid LANGUAGE sql SET application_name = $worker$batch worker$worker$ AS $$SELECT NULL;$$;"
            parsed.functionSettings `shouldBe` [FunctionSetting { settingName = "application_name", settingValue = "$worker$batch worker$worker$" }]

        it "should parse escape-string function settings containing whitespace" do
            let sql = "CREATE FUNCTION configured_path() RETURNS uuid LANGUAGE sql SET application_name = E'C:\\\\Program Files' AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "configured_path"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = []
                    , functionSettings =
                        [ FunctionSetting
                            { settingName = "application_name"
                            , settingValue = "E'C:\\\\Program Files'"
                            }
                        ]
                    }

        it "should parse Unicode-escape function settings containing whitespace" do
            let parsed = parseSql "CREATE FUNCTION configured() RETURNS uuid LANGUAGE sql SET application_name = U&'batch worker' AS $$SELECT 1;$$;"
            parsed.functionSettings `shouldBe` [FunctionSetting { settingName = "application_name", settingValue = "U&'batch worker'" }]

        it "should parse quoted identifiers in function settings" do
            let sql = "CREATE FUNCTION configured_schema() RETURNS uuid LANGUAGE sql SET search_path = \"tenant schema\", public AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "configured_schema"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = []
                    , functionSettings = [FunctionSetting { settingName = "search_path", settingValue = "\"tenant schema\", public" }]
                    }

        it "should preserve function SUPPORT attributes" do
            let sql = "CREATE FUNCTION supported() RETURNS uuid LANGUAGE sql SUPPORT public.my_support AS $$SELECT 1;$$;"
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "supported"
                    , functionArguments = []
                    , functionBody = "SELECT 1;"
                    , orReplace = False
                    , returns = PUUID
                    , language = "sql"
                    , securityDefiner = False
                    , functionAttributes = ["SUPPORT my_support"]
                    , functionSettings = []
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
                    , functionAttributes = []
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
                    , functionAttributes = []
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
                    , functionAttributes = []
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
                    , functionAttributes = []
                    , functionSettings = []
                    }

        it "should parse DROP FUNCTION with a non-public schema-qualified name" do
            -- DROP FUNCTION must accept the same schema-qualified names as CREATE FUNCTION
            parseSql "DROP FUNCTION private.sync_access;" `shouldBe` DropFunction { functionName = "private.sync_access" }

        it "should normalize the default public schema away on DROP FUNCTION" do
            parseSql "DROP FUNCTION public.sync_access;" `shouldBe` DropFunction { functionName = "sync_access" }

        it "should preserve a non-public schema in a SUPPORT function" do
            let parsed = parseSql "CREATE FUNCTION public.f(value integer) RETURNS integer LANGUAGE sql SUPPORT private.my_support AS $$ SELECT value $$;"
            parsed.functionAttributes `shouldBe` ["SUPPORT private.my_support"]

        it "should preserve quoted identifiers in a SUPPORT function" do
            let parsed = parseSql "CREATE FUNCTION public.f(value integer) RETURNS integer LANGUAGE sql SUPPORT public.\"MySupport\" AS $$ SELECT value $$;"
            parsed.functionAttributes `shouldBe` ["SUPPORT \"MySupport\""]

        it "should normalize a redundantly quoted public SUPPORT schema" do
            let parsed = parseSql "CREATE FUNCTION public.f(value integer) RETURNS integer LANGUAGE sql SUPPORT \"public\".my_support AS $$ SELECT value $$;"
            parsed.functionAttributes `shouldBe` ["SUPPORT my_support"]

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
            parseSql "CREATE SEQUENCE a;" `shouldBe` CreateSequence { name = "a", sequenceOptions = [] }

        it "should preserve pg_dump sequence options" do
            let sql = "CREATE SEQUENCE a AS bigint START WITH 1 INCREMENT BY 2 NO MINVALUE MAXVALUE 99 CACHE 4 NO CYCLE;"
            parseSql (compileSql [parseSql sql]) `shouldBe` parseSql sql

        it "should not read INCREMENT as the IN expression operator" do
            parseSql "CREATE SEQUENCE a START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;"
                `shouldBe` CreateSequence
                    { name = "a"
                    , sequenceOptions =
                        [ SequenceStart (IntExpression 1)
                        , SequenceIncrement (IntExpression 1)
                        , SequenceNoMinValue
                        , SequenceNoMaxValue
                        , SequenceCache (IntExpression 1)
                        ]
                    }

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

        it "should preserve policy roles" do
            parseSql "CREATE POLICY access ON tickets FOR SELECT TO ihp_authenticated, PUBLIC USING (active);" `shouldBe`
                (policy "access" "tickets")
                    { action = Just PolicyForSelect
                    , roles = [PolicyRole "ihp_authenticated", SpecialPolicyRole "PUBLIC"]
                    , using = Just (VarExpression "active")
                    }

        it "should distinguish quoted policy roles from special role specifications" do
            parseSql "CREATE POLICY access ON tickets TO \"current_user\", CURRENT_USER;" `shouldBe`
                (policy "access" "tickets")
                    { roles = [QuotedPolicyRole "current_user", SpecialPolicyRole "CURRENT_USER"] }

        it "should fold unquoted policy roles and decode quoted role escapes" do
            parseSql "CREATE POLICY access ON tickets TO MyRole, \"ops\"\"team\";" `shouldBe`
                (policy "access" "tickets")
                    { roles = [PolicyRole "myrole", QuotedPolicyRole "ops\"team"] }

        it "should fold only ASCII characters in unquoted policy roles" do
            parseSql "CREATE POLICY access ON tickets TO ÄRole;" `shouldBe`
                (policy "access" "tickets") { roles = [PolicyRole "Ärole"] }

        it "should recognize special policy roles with ASCII folding only" do
            parseSql "CREATE POLICY access ON tickets TO publıc;" `shouldBe`
                (policy "access" "tickets") { roles = [PolicyRole "publıc"] }

        it "should parse dollar signs in unquoted policy roles" do
            parseSql "CREATE POLICY access ON tickets TO app$user;" `shouldBe`
                (policy "access" "tickets") { roles = [PolicyRole "app$user"] }

        it "should parse FORCE ROW LEVEL SECURITY" do
            parseSql "ALTER TABLE tickets FORCE ROW LEVEL SECURITY;" `shouldBe`
                ForceRowLevelSecurity { tableName = "tickets" }

        it "should parse NO FORCE ROW LEVEL SECURITY" do
            parseSql "ALTER TABLE tickets NO FORCE ROW LEVEL SECURITY;" `shouldBe`
                NoForceRowLevelSecurity { tableName = "tickets" }

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
            normalizeComment "COMMENT/* keyword trivia */ON TABLE users IS/* value trivia */'owner records'" `shouldBe`
                Just "COMMENT ON TABLE users IS 'owner records'"

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
            unsetComment "COMMENT ON TABLE users IS/* value trivia */'documented'" `shouldBe`
                Just "COMMENT ON TABLE users IS NULL"

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

        it "should reject dollar quote tags starting with a digit" do
            parseSqlText "CREATE FUNCTION f() RETURNS text AS $1$body$1$ language sql;" `shouldSatisfy` isLeft
            parseSqlText "DO $1$body$1$;" `shouldSatisfy` isLeft

        it "should parse dollar signs inside a function body" do
            parseSql "CREATE FUNCTION f(a TEXT) RETURNS text AS $$ SELECT $1; $$ language sql;" `shouldBe`
                (function "f") { functionArguments = [("a", PText)], returns = PText, functionBody = " SELECT $1; ", language = "sql" }

        it "should parse an operator behind an integer literal" do
            parseExpression "a > 0 AND b > 0" `shouldBe`
                AndExpression
                    (GreaterThanExpression (VarExpression "a") (IntExpression 0))
                    (GreaterThanExpression (VarExpression "b") (IntExpression 0))

        it "should parse an operator behind a double literal" do
            parseExpression "a > 0.5 AND b > 0.5" `shouldBe`
                AndExpression
                    (GreaterThanExpression (VarExpression "a") (NumericExpression "0.5"))
                    (GreaterThanExpression (VarExpression "b") (NumericExpression "0.5"))

        it "should parse arithmetic operators" do
            parseExpression "a + b <= 100" `shouldBe`
                LessThanOrEqualToExpression
                    (BinaryOperatorExpression "+" (VarExpression "a") (VarExpression "b"))
                    (IntExpression 100)

        it "should give multiplication a tighter precedence than addition" do
            parseExpression "a + b * c" `shouldBe`
                BinaryOperatorExpression "+"
                    (VarExpression "a")
                    (BinaryOperatorExpression "*" (VarExpression "b") (VarExpression "c"))

        it "should give exponentiation tighter precedence than multiplication" do
            parseExpression "2 * 3 ^ 2" `shouldBe`
                BinaryOperatorExpression "*"
                    (IntExpression 2)
                    (BinaryOperatorExpression "^" (IntExpression 3) (IntExpression 2))

        it "should parse regular expression operators" do
            parseExpression "code ~ '^[A-Z]{3}$'" `shouldBe`
                BinaryOperatorExpression "~" (VarExpression "code") (TextExpression "^[A-Z]{3}$")

        it "should parse PostgreSQL JSON operators" do
            parseExpression "metadata ->> 'kind' = 'invoice'" `shouldBe`
                EqExpression
                    (BinaryOperatorExpression "->>" (VarExpression "metadata") (TextExpression "kind"))
                    (TextExpression "invoice")
            parseExpression "metadata ? 'kind'" `shouldBe`
                BinaryOperatorExpression "?" (VarExpression "metadata") (TextExpression "kind")
            parseExpression "payload @> '{\"kind\":\"booking\"}'" `shouldBe`
                BinaryOperatorExpression "@>" (VarExpression "payload") (TextExpression "{\"kind\":\"booking\"}")
            parseExpression "payload <@ expected #> '{items}'" `shouldBe`
                BinaryOperatorExpression "#>"
                    (BinaryOperatorExpression "<@" (VarExpression "payload") (VarExpression "expected"))
                    (TextExpression "{items}")
            parseExpression "payload ?| keys" `shouldBe`
                BinaryOperatorExpression "?|" (VarExpression "payload") (VarExpression "keys")
            parseExpression "payload ?& keys" `shouldBe`
                BinaryOperatorExpression "?&" (VarExpression "payload") (VarExpression "keys")
            parseExpression "payload #>> '{items,0}'" `shouldBe`
                BinaryOperatorExpression "#>>" (VarExpression "payload") (TextExpression "{items,0}")
            parseExpression "left_value ## right_value" `shouldBe`
                BinaryOperatorExpression "##" (VarExpression "left_value") (VarExpression "right_value")

        it "should keep concatenation at the generic operator precedence" do
            parseExpression "defaults || payload ->> 'name'" `shouldBe`
                BinaryOperatorExpression "->>"
                    (ConcatenationExpression (VarExpression "defaults") (VarExpression "payload"))
                    (TextExpression "name")

        it "should parse user-defined operators beginning with arithmetic characters" do
            parseExpression "lhs +> rhs" `shouldBe`
                BinaryOperatorExpression "+>" (VarExpression "lhs") (VarExpression "rhs")
            parseExpression "lhs ^@ rhs" `shouldBe`
                BinaryOperatorExpression "^@" (VarExpression "lhs") (VarExpression "rhs")

        it "should ignore WITH inside PostgreSQL escape strings in exclusion elements" do
            let parsed = parseSql "CREATE TABLE bookings (EXCLUDE (room_id || E'foo\\' WITH bar' WITH =));"
            parsed.unsafeGetCreateTable.constraints `shouldBe`
                [ExcludeConstraint Nothing [ExcludeConstraintElement "room_id || E'foo\\' WITH bar'" "="] Nothing Nothing]

        it "should parse BETWEEN and NOT IN" do
            parseExpression "month BETWEEN 1 AND 12" `shouldBe`
                AndExpression
                    (GreaterThanOrEqualToExpression (VarExpression "month") (IntExpression 1))
                    (LessThanOrEqualToExpression (VarExpression "month") (IntExpression 12))
            parseExpression "kind NOT IN ('draft', 'void')" `shouldBe`
                BinaryOperatorExpression "NOT IN"
                    (VarExpression "kind")
                    (InArrayExpression [TextExpression "draft", TextExpression "void"])
            parseExpression "age NOT BETWEEN 13 AND 19" `shouldBe`
                NotExpression
                    (AndExpression
                        (GreaterThanOrEqualToExpression (VarExpression "age") (IntExpression 13))
                        (LessThanOrEqualToExpression (VarExpression "age") (IntExpression 19)))

        it "should parse BETWEEN after arithmetic expressions" do
            parseExpression "subtotal + tax BETWEEN minimum + 1 AND maximum" `shouldBe`
                AndExpression
                    (GreaterThanOrEqualToExpression
                        (BinaryOperatorExpression "+" (VarExpression "subtotal") (VarExpression "tax"))
                        (BinaryOperatorExpression "+" (VarExpression "minimum") (IntExpression 1)))
                    (LessThanOrEqualToExpression
                        (BinaryOperatorExpression "+" (VarExpression "subtotal") (VarExpression "tax"))
                        (VarExpression "maximum"))

        it "should parse generic operators in BETWEEN bounds" do
            parseExpression "value BETWEEN bounds ->> 'lower' AND bounds ->> 'upper'" `shouldBe`
                AndExpression
                    (GreaterThanOrEqualToExpression (VarExpression "value") (BinaryOperatorExpression "->>" (VarExpression "bounds") (TextExpression "lower")))
                    (LessThanOrEqualToExpression (VarExpression "value") (BinaryOperatorExpression "->>" (VarExpression "bounds") (TextExpression "upper")))

        it "should parse concatenation in BETWEEN bounds" do
            parseExpression "value BETWEEN prefix || suffix AND upper" `shouldBe`
                AndExpression
                    (GreaterThanOrEqualToExpression (VarExpression "value") (ConcatenationExpression (VarExpression "prefix") (VarExpression "suffix")))
                    (LessThanOrEqualToExpression (VarExpression "value") (VarExpression "upper"))

        it "should give AT TIME ZONE precedence over comparisons" do
            parseExpression "cutoff < created_at AT TIME ZONE 'UTC'" `shouldBe`
                LessThanExpression
                    (VarExpression "cutoff")
                    (BinaryOperatorExpression "AT TIME ZONE" (VarExpression "created_at") (TextExpression "UTC"))

            parseExpression "created_at AT /* normalize */ TIME\nZONE 'UTC'" `shouldBe`
                BinaryOperatorExpression "AT TIME ZONE" (VarExpression "created_at") (TextExpression "UTC")

        it "should parse typed PostgreSQL literals" do
            parseExpression "closed_at - INTERVAL '30 days' > opened_at" `shouldBe`
                GreaterThanExpression
                    (BinaryOperatorExpression "-"
                        (VarExpression "closed_at")
                        (TypeCastExpression (TextExpression "30 days") (PInterval Nothing)))
                    (VarExpression "opened_at")
            parseExpression "TIMESTAMPTZ '2026-08-09 18:00:00+00'" `shouldBe`
                TypeCastExpression (TextExpression "2026-08-09 18:00:00+00") PTimestampWithTimezone
            parseExpression "created_at + INTERVAL '1' DAY" `shouldBe`
                BinaryOperatorExpression "+"
                    (VarExpression "created_at")
                    (TypeCastExpression (TextExpression "1") (PInterval (Just "DAY")))

        it "should parse expression-based EXCLUDE constraints" do
            parseSql "ALTER TABLE bookings ADD CONSTRAINT bookings_no_overlap EXCLUDE USING gist (room_id WITH =, daterange(starts_on, ends_on) WITH &&);" `shouldBe`
                AddConstraint
                    { tableName = "bookings"
                    , constraint = ExcludeConstraint
                        { name = Just "bookings_no_overlap"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "room_id", operator = "=" }
                            , ExcludeConstraintElement { element = "daterange(starts_on, ends_on)", operator = "&&" }
                            ]
                        , predicate = Nothing
                        , indexType = Just Gist
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should prefer the longest regular expression operator" do
            parseExpression "code !~* 'x'" `shouldBe`
                BinaryOperatorExpression "!~*" (VarExpression "code") (TextExpression "x")

        it "should parse LIKE without consuming an identifier that starts with it" do
            parseExpression "name LIKE 'a%'" `shouldBe`
                BinaryOperatorExpression "LIKE" (VarExpression "name") (TextExpression "a%")
            parseExpression "likelihood" `shouldBe` VarExpression "likelihood"

        it "should preserve LIKE escape clauses" do
            parseExpression "code LIKE 'A!_%' ESCAPE '!'" `shouldBe`
                BinaryOperatorExpression "ESCAPE"
                    (BinaryOperatorExpression "LIKE" (VarExpression "code") (TextExpression "A!_%"))
                    (TextExpression "!")
            parseExpression "code LIKE pattern ESCAPE escape_prefix || ''" `shouldBe`
                BinaryOperatorExpression "ESCAPE"
                    (BinaryOperatorExpression "LIKE" (VarExpression "code") (VarExpression "pattern"))
                    (ConcatenationExpression (VarExpression "escape_prefix") (TextExpression ""))

        it "should bind LIKE before prefix NOT and allow trivia in NOT LIKE" do
            parseExpression "NOT name LIKE 'a%'" `shouldBe`
                NotExpression (BinaryOperatorExpression "LIKE" (VarExpression "name") (TextExpression "a%"))
            parseExpression "name NOT /* pattern */ LIKE 'a%'" `shouldBe`
                BinaryOperatorExpression "NOT LIKE" (VarExpression "name") (TextExpression "a%")

        it "should read != as the canonical <> operator" do
            parseExpression "a != b" `shouldBe` NotEqExpression (VarExpression "a") (VarExpression "b")

        it "should bind regular expression operators before comparisons" do
            parseExpression "flag <> code ~ '^x'" `shouldBe`
                NotEqExpression
                    (VarExpression "flag")
                    (BinaryOperatorExpression "~" (VarExpression "code") (TextExpression "^x"))

        it "should bind arithmetic before JSON operators" do
            parseExpression "payload -> 0 + 1" `shouldBe`
                BinaryOperatorExpression "->"
                    (VarExpression "payload")
                    (BinaryOperatorExpression "+" (IntExpression 0) (IntExpression 1))

        it "should parse WITH only as an exclusion-element delimiter" do
            parseSql "CREATE TABLE reservations (EXCLUDE (overlaps_with WITH =));" `shouldBe`
                StatementCreateTable (table "reservations")
                    { constraints =
                        [ ExcludeConstraint
                            { name = Nothing
                            , excludeElements = [ExcludeConstraintElement { element = "overlaps_with", operator = "=" }]
                            , predicate = Nothing
                            , indexType = Nothing
                            }
                        ]
                    }

        it "should ignore quoted WITH text inside exclusion elements" do
            parseSql "CREATE TABLE reservations (EXCLUDE ((name || ' WITH ') WITH =));" `shouldBe`
                StatementCreateTable (table "reservations")
                    { constraints =
                        [ ExcludeConstraint
                            { name = Nothing
                            , excludeElements = [ExcludeConstraintElement { element = "(name || ' WITH ')", operator = "=" }]
                            , predicate = Nothing
                            , indexType = Nothing
                            }
                        ]
                    }

        it "should ignore WITH inside comments in exclusion elements" do
            parseSql "CREATE TABLE reservations (EXCLUDE (room_id /* WITH marker */ WITH =));" `shouldBe`
                StatementCreateTable (table "reservations")
                    { constraints =
                        [ ExcludeConstraint
                            { name = Nothing
                            , excludeElements = [ExcludeConstraintElement { element = "room_id /* WITH marker */", operator = "=" }]
                            , predicate = Nothing
                            , indexType = Nothing
                            }
                        ]
                    }

        it "should ignore WITH inside dollar-quoted exclusion literals" do
            parseSql "CREATE TABLE reservations (EXCLUDE ((name || $tag$ WITH $tag$) WITH =));" `shouldBe`
                StatementCreateTable (table "reservations")
                    { constraints =
                        [ ExcludeConstraint
                            { name = Nothing
                            , excludeElements = [ExcludeConstraintElement { element = "(name || $tag$ WITH $tag$)", operator = "=" }]
                            , predicate = Nothing
                            , indexType = Nothing
                            }
                        ]
                    }

        it "should parse compact exclusion operators" do
            parseSql "CREATE TABLE reservations (EXCLUDE (room_id WITH=));" `shouldBe`
                StatementCreateTable (table "reservations")
                    { constraints =
                        [ ExcludeConstraint
                            { name = Nothing
                            , excludeElements = [ExcludeConstraintElement { element = "room_id", operator = "=" }]
                            , predicate = Nothing
                            , indexType = Nothing
                            }
                        ]
                    }

        it "should accept punctuation before the exclusion WITH delimiter" do
            let parsed = parseSql "CREATE TABLE reservations (EXCLUDE ((lower(room_id))WITH=));"
            parsed.unsafeGetCreateTable.constraints `shouldBe`
                [ ExcludeConstraint
                    { name = Nothing
                    , excludeElements = [ExcludeConstraintElement { element = "(lower(room_id))", operator = "=" }]
                    , predicate = Nothing
                    , indexType = Nothing
                    }
                ]

        it "should cast the operand rather than the sum" do
            parseExpression "a::integer + 1" `shouldBe`
                BinaryOperatorExpression "+"
                    (TypeCastExpression (VarExpression "a") PInt)
                    (IntExpression 1)

        it "should parse a CHECK constraint combining comparisons with AND" do
            parseSql "CREATE TABLE t (a INT, b INT, CONSTRAINT t_positive CHECK (a > 0 AND b > 0));" `shouldBe`
                StatementCreateTable (table "t")
                    { columns = [col "a" PInt, col "b" PInt]
                    , constraints =
                        [ CheckConstraint
                            { name = Just "t_positive"
                            , checkExpression =
                                AndExpression
                                    (GreaterThanExpression (VarExpression "a") (IntExpression 0))
                                    (GreaterThanExpression (VarExpression "b") (IntExpression 0))
                            }
                        ]
                    }
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
