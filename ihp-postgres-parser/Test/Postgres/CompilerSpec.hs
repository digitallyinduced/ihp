{-|
Module: Postgres.CompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Postgres.CompilerSpec where

import Prelude
import Test.Hspec
import IHP.Postgres.Compiler (compileSql)
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
            compileSql [CreateExtension { name = "uuid-ossp", ifNotExists = True }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

        it "should compile a line comment" do
            compileSql [Comment { content = " Comment value" }] `shouldBe` "-- Comment value\n"

        it "should compile a empty line comments" do
            compileSql [Comment { content = "" }, Comment { content = "" }] `shouldBe` "--\n--\n"

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
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX statement" do
            let sql = "CREATE UNIQUE INDEX users_index ON users (user_name);\n"
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = []}]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile 'ENABLE ROW LEVEL SECURITY' statements" do
            let sql = "ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;\n"
            let statements = [EnableRowLevelSecurity { tableName = "tasks" }]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE POLICY' statements" do
            let sql = "CREATE POLICY \"Users can manage their tasks\" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());\n"
            let policy = CreatePolicy
                    { name = "Users can manage their tasks"
                    , action = Nothing
                    , tableName = "tasks"
                    , using = Just (
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
            compileSql [policy] `shouldBe` sql

        it "should compile 'DROP TABLE ..' statements" do
            let sql = "DROP TABLE tasks;\n"
            let statements = [ DropTable { tableName = "tasks" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE SEQUENCE ..' statements" do
            let sql = "CREATE SEQUENCE a;\n"
            let statements = [ CreateSequence { name = "a" } ]
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

        it "should compile 'CREATE UNLOGGED TABLE' statements" do
            let sql = "CREATE UNLOGGED TABLE pg_large_notifications (\n\n);\n"
            let statements = [
                        StatementCreateTable (table "pg_large_notifications")
                            { unlogged = True
                            }
                        ]
            compileSql statements `shouldBe` sql

col :: Text -> PostgresType -> Column
col columnName columnType = Column
    { name = columnName
    , columnType = columnType
    , defaultValue = Nothing
    , notNull = False
    , isUnique = False
    , generator = Nothing
    }

table :: Text -> CreateTable
table name = CreateTable
    { name = name
    , columns = []
    , primaryKeyConstraint = PrimaryKeyConstraint []
    , constraints = []
    , unlogged = False
    }

parseSql :: Text -> Statement
parseSql sql =
    case Megaparsec.runParser parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError)
            Right [statement] -> statement
            Right statements -> error $ "Expected single statement but got: " <> show (length statements)
