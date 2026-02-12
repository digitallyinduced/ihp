{-|
Module: Postgres.ParserSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Postgres.ParserSpec where

import Prelude
import Test.Hspec
import IHP.Postgres.Parser
import IHP.Postgres.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Megaparsec as Megaparsec
import GHC.IO (evaluate)

spec :: Spec
spec = do
    describe "The Schema.sql Parser" do
        it "should parse an empty CREATE TABLE statement" do
            parseSql "CREATE TABLE users ();"  `shouldBe` StatementCreateTable CreateTable { name = "users", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [], unlogged = False }

        it "should parse an CREATE EXTENSION for the UUID extension" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

        it "should parse an CREATE EXTENSION with schema suffix" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" WITH SCHEMA public;" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

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
                    }

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` StatementCreateTable (table "quoted name")

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
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }

        it "should parse a CREATE UNIQUE INDEX statement" do
            parseSql "CREATE UNIQUE INDEX users_index ON users (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }

        it "should parse 'ENABLE ROW LEVEL SECURITY' statements" do
            parseSql "ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;" `shouldBe` EnableRowLevelSecurity { tableName = "tasks" }

        it "should parse 'CREATE POLICY' statements" do
            parseSql "CREATE POLICY \"Users can manage their tasks\" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());" `shouldBe` CreatePolicy
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

        it "should parse 'DROP TABLE ..' statements" do
            parseSql "DROP TABLE tasks;" `shouldBe` DropTable { tableName = "tasks" }

        it "should parse 'DROP TYPE ..' statements" do
            parseSql "DROP TYPE colors;" `shouldBe` DropEnumType { name = "colors" }

        it "should parse 'CREATE SEQUENCE ..' statements" do
            parseSql "CREATE SEQUENCE a;" `shouldBe` CreateSequence { name = "a" }

        it "should parse 'BEGIN' statements" do
            parseSql "BEGIN;" `shouldBe` Begin

        it "should parse 'COMMIT' statements" do
            parseSql "COMMIT;" `shouldBe` Commit

        it "should parse 'CREATE UNLOGGED TABLE' statement" do
            parseSql "CREATE UNLOGGED TABLE pg_large_notifications ();"  `shouldBe` StatementCreateTable CreateTable { name = "pg_large_notifications", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [], unlogged = True }

        it "should parse positive IntExpression's" do
            parseExpression "1" `shouldBe` (IntExpression 1)

        it "should parse negative IntExpression's" do
            parseExpression "-1" `shouldBe` (IntExpression (-1))

        it "should parse positive DoubleExpression's" do
            parseExpression "1.337" `shouldBe` (DoubleExpression 1.337)

        it "should parse negative DoubleExpression's" do
            parseExpression "-1.337" `shouldBe` (DoubleExpression (-1.337))

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
