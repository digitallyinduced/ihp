{-|
Module: Test.IDE.SchemaDesigner.CompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.SchemaDesigner.CompilerSpec where

import Test.Hspec
import IHP.Prelude
import  IHP.IDE.SchemaDesigner.Compiler (compileSql)
import IHP.IDE.SchemaDesigner.Types
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import Test.IDE.SchemaDesigner.ParserSpec (col)

tests = do
    describe "The Schema.sql Compiler" do
        it "should compile an empty CREATE TABLE statement" do
            compileSql [CreateTable { name = "users", columns = [], constraints = [] }] `shouldBe` "CREATE TABLE users (\n\n);\n"

        it "should compile a CREATE EXTENSION for the UUID extension" do
            compileSql [CreateExtension { name = "uuid-ossp", ifNotExists = True }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

        it "should compile a line comment" do
            compileSql [Comment { content = "Comment value" }] `shouldBe` "-- Comment value\n"

        it "should compile a CREATE TABLE with columns" do
            let sql = cs [plain|CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    email TEXT NOT NULL,
    company_id UUID NOT NULL,
    picture_url TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
|]
            let statement = CreateTable
                    { name = "users"
                    , columns = [
                        Column
                            { name = "id"
                            , columnType = PUUID
                            , primaryKey = True
                            , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "firstname"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "lastname"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "password_hash"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "email"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "company_id"
                            , columnType = PUUID
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "picture_url"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            }
                        , Column
                            { name = "created_at"
                            , columnType = PTimestampWithTimezone
                            , primaryKey = False
                            , defaultValue = Just (CallExpression "NOW" [])
                            , notNull = True
                            , isUnique = False
                            }
                        ]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE with quoted identifiers" do
            compileSql [CreateTable { name = "quoted name", columns = [], constraints = [] }] `shouldBe` "CREATE TABLE \"quoted name\" (\n\n);\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Cascade
                        }
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;\n"
       
        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET NULL" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just SetNull
                        }
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET NULL;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE RESTRICT" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Restrict
                        }
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE RESTRICT;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE NO ACTION" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just NoAction
                        }
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE NO ACTION;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. (without ON DELETE)" do
            let statement = AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Nothing
                        }
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ;\n"


        it "should compile a CREATE TABLE with text default value in columns" do
            let sql = cs [plain|CREATE TABLE a (\n    content TEXT DEFAULT 'example text' NOT NULL\n);\n|]
            let statement = CreateTable
                    { name = "a"
                    , columns = [
                        Column
                            { name = "content"
                            , columnType = PText
                            , primaryKey = False
                            , defaultValue = Just (TextExpression "example text")
                            , notNull = True
                            , isUnique = False
                            }
                        ]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TYPE .. AS ENUM" do
            let sql = cs [plain|CREATE TYPE colors AS ENUM ('yellow', 'red', 'blue');\n|]
            let statement = CreateEnumType
                    { name = "colors"
                    , values = ["yellow", "red", "blue"]
                    }
            compileSql [statement] `shouldBe` sql           

        it "should compile a CREATE TABLE with (deprecated) NUMERIC, NUMERIC(x), NUMERIC (x,y), VARYING(n) columns" do
            let sql = cs [plain|CREATE TABLE deprecated_variables (\n    a NUMERIC,\n    b NUMERIC(1),\n    c NUMERIC(1,2),\n    d CHARACTER VARYING(10)\n);\n|]
            let statement = CreateTable
                    { name = "deprecated_variables"
                    , columns = 
                        [ Column
                            { name = "a"
                            , columnType = (PNumeric Nothing Nothing)
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , primaryKey = False
                            }
                        , Column
                            { name = "b"
                            , columnType = (PNumeric (Just 1) Nothing) 
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , primaryKey = False
                            }
                        , Column 
                            { name = "c"
                            , columnType = (PNumeric (Just 1) (Just 2)) 
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , primaryKey = False
                            }
                        , Column
                            { name = "d"
                            , columnType = (PVaryingN 10) 
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , primaryKey = False
                            }
                        ]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a multi-column UNIQUE (a, b) constraint" do
            let sql = cs [plain|CREATE TABLE user_followers (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    user_id UUID NOT NULL,\n    follower_id UUID NOT NULL,\n    UNIQUE(user_id, follower_id)\n);\n|]
            let statement = CreateTable
                    { name = "user_followers"
                    , columns =
                        [ col { name = "id", columnType = PUUID, primaryKey = True, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , col { name = "user_id", columnType = PUUID, notNull = True }
                        , col { name = "follower_id", columnType = PUUID, notNull = True }
                        ]
                    , constraints = [ UniqueConstraint { columnNames = [ "user_id", "follower_id" ] } ]
                    }
            compileSql [statement] `shouldBe` sql       

        it "should compile a CREATE TABLE statement with a serial id" do
            let sql = cs [plain|CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n|]
            let statement = CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PSerial, notNull = True, primaryKey = True} ]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql   

        it "should compile a CREATE TABLE statement with a bigserial id" do
            let sql = cs [plain|CREATE TABLE orders (\n    id BIGSERIAL PRIMARY KEY NOT NULL\n);\n|]
            let statement = CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PBigserial, notNull = True, primaryKey = True} ]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql   