{-|
Module: Test.IDE.SchemaDesigner.ParserSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.SchemaDesigner.ParserSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import IHP.IDE.SchemaDesigner.Types
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import GHC.IO (evaluate)


tests = do
    describe "The Schema.sql Parser" do
        it "should parse an empty CREATE TABLE statement" do
            parseSql "CREATE TABLE users ();"  `shouldBe` CreateTable { name = "users", columns = [] }

        it "should parse an CREATE EXTENSION for the UUID extension" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

        it "should parse a line comment" do
            parseSql "-- Comment value" `shouldBe` Comment { content = "Comment value" }

        it "should parse a CREATE TABLE with columns" do
            let sql = cs [plain|CREATE TABLE users (
                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    firstname TEXT NOT NULL,
                    lastname TEXT NOT NULL,
                    password_hash TEXT NOT NULL,
                    email TEXT NOT NULL,
                    company_id UUID NOT NULL,
                    picture_url TEXT,
                    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
                ); |]
            parseSql sql `shouldBe` CreateTable
                    { name = "users"
                    , columns = [
                        Column
                            { name = "id"
                            , columnType = "UUID"
                            , primaryKey = True
                            , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "firstname"
                            , columnType = "TEXT"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "lastname"
                            , columnType = "TEXT"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "password_hash"
                            , columnType = "TEXT"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "email"
                            , columnType = "TEXT"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "company_id"
                            , columnType = "UUID"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "picture_url"
                            , columnType = "TEXT"
                            , primaryKey = False
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            }
                        , Column
                            { name = "created_at"
                            , columnType = "TIMESTAMP WITH TIME ZONE"
                            , primaryKey = False
                            , defaultValue = Just (CallExpression "NOW" [])
                            , notNull = True
                            , isUnique = False
                            }
                        ]
                    }

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` CreateTable { name = "quoted name", columns = [] }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Cascade
                        }
                    }
       
        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET NULL" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET NULL;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just SetNull
                        }
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE RESTRICT" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE RESTRICT;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Restrict
                        }
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE NO ACTION" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE NO ACTION;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just NoAction
                        }
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. (without ON DELETE)" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id);" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint 
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Nothing
                        }
                    }

        it "should parse CREATE TYPE .. AS ENUM" do
            parseSql "CREATE TYPE colors AS ENUM ('yellow', 'red', 'green');" `shouldBe` CreateEnumType { name = "colors", values = ["yellow", "red", "green"] }
        
        -- When creating a new Enum Type, it is empty at first.
        -- Throwing an error for empty Enums renders the visual editor inaccessible.
        -- Catching empty Enums results in the "Create Enum" UI button being useless.
        -- Thats why empty Enums will not throw errors.
        it "should parse CREATE TYPE .. AS ENUM without values" do
            parseSql "CREATE TYPE colors AS ENUM ();" `shouldBe` CreateEnumType { name = "colors", values = [] }

parseSql :: Text -> Statement
parseSql sql = let [statement] = parseSqlStatements sql in statement

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements