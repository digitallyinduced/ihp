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
            parseSql "CREATE TABLE users ();"  `shouldBe` CreateTable { name = "users", columns = [], constraints = [] }

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

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` CreateTable { name = "quoted name", columns = [], constraints = [] }

        it "should parse a CREATE TABLE with public schema prefix" do
            parseSql "CREATE TABLE public.users ();" `shouldBe` CreateTable { name = "users", columns = [], constraints = [] }

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

        it "should parse a CREATE TABLE with INTEGER / INT / INT4 / BIGINT / INT 8 columns" do
            parseSql "CREATE TABLE ints (int_a INTEGER, int_b INT, int_c int4, bigint_a BIGINT, bigint_b int8);" `shouldBe` CreateTable
                    { name = "ints"
                    , columns =
                        [ col { name = "int_a", columnType = PInt }
                        , col { name = "int_b", columnType = PInt }
                        , col { name = "int_c", columnType = PInt }
                        , col { name = "bigint_a", columnType = PBigInt }
                        , col { name = "bigint_b", columnType = PBigInt }
                        ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with TIMESTAMP WITH TIMEZONE / TIMESTAMPZ columns" do
            parseSql "CREATE TABLE timestamps (a TIMESTAMP WITH TIME ZONE, b TIMESTAMPZ);" `shouldBe` CreateTable
                    { name = "timestamps"
                    , columns =
                        [ col { name = "a", columnType = PTimestampWithTimezone }
                        , col { name = "b", columnType = PTimestampWithTimezone }
                        ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with BOOLEAN / BOOL columns" do
            parseSql "CREATE TABLE bools (a BOOLEAN, b BOOL);" `shouldBe` CreateTable
                    { name = "bools"
                    , columns =
                        [ col { name = "a", columnType = PBoolean }
                        , col { name = "b", columnType = PBoolean }
                        ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with REAL, FLOAT4, DOUBLE, FLOAT8 columns" do
            parseSql "CREATE TABLE bools (a REAL, b FLOAT4, c DOUBLE PRECISION, d FLOAT8);" `shouldBe` CreateTable
                    { name = "bools"
                    , columns =
                        [ col { name = "a", columnType = PReal }
                        , col { name = "b", columnType = PReal }
                        , col { name = "c", columnType = PDouble }
                        , col { name = "d", columnType = PDouble }
                        ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with (deprecated) NUMERIC, NUMERIC(x), NUMERIC (x,y), VARYING(n) columns" do
            parseSql ("CREATE TABLE deprecated_variables (a NUMERIC, b NUMERIC(1), c NUMERIC(1,2), d CHARACTER VARYING(10));") `shouldBe` CreateTable
                    { name = "deprecated_variables"
                    , columns = 
                        [ col { name = "a", columnType = PNumeric Nothing Nothing}
                        , col { name = "b", columnType = (PNumeric (Just 1) Nothing) }
                        , col { name = "c", columnType = (PNumeric (Just 1) (Just 2)) }
                        , col { name = "d", columnType = (PVaryingN 10) }
                        ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with a multi-column UNIQUE (a, b) constraint" do
            parseSql "CREATE TABLE user_followers (id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL, user_id UUID NOT NULL, follower_id UUID NOT NULL, UNIQUE(user_id, follower_id));"  `shouldBe` CreateTable
                    { name = "user_followers"
                    , columns =
                        [ col { name = "id", columnType = PUUID, primaryKey = True, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , col { name = "user_id", columnType = PUUID, notNull = True }
                        , col { name = "follower_id", columnType = PUUID, notNull = True }
                        ]
                    , constraints = [ UniqueConstraint { columnNames = [ "user_id", "follower_id" ] } ]
                    }

        it "should fail to parse a CREATE TABLE statement with an empty UNIQUE () constraint" do
            (evaluate (parseSql "CREATE TABLE user_followers (id UUID, UNIQUE());")) `shouldThrow` anyException
            pure ()

        it "should parse a CREATE TABLE statement with a serial id" do
            parseSql "CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n" `shouldBe` CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PSerial, notNull = True, primaryKey = True} ]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with a bigserial id" do
            parseSql "CREATE TABLE orders (\n    id BIGSERIAL PRIMARY KEY NOT NULL\n);\n" `shouldBe` CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PBigserial, notNull = True, primaryKey = True} ]
                    , constraints = []
                    }

col :: Column
col = Column
    { name = ""
    , columnType = PCustomType ""
    , primaryKey = False
    , defaultValue = Nothing
    , notNull = False
    , isUnique = False
    }

parseSql :: Text -> Statement
parseSql sql = let [statement] = parseSqlStatements sql in statement

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements