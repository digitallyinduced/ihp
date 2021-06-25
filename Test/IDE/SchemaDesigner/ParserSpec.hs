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
            parseSql "CREATE TABLE users ();"  `shouldBe` StatementCreateTable CreateTable { name = "users", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }

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
            parseSql sql `shouldBe` StatementCreateTable CreateTable
                    { name = "users"
                    , columns = [
                        Column
                            { name = "id"
                            , columnType = PUUID
                            , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "firstname"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "lastname"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "password_hash"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "email"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "company_id"
                            , columnType = PUUID
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            }
                        , Column
                            { name = "picture_url"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            }
                        , Column
                            { name = "created_at"
                            , columnType = PTimestampWithTimezone
                            , defaultValue = Just (CallExpression "NOW" [])
                            , notNull = True
                            , isUnique = False
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` StatementCreateTable CreateTable { name = "quoted name", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }

        it "should parse a CREATE TABLE with public schema prefix" do
            parseSql "CREATE TABLE public.users ();" `shouldBe` StatementCreateTable CreateTable { name = "users", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }

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

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET DEFAULT" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET DEFAULT;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraintName = "users_ref_company_id"
                    , constraint = ForeignKeyConstraint
                        { columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just SetDefault
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

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraintName = "check_title_length"
                    , constraint = CheckConstraint
                        { checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    }

        it "should parse a complex ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            parseSql "ALTER TABLE properties ADD CONSTRAINT foobar CHECK ((property_type = 'haus_buy' AND area_garden IS NOT NULL AND rent_monthly IS NULL) OR (property_type = 'haus_rent' AND rent_monthly IS NOT NULL AND price IS NULL));" `shouldBe` AddConstraint
                    { tableName = "properties"
                    , constraintName = "foobar"
                    , constraint = CheckConstraint
                        { checkExpression = OrExpression
                                (AndExpression
                                    (AndExpression
                                        (EqExpression (VarExpression "property_type") (TextExpression "haus_buy"))
                                        (IsExpression (VarExpression "area_garden") (NotExpression (VarExpression "NULL")))
                                    )
                                    (IsExpression (VarExpression "rent_monthly") (VarExpression "NULL"))
                                )

                                (AndExpression
                                    (AndExpression
                                        (EqExpression (VarExpression "property_type") (TextExpression "haus_rent"))
                                        (IsExpression (VarExpression "rent_monthly") (NotExpression (VarExpression "NULL")))
                                    )
                                    (IsExpression (VarExpression "price") (VarExpression "NULL"))
                                )
                        }
                    }


        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) < 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraintName = "check_title_length"
                    , constraint = CheckConstraint
                        { checkExpression = 
                            LessThanExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (VarExpression "20")
                        }
                    }



        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <=" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) <= 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraintName = "check_title_length"
                    , constraint = CheckConstraint
                        { checkExpression = 
                            LessThanOrEqualToExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (VarExpression "20")
                        }
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) > 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraintName = "check_title_length"
                    , constraint = CheckConstraint
                        { checkExpression = 
                            GreaterThanExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (VarExpression "20")
                        }
                    }


        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >=" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) >= 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraintName = "check_title_length"
                    , constraint = CheckConstraint
                        { checkExpression = 
                            GreaterThanOrEqualToExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (VarExpression "20")
                        }
                    }


        it "should parse CREATE TYPE .. AS ENUM" do
            parseSql "CREATE TYPE colors AS ENUM ('yellow', 'red', 'green');" `shouldBe` CreateEnumType { name = "colors", values = ["yellow", "red", "green"] }

        it "should parse CREATE TYPE .. AS ENUM with extra whitespace" do
            parseSql "CREATE TYPE Numbers AS ENUM (\n\t'One',\t'Two',\t'Three',\t'Four',\t'Five',\t'Six',\t'Seven',\t'Eight',\t'Nine',\t'Ten'\n);" `shouldBe` CreateEnumType { name = "Numbers", values = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"] }

        -- When creating a new Enum Type, it is empty at first.
        -- Throwing an error for empty Enums renders the visual editor inaccessible.
        -- Catching empty Enums results in the "Create Enum" UI button being useless.
        -- Thats why empty Enums will not throw errors.
        it "should parse CREATE TYPE .. AS ENUM without values" do
            parseSql "CREATE TYPE colors AS ENUM ();" `shouldBe` CreateEnumType { name = "colors", values = [] }

        it "should parse a CREATE TABLE with INTEGER / INT / INT4 / SMALLINT / INT2 / BIGINT / INT8 columns" do
            parseSql "CREATE TABLE ints (int_a INTEGER, int_b INT, int_c int4, smallint_a SMALLINT, smallint_b INT2, bigint_a BIGINT, bigint_b int8);" `shouldBe` StatementCreateTable CreateTable
                    { name = "ints"
                    , columns =
                        [ col { name = "int_a", columnType = PInt }
                        , col { name = "int_b", columnType = PInt }
                        , col { name = "int_c", columnType = PInt }
                        , col { name = "smallint_a", columnType = PSmallInt }
                        , col { name = "smallint_b", columnType = PSmallInt }
                        , col { name = "bigint_a", columnType = PBigInt }
                        , col { name = "bigint_b", columnType = PBigInt }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with TIMESTAMP WITH TIMEZONE / TIMESTAMPZ columns" do
            parseSql "CREATE TABLE timestamps (a TIMESTAMP WITH TIME ZONE, b TIMESTAMPZ);" `shouldBe` StatementCreateTable CreateTable
                    { name = "timestamps"
                    , columns =
                        [ col { name = "a", columnType = PTimestampWithTimezone }
                        , col { name = "b", columnType = PTimestampWithTimezone }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with BOOLEAN / BOOL columns" do
            parseSql "CREATE TABLE bools (a BOOLEAN, b BOOL);" `shouldBe` StatementCreateTable CreateTable
                    { name = "bools"
                    , columns =
                        [ col { name = "a", columnType = PBoolean }
                        , col { name = "b", columnType = PBoolean }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with REAL, FLOAT4, DOUBLE, FLOAT8 columns" do
            parseSql "CREATE TABLE bools (a REAL, b FLOAT4, c DOUBLE PRECISION, d FLOAT8);" `shouldBe` StatementCreateTable CreateTable
                    { name = "bools"
                    , columns =
                        [ col { name = "a", columnType = PReal }
                        , col { name = "b", columnType = PReal }
                        , col { name = "c", columnType = PDouble }
                        , col { name = "d", columnType = PDouble }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with (deprecated) NUMERIC, NUMERIC(x), NUMERIC (x,y), VARYING(n) columns" do
            parseSql ("CREATE TABLE deprecated_variables (a NUMERIC, b NUMERIC(1), c NUMERIC(1,2), d CHARACTER VARYING(10));") `shouldBe` StatementCreateTable CreateTable
                    { name = "deprecated_variables"
                    , columns =
                        [ col { name = "a", columnType = PNumeric Nothing Nothing}
                        , col { name = "b", columnType = (PNumeric (Just 1) Nothing) }
                        , col { name = "c", columnType = (PNumeric (Just 1) (Just 2)) }
                        , col { name = "d", columnType = (PVaryingN 10) }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with a multi-column UNIQUE (a, b) constraint" do
            parseSql "CREATE TABLE user_followers (id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL, user_id UUID NOT NULL, follower_id UUID NOT NULL, UNIQUE(user_id, follower_id));"  `shouldBe` StatementCreateTable CreateTable
                    { name = "user_followers"
                    , columns =
                        [ col { name = "id", columnType = PUUID, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , col { name = "user_id", columnType = PUUID, notNull = True }
                        , col { name = "follower_id", columnType = PUUID, notNull = True }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = [ UniqueConstraint { columnNames = [ "user_id", "follower_id" ] } ]
                    }

        it "should fail to parse a CREATE TABLE statement with an empty UNIQUE () constraint" do
            (evaluate (parseSql "CREATE TABLE user_followers (id UUID, UNIQUE());")) `shouldThrow` anyException
            pure ()

        it "should parse a CREATE TABLE statement with a multi-column PRIMARY KEY (a, b) constraint" do
            parseSql "CREATE TABLE user_followers (user_id UUID NOT NULL, follower_id UUID NOT NULL, PRIMARY KEY (user_id, follower_id));"  `shouldBe` StatementCreateTable CreateTable
                    { name = "user_followers"
                    , columns =
                        [ col { name = "user_id", columnType = PUUID, notNull = True }
                        , col { name = "follower_id", columnType = PUUID, notNull = True }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint [ "user_id", "follower_id" ]
                    , constraints = []
                    }

        it "should fail to parse a CREATE TABLE statement with PRIMARY KEY column and table constraints" do
            (evaluate (parseSql "CREATE TABLE user_followers (id UUID PRIMARY KEY, PRIMARY KEY(id));")) `shouldThrow` anyException
            pure ()

        it "should fail to parse a CREATE TABLE statement with an empty PRIMARY KEY () constraint" do
            (evaluate (parseSql "CREATE TABLE user_followers (id UUID, PRIMARY KEY ());")) `shouldThrow` anyException
            pure ()

        it "should parse a CREATE TABLE statement with a serial id" do
            parseSql "CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n" `shouldBe` StatementCreateTable CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PSerial, notNull = True} ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with a bigserial id" do
            parseSql "CREATE TABLE orders (\n    id BIGSERIAL PRIMARY KEY NOT NULL\n);\n" `shouldBe` StatementCreateTable CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PBigserial, notNull = True} ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with an array column" do
            parseSql "CREATE TABLE array_tests (\n    pay_by_quarter integer[]\n);\n" `shouldBe` StatementCreateTable CreateTable
                    { name = "array_tests"
                    , columns = [ col { name = "pay_by_quarter", columnType = PArray PInt } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE statement with a point column" do
            parseSql "CREATE TABLE points (\n    pos POINT\n);\n" `shouldBe` StatementCreateTable CreateTable
                    { name = "points"
                    , columns = [ col { name = "pos", columnType = PPoint } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE INDEX statement" do
            parseSql "CREATE INDEX users_index ON users (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , tableName = "users"
                    , expressions = [VarExpression "user_name"]
                    }
        it "should parse a CREATE INDEX statement with multiple columns" do
            parseSql "CREATE INDEX users_index ON users (user_name, project_id);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , tableName = "users"
                    , expressions = [VarExpression "user_name", VarExpression "project_id"]
                    }
        it "should parse a CREATE INDEX statement with a LOWER call" do
            parseSql "CREATE INDEX users_email_index ON users (LOWER(email));\n" `shouldBe` CreateIndex
                    { indexName = "users_email_index"
                    , tableName = "users"
                    , expressions = [CallExpression "LOWER" [VarExpression "email"]]
                    }

        it "should parse a CREATE OR REPLACE FUNCTION ..() RETURNS TRIGGER .." do
            parseSql "CREATE OR REPLACE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;" `shouldBe` CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = True
                    }

        it "should parse a CREATE FUNCTION ..() RETURNS TRIGGER .." do
            parseSql "CREATE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;" `shouldBe` CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = False
                    }

        it "should parse unsupported SQL as a unknown statement" do
            let sql = "CREATE TABLE a(); CREATE TRIGGER t AFTER INSERT ON x FOR EACH ROW EXECUTE PROCEDURE y(); CREATE TABLE b();"
            let statements =
                    [ StatementCreateTable CreateTable { name = "a", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
                    , UnknownStatement { raw = "CREATE TRIGGER t AFTER INSERT ON x FOR EACH ROW EXECUTE PROCEDURE y()"  }
                    , StatementCreateTable CreateTable { name = "b", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
                    ]
            parseSqlStatements sql `shouldBe` statements
            

col :: Column
col = Column
    { name = ""
    , columnType = PCustomType ""
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
