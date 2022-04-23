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

        it "should parse an CREATE EXTENSION with schema suffix" do
            parseSql "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" WITH SCHEMA public;" `shouldBe` CreateExtension { name = "uuid-ossp", ifNotExists = True }

        it "should parse an CREATE EXTENSION without quotes" do
            parseSql "CREATE EXTENSION IF NOT EXISTS fuzzystrmatch WITH SCHEMA public;" `shouldBe` CreateExtension { name = "fuzzystrmatch", ifNotExists = True }

        it "should parse a line comment" do
            parseSql "-- Comment value" `shouldBe` Comment { content = " Comment value" }

        it "should parse an empty comment" do
            parseSqlStatements "--\n--" `shouldBe` [ Comment { content = "" }, Comment { content = "" } ]

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
                            , generator = Nothing
                            }
                        , Column
                            { name = "firstname"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "lastname"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "password_hash"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "email"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "company_id"
                            , columnType = PUUID
                            , defaultValue = Nothing
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "picture_url"
                            , columnType = PText
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "created_at"
                            , columnType = PTimestampWithTimezone
                            , defaultValue = Just (CallExpression "NOW" [])
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with a generated column" do
            let sql = cs [plain|
                CREATE TABLE products (
                        ts tsvector GENERATED ALWAYS AS (setweight(to_tsvector('english', sku), 'A') || setweight(to_tsvector('english', name), 'B') || setweight(to_tsvector('english', description), 'C')) STORED
                );
            |]
            parseSql sql `shouldBe` StatementCreateTable CreateTable
                    { name = "products"
                    , columns = [
                        Column
                            { name = "ts"
                            , columnType = PTSVector
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Just $ ColumnGenerator
                                        { generate =
                                            ConcatenationExpression
                                                (ConcatenationExpression
                                                    (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "sku"],TextExpression "A"])
                                                    (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "name"],TextExpression "B"])
                                                )
                                                (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "description"],TextExpression "C"])
                                        , stored = True
                                        }
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }

        it "should parse a CREATE TABLE with quoted identifiers" do
            parseSql "CREATE TABLE \"quoted name\" ();" `shouldBe` StatementCreateTable CreateTable { name = "quoted name", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }

        it "should parse a CREATE TABLE with public schema prefix" do
            parseSql "CREATE TABLE public.users ();" `shouldBe` StatementCreateTable CreateTable { name = "users", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Cascade
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET DEFAULT" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET DEFAULT;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just SetDefault
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET NULL" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET NULL;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just SetNull
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE RESTRICT" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE RESTRICT;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just Restrict
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE NO ACTION" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE NO ACTION;" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Just NoAction
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD FOREIGN KEY .. (without ON DELETE)" do
            parseSql "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id);" `shouldBe` AddConstraint
                    { tableName = "users"
                    , constraint = ForeignKeyConstraint
                        { name = "users_ref_company_id"
                        , columnName = "company_id"
                        , referenceTable = "companies"
                        , referenceColumn = "id"
                        , onDelete = Nothing
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse a complex ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            parseSql "ALTER TABLE properties ADD CONSTRAINT foobar CHECK ((property_type = 'haus_buy' AND area_garden IS NOT NULL AND rent_monthly IS NULL) OR (property_type = 'haus_rent' AND rent_monthly IS NOT NULL AND price IS NULL));" `shouldBe` AddConstraint
                    { tableName = "properties"
                    , constraint = CheckConstraint
                        { name = "foobar"
                        , checkExpression = OrExpression
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
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }


        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) < 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression =
                            LessThanExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (IntExpression 20)
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }



        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <=" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) <= 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression =
                            LessThanOrEqualToExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (IntExpression 20)
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) > 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression =
                            GreaterThanExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (IntExpression 20)
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }


        it "should parse ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >=" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) >= 20);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression =
                            GreaterThanOrEqualToExpression
                                (CallExpression ("length") [VarExpression "title"])
                                (IntExpression 20)
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =, author WITH =);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            , ExcludeConstraintElement { element = "author", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE USING btree .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING btree (title WITH =, author WITH =);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            , ExcludeConstraintElement { element = "author", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Just Btree
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE USING gin .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING gin (title WITH =, author WITH =);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            , ExcludeConstraintElement { element = "author", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Just Gin
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE USING gist .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING gist (title WITH =, author WITH =);" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            , ExcludeConstraintElement { element = "author", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Just Gist
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. WHERE .." do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =, author WITH =) WHERE (title = 'why');" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            , ExcludeConstraintElement { element = "author", operator = "=" }
                            ]
                        , predicate = Just $ EqExpression (VarExpression "title") (TextExpression "why")
                        , indexType = Nothing
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. WHERE .. with various operators" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (i1 WITH =, i2 WITH <>, i3 WITH !=, i4 WITH AND, i5 WITH OR) WHERE (title = 'why');" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "i1", operator = "=" }
                            , ExcludeConstraintElement { element = "i2", operator = "<>" }
                            , ExcludeConstraintElement { element = "i3", operator = "!=" }
                            , ExcludeConstraintElement { element = "i4", operator = "AND" }
                            , ExcludeConstraintElement { element = "i5", operator = "OR" }
                            ]
                        , predicate = Just $ EqExpression (VarExpression "title") (TextExpression "why")
                        , indexType = Nothing
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT deferrable_unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE;" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "deferrable_unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Nothing
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE INITIALLY IMMEDIATE" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT deferrable_unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE INITIALLY IMMEDIATE;" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "deferrable_unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Just InitiallyImmediate
                    }

        it "should parse ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE INITIALLY DEFERRED" do
            parseSql "ALTER TABLE posts ADD CONSTRAINT deferrable_unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE INITIALLY DEFERRED;" `shouldBe` AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "deferrable_unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Just InitiallyDeferred
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

        it "should parse ALTER TYPE .. ADD VALUE .." do
            parseSql "ALTER TYPE colors ADD VALUE 'blue';" `shouldBe` AddValueToEnumType { enumName = "colors", newValue = "blue", ifNotExists = False }

        it "should parse ALTER TYPE .. ADD VALUE IF NOT EXISTS .." do
            parseSql "ALTER TYPE colors ADD VALUE IF NOT EXISTS 'blue';" `shouldBe` AddValueToEnumType { enumName = "colors", newValue = "blue", ifNotExists = True }

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
                        , col { name = "d", columnType = (PVaryingN (Just 10)) }
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
                    , constraints = [ UniqueConstraint { name = Nothing, columnNames = [ "user_id", "follower_id" ] } ]
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

        it "should parse a CREATE TABLE statement with a polygon column" do
            parseSql "CREATE TABLE polygons (\n    poly POLYGON\n);\n" `shouldBe` StatementCreateTable CreateTable
                    { name = "polygons"
                    , columns = [ col { name = "poly", columnType = PPolygon } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
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

        it "should parse a 'CREATE INDEX .. ON .. USING GIN' statement" do
            parseSql "CREATE INDEX users_index ON users USING GIN (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Gin
                    }

        it "should parse a 'CREATE INDEX .. ON .. USING btree' statement" do
            parseSql "CREATE INDEX users_index ON users USING btree (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Btree
                    }

        it "should parse a 'CREATE INDEX .. ON .. USING GIST' statement" do
            parseSql "CREATE INDEX users_index ON users USING GIST (user_name);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Gist
                    }

        it "should parse a CREATE INDEX statement with multiple columns" do
            parseSql "CREATE INDEX users_index ON users (user_name, project_id);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns =
                        [ IndexColumn { column = VarExpression "user_name", columnOrder = [] }
                        , IndexColumn { column = VarExpression "project_id", columnOrder = [] }
                        ]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
        it "should parse a CREATE INDEX statement with a LOWER call" do
            parseSql "CREATE INDEX users_email_index ON users (LOWER(email));\n" `shouldBe` CreateIndex
                    { indexName = "users_email_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = CallExpression "LOWER" [VarExpression "email"], columnOrder = [] }]
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

        it "should parse a CREATE INDEX with column order ASC NULLS FIRST statement" do
            parseSql "CREATE UNIQUE INDEX users_index ON users (user_name ASC NULLS FIRST);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [Asc, NullsFirst] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }

        it "should parse a CREATE INDEX with column order DESC NULLS LAST statement" do
            parseSql "CREATE UNIQUE INDEX users_index ON users (user_name DESC NULLS LAST);\n" `shouldBe` CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [Desc, NullsLast] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }

        it "should parse a CREATE OR REPLACE FUNCTION ..() RETURNS TRIGGER .." do
            parseSql "CREATE OR REPLACE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;" `shouldBe` CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = []
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

        it "should parse a CREATE FUNCTION ..() RETURNS TRIGGER .." do
            parseSql "CREATE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;" `shouldBe` CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = []
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = False
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

        it "should parse a CREATE FUNCTION with parameters ..() RETURNS TRIGGER .." do
            parseSql "CREATE FUNCTION notify_did_insert_webrtc_connection(param1 INT, param2 TEXT) RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;" `shouldBe` CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = [("param1", PInt), ("param2", PText)]
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = False
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

        it "should parse CREATE FUNCTION statements that are outputted by pg_dump" do
            let sql = cs [plain|
CREATE FUNCTION public.notify_did_change_projects() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
    PERFORM pg_notify('did_change_projects', '');
    RETURN new;END;
$$;
            |]
            parseSql sql `shouldBe` CreateFunction
                    { functionName = "notify_did_change_projects"
                    , functionArguments = []
                    , functionBody = "BEGIN\n    PERFORM pg_notify('did_change_projects', '');\n    RETURN new;END;\n"
                    , orReplace = False
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

        it "should parse a decimal default value with a type-cast" do
            let sql = "CREATE TABLE a(electricity_unit_price DOUBLE PRECISION DEFAULT 0.17::double precision NOT NULL);"
            let statements =
                    [ StatementCreateTable CreateTable { name = "a", columns = [Column {name = "electricity_unit_price", columnType = PDouble, defaultValue = Just (TypeCastExpression (DoubleExpression 0.17) PDouble), notNull = True, isUnique = False, generator = Nothing}], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
                    ]
            parseSqlStatements sql `shouldBe` statements

        it "should parse a integer default value" do
            let sql = "CREATE TABLE a(electricity_unit_price INT DEFAULT 0 NOT NULL);"
            let statements =
                    [ StatementCreateTable CreateTable { name = "a", columns = [Column {name = "electricity_unit_price", columnType = PInt, defaultValue = Just (IntExpression 0), notNull = True, isUnique = False, generator = Nothing}], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
                    ]
            parseSqlStatements sql `shouldBe` statements

        it "should parse a partial index" do
            parseSql "CREATE UNIQUE INDEX unique_source_id ON listings (source, source_id) WHERE source IS NOT NULL AND source_id IS NOT NULL;" `shouldBe` CreateIndex
                    { indexName = "unique_source_id"
                    , unique = True
                    , tableName = "listings"
                    , columns =
                        [ IndexColumn { column = VarExpression "source", columnOrder = [] }
                        , IndexColumn { column = VarExpression "source_id", columnOrder = [] }
                        ]
                    , whereClause = Just (
                        AndExpression
                            (IsExpression (VarExpression "source") (NotExpression (VarExpression "NULL")))
                            (IsExpression (VarExpression "source_id") (NotExpression (VarExpression "NULL"))))
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
        it "should parse 'ALTER TABLE .. ADD COLUMN' statements" do
            parseSql "ALTER TABLE a ADD COLUMN b INT NOT NULL;" `shouldBe` AddColumn { tableName = "a", column = Column { name ="b", columnType = PInt, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing}}

        it "should parse 'ALTER TABLE .. DROP COLUMN ..' statements" do
            parseSql "ALTER TABLE tasks DROP COLUMN description;" `shouldBe` DropColumn { tableName = "tasks", columnName = "description" }

        it "should parse 'ALTER TABLE .. RENAME COLUMN .. TO ..' statements" do
            parseSql "ALTER TABLE users RENAME COLUMN name TO full_name;" `shouldBe` RenameColumn { tableName = "users", from = "name", to = "full_name" }

        it "should parse 'DROP TABLE ..' statements" do
            parseSql "DROP TABLE tasks;" `shouldBe` DropTable { tableName = "tasks" }

        it "should parse 'DROP TYPE ..' statements" do
            parseSql "DROP TYPE colors;" `shouldBe` DropEnumType { name = "colors" }

        it "should parse 'ALTER TABLE .. DROP CONSTRAINT ..' statements" do
            parseSql "ALTER TABLE tasks DROP CONSTRAINT tasks_title_key;" `shouldBe` DropConstraint { tableName = "tasks", constraintName = "tasks_title_key" }

        it "should parse 'CREATE SEQUENCE ..' statements" do
            parseSql "CREATE SEQUENCE a;" `shouldBe` CreateSequence { name = "a" }

        it "should parse 'CREATE SEQUENCE ..' statements with qualified name" do
            parseSql "CREATE SEQUENCE public.a;" `shouldBe` CreateSequence { name = "a" }

        it "should parse 'CREATE SEQUENCE .. AS .. START WITH .. INCREMENT BY .. NO MINVALUE NO MAXVALUE CACHE ..;'" do
            let sql = [trimming|
                CREATE SEQUENCE public.a
                    AS integer
                    START WITH 1
                    INCREMENT BY 1
                    NO MINVALUE
                    NO MAXVALUE
                    CACHE 1;
            |]
            parseSql sql `shouldBe` CreateSequence { name = "a" }

        it "should parse 'SET' statements" do
            parseSql "SET statement_timeout = 0;" `shouldBe` Set { name = "statement_timeout", value = IntExpression 0 }
            parseSql "SET client_encoding = 'UTF8';" `shouldBe` Set { name = "client_encoding", value = TextExpression "UTF8" }

        it "should parse 'SELECT' statements" do
            parseSql "SELECT pg_catalog.set_config('search_path', '', false);" `shouldBe` SelectStatement { query = "pg_catalog.set_config('search_path', '', false)" }
        it "should parse 'COMMENT' statements" do
            parseSql "COMMENT ON EXTENSION \"uuid-ossp\" IS 'generate universally unique identifiers (UUIDs)';" `shouldBe` Comment { content = "ON EXTENSION \"uuid-ossp\" IS 'generate universally unique identifiers (UUIDs)'" }

        it "should parse a column with a default value that has a qualified function call" do
            let sql = cs [plain|
                CREATE TABLE a(id UUID DEFAULT public.uuid_generate_v4() NOT NULL);
            |]
            let statement = StatementCreateTable CreateTable { name = "a", columns = [Column {name = "id", columnType = PUUID, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True, isUnique = False, generator = Nothing}], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
            parseSql sql `shouldBe` statement


        it "should parse character varying type casts" do
            let sql = cs [plain|
                CREATE TABLE a (
                    a character varying(510) DEFAULT NULL::character varying
                );
            |]
            let statement = StatementCreateTable CreateTable
                    { name = "a"
                    , columns = [ Column
                            {name = "a"
                            , columnType = PVaryingN (Just 510)
                            , defaultValue = Just (TypeCastExpression (VarExpression "NULL") (PVaryingN Nothing))
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            parseSql sql `shouldBe` statement

        it "should parse empty binary strings" do
            let sql = cs [plain|
                CREATE TABLE a (
                    a bytea DEFAULT '\\x'::bytea NOT NULL
                );
            |]
            let statement = StatementCreateTable CreateTable
                    { name = "a"
                    , columns = [ Column
                            {name = "a"
                            , columnType = PBinary
                            , defaultValue = Just (TypeCastExpression (TextExpression "") PBinary)
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            parseSql sql `shouldBe` statement
        it "should parse a pg_dump header" do
            let sql = cs [plain|
--
-- PostgreSQL database dump
--

-- Dumped from database version 13.3
-- Dumped by pg_dump version 13.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


            |]

            let statements =
                    [ Comment {content = ""}
                    , Comment {content = " PostgreSQL database dump"}
                    , Comment {content = ""}
                    , Comment {content = " Dumped from database version 13.3"}
                    , Comment {content = " Dumped by pg_dump version 13.3"}
                    , Set {name = "statement_timeout", value = IntExpression 0}
                    , Set {name = "lock_timeout", value = IntExpression 0}
                    , Set {name = "idle_in_transaction_session_timeout", value = IntExpression 0}
                    , Set {name = "client_encoding", value = TextExpression "UTF8"}
                    , Set {name = "standard_conforming_strings", value = VarExpression "on"}
                    , SelectStatement {query = "pg_catalog.set_config('search_path', '', false)"}
                    , Set {name = "check_function_bodies", value = VarExpression "false"}
                    , Set {name = "xmloption", value = VarExpression "content"}
                    , Set {name = "client_min_messages", value = VarExpression "warning"}
                    , Set {name = "row_security", value = VarExpression "off"}
                    , Comment {content = ""}
                    , Comment {content = " Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -"}
                    , Comment {content = ""}
                    , CreateExtension {name = "uuid-ossp", ifNotExists = True}
                    , Comment {content = ""}
                    , Comment {content = " Name: EXTENSION \"uuid-ossp\"; Type: COMMENT; Schema: -; Owner: -"}
                    , Comment {content = ""}
                    , Comment {content = "ON EXTENSION \"uuid-ossp\" IS 'generate universally unique identifiers (UUIDs)'"}
                    ]
            parseSqlStatements sql `shouldBe` statements

        it "should parse 'DROP INDEX ..' statements" do
            parseSql "DROP INDEX a;" `shouldBe` DropIndex { indexName = "a" }

        it "should parse 'ALTER TABLE .. ALTER COLUMN .. DROP NOT NULL' statements" do
            parseSql "ALTER TABLE a ALTER COLUMN b DROP NOT NULL;" `shouldBe` DropNotNull { tableName = "a", columnName = "b" }

        it "should parse 'ALTER TABLE .. ALTER COLUMN .. SET NOT NULL' statements" do
            parseSql "ALTER TABLE a ALTER COLUMN b SET NOT NULL;" `shouldBe` SetNotNull { tableName = "a", columnName = "b" }

        it "should parse 'ALTER TABLE .. ALTER COLUMN .. SET DEFAULT ..' statements" do
            parseSql "ALTER TABLE a ALTER COLUMN b SET DEFAULT null;" `shouldBe` SetDefaultValue { tableName = "a", columnName = "b", value = VarExpression "null" }

        it "should parse 'ALTER TABLE .. ALTER COLUMN .. DROP DEFAULT' statements" do
            parseSql "ALTER TABLE a ALTER COLUMN b DROP DEFAULT;" `shouldBe` DropDefaultValue { tableName = "a", columnName = "b" }

        it "should parse 'ALTER TABLE .. RENAME TO ..' statements" do
            parseSql "ALTER TABLE profiles RENAME TO users;" `shouldBe` RenameTable { from = "profiles", to = "users" }

        it "should parse 'DROP POLICY .. ON ..' statements" do
            parseSql "DROP POLICY \"Users can manage their todos\" ON todos;" `shouldBe` DropPolicy { tableName = "todos", policyName = "Users can manage their todos" }

        it "should parse 'CREATE POLICY .. FOR SELECT' statements" do
            let sql = cs [plain|CREATE POLICY "Messages are public" ON messages FOR SELECT USING (true);|]
            parseSql sql `shouldBe` CreatePolicy
                    { name = "Messages are public"
                    , action = Just PolicyForSelect
                    , tableName = "messages"
                    , using = Just (VarExpression "true")
                    , check = Nothing
                    }

        it "should parse policies with an EXISTS condition" do
            let sql = cs [plain|CREATE POLICY "Users can manage their project's migrations" ON migrations USING (EXISTS (SELECT 1 FROM projects WHERE id = project_id)) WITH CHECK (EXISTS (SELECT 1 FROM projects WHERE id = project_id));|]
            parseSql sql `shouldBe` CreatePolicy
                    { name = "Users can manage their project's migrations"
                    , action = Nothing
                    , tableName = "migrations"
                    , using = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = VarExpression "projects", alias = Nothing, whereClause = EqExpression (VarExpression "id") (VarExpression "project_id")})))
                    , check = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = VarExpression "projects", alias = Nothing, whereClause = EqExpression (VarExpression "id") (VarExpression "project_id")})))
                    }

        it "should parse policies with an EXISTS condition and a qualified table name" do
            let sql = cs [plain|CREATE POLICY "Users can manage their project's migrations" ON migrations USING (EXISTS (SELECT 1 FROM public.projects WHERE projects.id = migrations.project_id)) WITH CHECK (EXISTS (SELECT 1 FROM public.projects WHERE projects.id = migrations.project_id));|]
            parseSql sql `shouldBe` CreatePolicy
                    { name = "Users can manage their project's migrations"
                    , action = Nothing
                    , tableName = "migrations"
                    , using = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "projects", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "projects") "id") (DotExpression (VarExpression "migrations") "project_id")})))
                    , check = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "projects", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "projects") "id") (DotExpression (VarExpression "migrations") "project_id")})))
                    }

        it "should parse a call expression with multiple arguments" do
            let sql = cs [plain|ALTER TABLE a ADD CONSTRAINT source CHECK (num_nonnulls(a, b, c) = 1);|]
            parseSql sql `shouldBe`  AddConstraint
                { tableName = "a"
                , constraint = CheckConstraint
                    { name = Just "source"
                    , checkExpression = EqExpression (CallExpression "num_nonnulls" [VarExpression "a",VarExpression "b",VarExpression "c"]) (IntExpression 1)
                    }
                , deferrable = Nothing
                , deferrableType = Nothing
                }

        it "should parse 'CREATE TRIGGER .. AFTER INSERT ON .. FOR EACH ROW EXECUTE ..;' statements" do
            parseSql "CREATE TRIGGER call_test_function_for_new_users AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION call_test_function('hello');" `shouldBe` CreateTrigger
                    { name = "call_test_function_for_new_users"
                    , eventWhen = After
                    , event = TriggerOnInsert
                    , tableName = "users"
                    , for = ForEachRow
                    , whenCondition = Nothing
                    , functionName = "call_test_function"
                    , arguments = [TextExpression "hello"]
                    }

        it "should parse 'ALTER SEQUENCE ..' statements" do
            let sql = cs [plain|ALTER SEQUENCE public.a OWNED BY public.b.serial_number;|]
            parseSql sql `shouldBe` UnknownStatement { raw = "ALTER SEQUENCE public.a OWNED BY public.b.serial_number" }

        it "should parse positive IntExpression's" do
            parseExpression "1" `shouldBe` (IntExpression 1)

        it "should parse negative IntExpression's" do
            parseExpression "-1" `shouldBe` (IntExpression (-1))

        it "should parse positive DoubleExpression's" do
            parseExpression "1.337" `shouldBe` (DoubleExpression 1.337)

        it "should parse negative DoubleExpression's" do
            parseExpression "-1.337" `shouldBe` (DoubleExpression (-1.337))

        it "should parse lower-cased SELECT expressions" do
            parseExpression "(select company_id from users where id = ihp_user_id())" `shouldBe` SelectExpression (Select {columns = [VarExpression "company_id"], from = VarExpression "users", alias = Nothing, whereClause = EqExpression (VarExpression "id") (CallExpression "ihp_user_id" [])})

        it "should parse policies with an alias in the USING expression" do
            let sql = cs [plain|
                CREATE POLICY "Users can see other users in their company" ON public.users USING ((company_id = ( SELECT users_1.company_id
                   FROM public.users users_1
                  WHERE (users_1.id = public.ihp_user_id()))));
            |]
            parseSql sql `shouldBe` CreatePolicy
                    { name = "Users can see other users in their company"
                    , action = Nothing
                    , tableName = "users"
                    , using = Just (EqExpression (VarExpression "company_id") (SelectExpression (Select {columns = [DotExpression (VarExpression "users_1") "company_id"], from = DotExpression (VarExpression "public") "users", alias = Just "users_1", whereClause = EqExpression (DotExpression (VarExpression "users_1") "id") (CallExpression "ihp_user_id" [])})))
                    , check = Nothing
                    }

        it "should parse 'BEGIN' statements" do
            let sql = cs [plain|BEGIN;|]
            parseSql sql `shouldBe` Begin

        it "should parse 'COMMIT' statements" do
            let sql = cs [plain|COMMIT;|]
            parseSql sql `shouldBe` Commit
        
        it "should parse 'DROP FUNCTION ..' statements" do
            let sql = cs [plain|DROP FUNCTION my_function;|]
            parseSql sql `shouldBe` DropFunction { functionName = "my_function" }

col :: Column
col = Column
    { name = ""
    , columnType = PCustomType ""
    , defaultValue = Nothing
    , notNull = False
    , isUnique = False
    , generator = Nothing
    }

parseSql :: Text -> Statement
parseSql sql = let [statement] = parseSqlStatements sql in statement

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements

parseExpression :: Text -> Expression
parseExpression sql =
    case Megaparsec.runParser Parser.expression "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right expression -> expression
