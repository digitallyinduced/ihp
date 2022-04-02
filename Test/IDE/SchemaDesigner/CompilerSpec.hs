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
import Test.IDE.SchemaDesigner.ParserSpec (col, parseSql)

tests = do
    describe "The Schema.sql Compiler" do
        it "should compile an empty CREATE TABLE statement" do
            compileSql [StatementCreateTable CreateTable { name = "users", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }] `shouldBe` "CREATE TABLE users (\n\n);\n"

        it "should compile a CREATE EXTENSION for the UUID extension" do
            compileSql [CreateExtension { name = "uuid-ossp", ifNotExists = True }] `shouldBe` "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n"

        it "should compile a line comment" do
            compileSql [Comment { content = " Comment value" }] `shouldBe` "-- Comment value\n"

        it "should compile a empty line comments" do
            compileSql [Comment { content = "" }, Comment { content = "" }] `shouldBe` "--\n--\n"

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
            let statement = StatementCreateTable CreateTable
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
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE with quoted identifiers" do
            compileSql [StatementCreateTable CreateTable { name = "quoted name", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }] `shouldBe` "CREATE TABLE \"quoted name\" (\n\n);\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE CASCADE" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE CASCADE;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET DEFAULT" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET DEFAULT;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE SET NULL" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE SET NULL;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE RESTRICT" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE RESTRICT;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. ON DELETE NO ACTION" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ON DELETE NO ACTION;\n"

        it "should compile ALTER TABLE .. ADD FOREIGN KEY .. (without ON DELETE)" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE users ADD CONSTRAINT users_ref_company_id FOREIGN KEY (company_id) REFERENCES companies (id) ;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = NotEqExpression (VarExpression "title") (TextExpression "")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (title <> '');\n"

        it "should compile a complex ALTER TABLE .. ADD CONSTRAINT .. CHECK .." do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE properties ADD CONSTRAINT foobar CHECK ((property_type = 'haus_buy' AND area_garden IS NOT NULL AND rent_monthly IS NULL) OR (property_type = 'haus_rent' AND rent_monthly IS NOT NULL AND price IS NULL));\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = LessThanExpression (CallExpression ("length") [VarExpression "title"]) (VarExpression "20")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) < 20);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a <=" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = LessThanOrEqualToExpression (CallExpression ("length") [VarExpression "title"]) (VarExpression "20")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) <= 20);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = GreaterThanExpression (CallExpression ("length") [VarExpression "title"]) (VarExpression "20")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) > 20);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. CHECK .. with a >=" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = CheckConstraint
                        { name = "check_title_length"
                        , checkExpression = GreaterThanOrEqualToExpression (CallExpression ("length") [VarExpression "title"]) (VarExpression "20")
                        }
                    , deferrable = Nothing
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT check_title_length CHECK (length(title) >= 20);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .." do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =, author WITH =);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. USING BTREE" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING BTREE (title WITH =, author WITH =);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. USING GIST" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING GIST (title WITH =, author WITH =);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. USING GIN" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE USING GIN (title WITH =, author WITH =);\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. WHERE .." do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =, author WITH =) WHERE (title = 'why');\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. WHERE .. with various operators" do
            let statement = AddConstraint
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
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (i1 WITH =, i2 WITH <>, i3 WITH !=, i4 WITH AND, i5 WITH OR) WHERE (title = 'why');\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. NOT DEFERRABLE" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just False
                    , deferrableType = Nothing
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =) NOT DEFERRABLE;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE INITIALLY IMMEDIATE" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Just InitiallyImmediate
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE INITIALLY IMMEDIATE;\n"

        it "should compile ALTER TABLE .. ADD CONSTRAINT .. EXCLUDE .. DEFERRABLE INITIALLY DEFERRED" do
            let statement = AddConstraint
                    { tableName = "posts"
                    , constraint = ExcludeConstraint
                        { name = "unique_title_by_author"
                        , excludeElements =
                            [ ExcludeConstraintElement { element = "title", operator = "=" }
                            ]
                        , predicate = Nothing
                        , indexType = Nothing
                        }
                    , deferrable = Just True
                    , deferrableType = Just InitiallyDeferred
                    }
            compileSql [statement] `shouldBe` "ALTER TABLE posts ADD CONSTRAINT unique_title_by_author EXCLUDE (title WITH =) DEFERRABLE INITIALLY DEFERRED;\n"

        it "should compile a CREATE TABLE with text default value in columns" do
            let sql = cs [plain|CREATE TABLE a (\n    content TEXT DEFAULT 'example text' NOT NULL\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "a"
                    , columns = [
                        Column
                            { name = "content"
                            , columnType = PText
                            , defaultValue = Just (TextExpression "example text")
                            , notNull = True
                            , isUnique = False
                            , generator = Nothing
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
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
            let statement = StatementCreateTable CreateTable
                    { name = "deprecated_variables"
                    , columns =
                        [ Column
                            { name = "a"
                            , columnType = (PNumeric Nothing Nothing)
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "b"
                            , columnType = (PNumeric (Just 1) Nothing)
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "c"
                            , columnType = (PNumeric (Just 1) (Just 2))
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        , Column
                            { name = "d"
                            , columnType = (PVaryingN (Just 10))
                            , defaultValue = Nothing
                            , notNull = False
                            , isUnique = False
                            , generator = Nothing
                            }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a multi-column UNIQUE (a, b) constraint" do
            let sql = cs [plain|CREATE TABLE user_followers (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    user_id UUID NOT NULL,\n    follower_id UUID NOT NULL,\n    UNIQUE(user_id, follower_id)\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "user_followers"
                    , columns =
                        [ col { name = "id", columnType = PUUID, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                        , col { name = "user_id", columnType = PUUID, notNull = True }
                        , col { name = "follower_id", columnType = PUUID, notNull = True }
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = [ UniqueConstraint { name = Nothing, columnNames = [ "user_id", "follower_id" ] } ]
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a serial id" do
            let sql = cs [plain|CREATE TABLE orders (\n    id SERIAL PRIMARY KEY NOT NULL\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PSerial, notNull = True} ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a bigserial id" do
            let sql = cs [plain|CREATE TABLE orders (\n    id BIGSERIAL PRIMARY KEY NOT NULL\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "orders"
                    , columns = [ col { name = "id", columnType = PBigserial, notNull = True} ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with a composite primary key" do
            let sql = cs [plain|CREATE TABLE orderTrucks (\n    order_id BIGSERIAL NOT NULL,\n    truck_id BIGSERIAL NOT NULL,\n    PRIMARY KEY(order_id, truck_id)\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "orderTrucks"
                    , columns =
                        [ col { name = "order_id", columnType = PBigserial, notNull = True}
                        , col { name = "truck_id", columnType = PBigserial, notNull = True}
                        ]
                    , primaryKeyConstraint = PrimaryKeyConstraint ["order_id", "truck_id"]
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with an array column" do
            let sql = cs [plain|CREATE TABLE array_tests (\n    pay_by_quarter INT[]\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "array_tests"
                    , columns = [ col { name = "pay_by_quarter", columnType = PArray PInt } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with an point column" do
            let sql = cs [plain|CREATE TABLE point_tests (\n    pos POINT\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "point_tests"
                    , columns = [ col { name = "pos", columnType = PPoint } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE TABLE statement with an polygon column" do
            let sql = cs [plain|CREATE TABLE polygon_tests (\n    poly POLYGON\n);\n|]
            let statement = StatementCreateTable CreateTable
                    { name = "polygon_tests"
                    , columns = [ col { name = "poly", columnType = PPolygon } ]
                    , primaryKeyConstraint = PrimaryKeyConstraint []
                    , constraints = []
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX statement" do
            let sql = cs [plain|CREATE INDEX users_index ON users (user_name);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql
        
        it "should escape an index name inside a 'CREATE INDEX' statement" do
            let sql = cs [plain|CREATE INDEX "Some Index" ON "Some Table" ("Some Col");\n|]
            let statement = CreateIndex
                    { indexName = "Some Index"
                    , unique = False
                    , tableName = "Some Table"
                    , columns = [IndexColumn { column = VarExpression "Some Col", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a 'CREATE INDEX .. ON .. USING GIN' statement" do
            let sql = cs [plain|CREATE INDEX users_index ON users USING GIN (user_name);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Gin
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a 'CREATE INDEX .. ON .. USING BTREE' statement" do
            let sql = cs [plain|CREATE INDEX users_index ON users USING BTREE (user_name);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Btree
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a 'CREATE INDEX .. ON .. USING GIST' statement" do
            let sql = cs [plain|CREATE INDEX users_index ON users USING GIST (user_name);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [] }]
                    , whereClause = Nothing
                    , indexType = Just Gist
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX statement with multiple columns" do
            let sql = cs [plain|CREATE INDEX users_index ON users (user_name, project_id);\n|]
            let statement = CreateIndex
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
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX statement with a LOWER call" do
            let sql = cs [plain|CREATE INDEX users_email_index ON users (LOWER(email));\n|]
            let statement = CreateIndex
                    { indexName = "users_email_index"
                    , unique = False
                    , tableName = "users"
                    , columns = [IndexColumn { column = CallExpression "LOWER" [VarExpression "email"], columnOrder = []}]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE UNIQUE INDEX statement" do
            let sql = cs [plain|CREATE UNIQUE INDEX users_index ON users (user_name);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = []}]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX with column order ASC NULLS FIRST statement" do
            let sql = cs [plain|CREATE UNIQUE INDEX users_index ON users (user_name ASC NULLS FIRST);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [Asc, NullsFirst]}]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE INDEX with column order DESC NULLS LAST statement" do
            let sql = cs [plain|CREATE UNIQUE INDEX users_index ON users (user_name DESC NULLS LAST);\n|]
            let statement = CreateIndex
                    { indexName = "users_index"
                    , unique = True
                    , tableName = "users"
                    , columns = [IndexColumn { column = VarExpression "user_name", columnOrder = [Desc, NullsLast]}]
                    , whereClause = Nothing
                    , indexType = Nothing
                    }
            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE OR REPLACE FUNCTION ..() RETURNS TRIGGER .." do
            let sql = cs [plain|CREATE OR REPLACE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;\n|]
            let statement = CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = []
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = True
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE FUNCTION ..() RETURNS TRIGGER .." do
            let sql = cs [plain|CREATE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;\n|]
            let statement = CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = []
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = False
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

            compileSql [statement] `shouldBe` sql

        it "should compile a CREATE FUNCTION with parameters ..() RETURNS TRIGGER .." do
            let sql = cs [plain|CREATE FUNCTION notify_did_insert_webrtc_connection(param1 TEXT, param2 INT) RETURNS TRIGGER AS $$ BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; $$ language plpgsql;\n|]
            let statement = CreateFunction
                    { functionName = "notify_did_insert_webrtc_connection"
                    , functionArguments = [("param1", PText), ("param2", PInt)]
                    , functionBody = " BEGIN PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text); RETURN NEW; END; "
                    , orReplace = False
                    , returns = PTrigger
                    , language = "plpgsql"
                    }

            compileSql [statement] `shouldBe` sql


        it "should compile a CREATE TRIGGER .." do
            let sql = cs [plain|CREATE TRIGGER t AFTER INSERT ON x FOR EACH ROW EXECUTE PROCEDURE y();\n|]
            let statement = UnknownStatement { raw = "CREATE TRIGGER t AFTER INSERT ON x FOR EACH ROW EXECUTE PROCEDURE y()"  }
            compileSql [statement] `shouldBe` sql

        it "should compile a decimal default value with a type-cast" do
            let sql = "CREATE TABLE a (\n    electricity_unit_price DOUBLE PRECISION DEFAULT 0.17::DOUBLE PRECISION NOT NULL\n);\n"
            let statement = StatementCreateTable CreateTable { name = "a", columns = [Column {name = "electricity_unit_price", columnType = PDouble, defaultValue = Just (TypeCastExpression (DoubleExpression 0.17) PDouble), notNull = True, isUnique = False, generator = Nothing}], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
            compileSql [statement] `shouldBe` sql

        it "should compile a integer default value" do
            let sql = "CREATE TABLE a (\n    electricity_unit_price INT DEFAULT 0 NOT NULL\n);\n"
            let statement = StatementCreateTable CreateTable { name = "a", columns = [Column {name = "electricity_unit_price", columnType = PInt, defaultValue = Just (IntExpression 0), notNull = True, isUnique = False, generator = Nothing}], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
            compileSql [statement] `shouldBe` sql

        it "should compile a partial index" do
            let sql = cs [plain|CREATE UNIQUE INDEX unique_source_id ON listings (source, source_id) WHERE source IS NOT NULL AND source_id IS NOT NULL;\n|]
            let index = CreateIndex
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
            compileSql [index] `shouldBe` sql

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
        
        it "should compile 'CREATE POLICY' statements with a 'ihp_user_id() IS NOT NULL' expression" do
            -- https://github.com/digitallyinduced/ihp/issues/1412
            let sql = "CREATE POLICY \"Users can manage tasks if logged in\" ON tasks USING (ihp_user_id() IS NOT NULL) WITH CHECK (ihp_user_id() IS NOT NULL);\n"
            let policy = CreatePolicy
                    { name = "Users can manage tasks if logged in"
                    , action = Nothing
                    , tableName = "tasks"
                    , using = Just (
                        IsExpression
                            (CallExpression "ihp_user_id" [])
                            (NotExpression (VarExpression "NULL"))
                        )
                    , check = Just (
                        IsExpression
                            (CallExpression "ihp_user_id" [])
                            (NotExpression (VarExpression "NULL"))
                        )
                    }
            compileSql [policy] `shouldBe` sql

        it "should compile 'CREATE POLICY .. FOR SELECT' statements" do
            let sql = "CREATE POLICY \"Messages are public\" ON messages FOR SELECT USING (true);\n"
            let policy = CreatePolicy
                    { name = "Messages are public"
                    , action = Just PolicyForSelect
                    , tableName = "messages"
                    , using = Just (VarExpression "true")
                    , check = Nothing
                    }
            compileSql [policy] `shouldBe` sql

        it "should use parentheses where needed" do
            -- https://github.com/digitallyinduced/ihp/issues/1087
            let inputSql = cs [plain|ALTER TABLE listings ADD CONSTRAINT source CHECK ((NOT (user_id IS NOT NULL AND agent_id IS NOT NULL)) AND (user_id IS NOT NULL OR agent_id IS NOT NULL));\n|]
            compileSql [parseSql inputSql] `shouldBe` inputSql

        it "should compile 'ALTER TABLE .. DROP COLUMN ..' statements" do
            let sql = "ALTER TABLE tasks DROP COLUMN description;\n"
            let statements = [ DropColumn { tableName = "tasks", columnName = "description" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP TABLE ..' statements" do
            let sql = "DROP TABLE tasks;\n"
            let statements = [ DropTable { tableName = "tasks" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE SEQUENCE ..' statements" do
            let sql = "CREATE SEQUENCE a;\n"
            let statements = [ CreateSequence { name = "a" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. RENAME COLUMN .. TO ..' statements" do
            let sql = "ALTER TABLE users RENAME COLUMN name TO full_name;\n"
            let statements = [ RenameColumn { tableName = "users", from = "name", to = "full_name" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ADD UNIQUE (..);' statements" do
            let sql = "ALTER TABLE users ADD UNIQUE (full_name);\n"
            let statements = [ AddConstraint { tableName = "users", constraint = UniqueConstraint { name = Nothing, columnNames = ["full_name"] }, deferrable = Nothing, deferrableType = Nothing } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. DROP CONSTRAINT ..;' statements" do
            let sql = "ALTER TABLE users DROP CONSTRAINT users_full_name_key;\n"
            let statements = [ DropConstraint { tableName = "users", constraintName = "users_full_name_key" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP TYPE ..;' statements" do
            let sql = "DROP TYPE colors;\n"
            let statements = [ DropEnumType { name = "colors" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP INDEX ..;' statements" do
            let sql = "DROP INDEX a;\n"
            let statements = [ DropIndex { indexName = "a" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ALTER COLUMN .. DROP NOT NULL;' statements" do
            let sql = "ALTER TABLE users ALTER COLUMN email DROP NOT NULL;\n"
            let statements = [ DropNotNull { tableName = "users", columnName = "email" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ALTER COLUMN .. SET NOT NULL;' statements" do
            let sql = "ALTER TABLE users ALTER COLUMN email SET NOT NULL;\n"
            let statements = [ SetNotNull { tableName = "users", columnName = "email" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ALTER COLUMN .. SET DEFAULT ..;' statements" do
            let sql = "ALTER TABLE users ALTER COLUMN email SET DEFAULT null;\n"
            let statements = [ SetDefaultValue { tableName = "users", columnName = "email", value = VarExpression "null" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ALTER COLUMN .. DROP DEFAULT;' statements" do
            let sql = "ALTER TABLE users ALTER COLUMN email DROP DEFAULT;\n"
            let statements = [ DropDefaultValue { tableName = "users", columnName = "email" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. RENAME TO ..;' statements" do
            let sql = "ALTER TABLE profiles RENAME TO users;\n"
            let statements = [ RenameTable { from = "profiles", to = "users" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'DROP POLICY .. ON ..;' statements" do
            let sql = "DROP POLICY \"Users can manage their todos\" ON todos;\n"
            let statements = [ DropPolicy { tableName = "todos", policyName = "Users can manage their todos" } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE EXTENSION IF NOT EXISTS;' statements with an unqualified name" do
            let sql = "CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;\n"
            let statements = [ CreateExtension { name = "fuzzystrmatch", ifNotExists = True } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE POLICY ..;' statements with an EXISTS condition" do
            let sql = cs [plain|CREATE POLICY "Users can manage their project's migrations" ON migrations USING (EXISTS (SELECT 1 FROM public.projects WHERE projects.id = migrations.project_id)) WITH CHECK (EXISTS (SELECT 1 FROM public.projects WHERE projects.id = migrations.project_id));\n|]
            let statements =
                    [ CreatePolicy
                        { name = "Users can manage their project's migrations"
                        , action = Nothing
                        , tableName = "migrations"
                        , using = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "projects", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "projects") "id") (DotExpression (VarExpression "migrations") "project_id")})))
                        , check = Just (ExistsExpression (SelectExpression (Select {columns = [IntExpression 1], from = DotExpression (VarExpression "public") "projects", alias = Nothing, whereClause = EqExpression (DotExpression (VarExpression "projects") "id") (DotExpression (VarExpression "migrations") "project_id")})))
                        }
                    ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TYPE .. ADD VALUE ..;' statements" do
            let sql = "ALTER TYPE colors ADD VALUE 'blue';\n"
            let statements = [ AddValueToEnumType { enumName = "colors", newValue = "blue", ifNotExists = False } ]
            compileSql statements `shouldBe` sql

        it "should compile 'CREATE TRIGGER .. AFTER INSERT ON .. FOR EACH ROW EXECUTE ..;' statements" do
            let sql = "CREATE TRIGGER call_test_function_for_new_users AFTER INSERT ON users FOR EACH ROW EXECUTE FUNCTION call_test_function('hello');\n"
            let statements = [ CreateTrigger
                    { name = "call_test_function_for_new_users"
                    , eventWhen = After
                    , event = TriggerOnInsert
                    , tableName = "users"
                    , for = ForEachRow
                    , whenCondition = Nothing
                    , functionName = "call_test_function"
                    , arguments = [TextExpression "hello"]
                    } ]
            compileSql statements `shouldBe` sql

        it "should compile 'BEGIN;' statements" do
            let sql = "BEGIN;\n"
            let statements = [ Begin ]
            compileSql statements `shouldBe` sql

        it "should compile 'COMMIT;' statements" do
            let sql = "COMMIT;\n"
            let statements = [ Commit ]
            compileSql statements `shouldBe` sql

        it "should compile 'ALTER TABLE .. ALTER COLUMN .. DROP NOT NULL;' statements" do
            let sql = "COMMIT;\n"
            let statements = [ Commit ]
            compileSql statements `shouldBe` sql
        it "should compile 'GENERATED' columns" do
            let sql = [trimming|
                CREATE TABLE products (
                    ts TSVECTOR GENERATED ALWAYS AS (setweight(to_tsvector('english', sku), ('A'::"char")) || setweight(to_tsvector('english', name), 'B') || setweight(to_tsvector('english', description), 'C')) STORED
                );
            |] <> "\n"
            let statements = [
                        StatementCreateTable CreateTable
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
                                                            (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "sku"], TypeCastExpression (TextExpression "A") PSingleChar])
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
                        ]
            compileSql statements `shouldBe` sql
