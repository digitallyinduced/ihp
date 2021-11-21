{-|
Module: Test.IDE.CodeGeneration.MigrationGenerator
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.IDE.CodeGeneration.MigrationGenerator where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.CodeGen.MigrationGenerator
import Data.String.Interpolate.IsString (i)
import qualified Text.Megaparsec as Megaparsec
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport

tests = do
    describe "MigrationGenerator" do
        describe "diffSchemas" do
            it "should handle an empty schema" do
                diffSchemas [] [] `shouldBe` []

            it "should handle a new table" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let actualSchema = sql ""

                diffSchemas targetSchema actualSchema `shouldBe` targetSchema

            it "should skip tables that are equals in both schemas" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let actualSchema = targetSchema

                diffSchemas targetSchema actualSchema `shouldBe` []

            it "should handle new columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users ADD COLUMN email TEXT NOT NULL;|]

                diffSchemas targetSchema actualSchema `shouldBe` migration


            it "should handle multiple new columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE users ADD COLUMN name TEXT NOT NULL;
                    ALTER TABLE users ADD COLUMN email TEXT NOT NULL;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle multiple new columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE users ADD COLUMN name TEXT NOT NULL;
                    ALTER TABLE users ADD COLUMN email TEXT NOT NULL;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle deleted columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE users DROP COLUMN email;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

sql :: Text -> [Statement]
sql code = case Megaparsec.runParser Parser.parseDDL "pg_dump" code of
    Left parsingFailed -> error (tshow parsingFailed)
    Right r -> r