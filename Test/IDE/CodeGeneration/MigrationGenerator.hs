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
                let migration = sql [i|ALTER TABLE users ADD COLUMN email TEXT NOT NULL;|]

                diffSchemas targetSchema actualSchema `shouldBe` migration


            it "should handle multiple new columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
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
                let migration = sql [i|
                    ALTER TABLE users DROP COLUMN email;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration


            it "should handle renamed columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users RENAME COLUMN name TO full_name;|]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should handle UNIQUE constraints added to columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL UNIQUE
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users ADD UNIQUE (full_name);|]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should handle UNIQUE constraints removed from columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL UNIQUE
                    );
                |]
                let migration = sql [i|ALTER TABLE users DROP CONSTRAINT users_full_name_key;|]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle new enums" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                    CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle new enum values" do
                let targetSchema = sql [i|
                    CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
                |]
                let actualSchema = sql [i|
                    CREATE TYPE mood AS ENUM ('sad', 'ok');
                |]
                let migration = sql [i|
                    ALTER TYPE mood ADD VALUE 'happy';
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle a real world table" do
                let targetSchema = sql [i|
                    CREATE TABLE subscribe_to_convert_kit_tag_jobs (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
                        last_error TEXT DEFAULT NULL,
                        attempts_count INT DEFAULT 0 NOT NULL,
                        locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                        locked_by UUID DEFAULT NULL,
                        run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        user_id UUID NOT NULL,
                        tag_id INT NOT NULL
                    );
                |]

                let actualSchema = sql [i|
                    CREATE TABLE public.subscribe_to_convert_kit_tag_jobs (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        created_at timestamp with time zone DEFAULT now() NOT NULL,
                        updated_at timestamp with time zone DEFAULT now() NOT NULL,
                        status public.job_status DEFAULT 'job_status_not_started'::public.job_status NOT NULL,
                        last_error text,
                        attempts_count integer DEFAULT 0 NOT NULL,
                        locked_at timestamp with time zone,
                        locked_by uuid,
                        run_at timestamp with time zone DEFAULT now() NOT NULL,
                        user_id uuid NOT NULL,
                        tag_id integer NOT NULL
                    );
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []

            it "should not detect unspecified on delete behaviour as a change" do
                let targetSchema = sql [i|
                    ALTER TABLE subscriptions ADD CONSTRAINT subscriptions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]

                let actualSchema = sql [i|
                    ALTER TABLE ONLY public.subscriptions ADD CONSTRAINT subscriptions_ref_user_id FOREIGN KEY (user_id) REFERENCES public.users(id);
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []

            it "should not detect changes on case differences of enums" do
                let targetSchema = sql [i|
                    CREATE TYPE A AS ENUM ();
                |]

                let actualSchema = sql [i|
                    CREATE TYPE a AS ENUM ();
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []
            
            it "should handle a deleted table" do
                let targetSchema = sql ""
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    DROP TABLE users;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle a deleted enum" do
                let targetSchema = sql ""
                let actualSchema = sql [i|
                    CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
                |]
                let migration = sql [i|
                    DROP TYPE mood;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle a new indexes" do
                let targetSchema = sql [i|
                    CREATE INDEX users_index ON users (user_name);
                |]
                let actualSchema = sql ""
                let migration = sql [i|
                    CREATE INDEX users_index ON users (user_name);
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration 
            
            it "should handle deleted indexes" do
                let targetSchema = sql ""
                let actualSchema = sql [i|
                    CREATE INDEX users_index ON users (user_name);
                |]
                let migration = sql [i|
                    DROP INDEX users_index;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration 
            
            it "should handle columns that have been made nullable" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT
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
                    ALTER TABLE users ALTER COLUMN email DROP NOT NULL;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should handle columns that have been made not nullable" do
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
                        name TEXT NOT NULL,
                        email TEXT
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE users ALTER COLUMN email SET NOT NULL;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration 


sql :: Text -> [Statement]
sql code = case Megaparsec.runParser Parser.parseDDL "" code of
    Left parsingFailed -> error (tshow parsingFailed)
    Right r -> r