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
                let migration = sql [i|ALTER TABLE users ADD CONSTRAINT "users_full_name_key" UNIQUE (full_name);|]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle changing default values for columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT DEFAULT 'new value' NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT DEFAULT 'old value' NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users ALTER COLUMN full_name SET DEFAULT 'new value';|]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle default values added to columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT DEFAULT 'value' NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users ALTER COLUMN full_name SET DEFAULT 'value';|]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should handle default values removed from columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        full_name TEXT DEFAULT 'value' NOT NULL
                    );
                |]
                let migration = sql [i|ALTER TABLE users ALTER COLUMN full_name DROP DEFAULT;|]

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
            
            it "should handle table renames" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE profiles (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE profiles RENAME TO users;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration 
            
            it "should not do a rename if tables are different" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE profiles (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        email TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    DROP TABLE profiles;
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL
                    );
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration 
            
            it "should handle new foreign keys" do
                let targetSchema = sql [i|
                    ALTER TABLE messages ADD CONSTRAINT messages_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]
                let actualSchema = sql ""
                let migration = sql [i|
                    ALTER TABLE messages ADD CONSTRAINT messages_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should handle new foreign keys" do
                let targetSchema = sql ""
                let actualSchema = sql [i|
                    ALTER TABLE ONLY public.messages ADD CONSTRAINT messages_ref_user_id FOREIGN KEY (user_id) REFERENCES public.users(id);
                |]
                let migration = sql [i|
                    ALTER TABLE messages DROP CONSTRAINT messages_ref_user_id;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should handle new policies" do
                let targetSchema = sql [i|
                    CREATE POLICY "Users can manage their todos" ON todos USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                |]
                let actualSchema = sql ""
                let migration = sql [i|
                    CREATE POLICY "Users can manage their todos" ON todos USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            
            it "should handle deleted policies" do
                let targetSchema = sql ""
                let actualSchema = sql [i|
                    CREATE POLICY "Users can manage their todos" ON todos USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                |]
                let migration = sql [i|
                    DROP POLICY "Users can manage their todos" ON todos;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should normalize primary keys" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        email TEXT NOT NULL,
                        password_hash TEXT NOT NULL,
                        locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                        failed_login_attempts INT DEFAULT 0 NOT NULL,
                        access_token TEXT DEFAULT NULL
                    );
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        body TEXT NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    --
                    -- PostgreSQL database dump
                    --

                    -- Dumped from database version 14.0 (Debian 14.0-1.pgdg110+1)
                    -- Dumped by pg_dump version 14beta1

                    SET statement_timeout = 0;
                    SET lock_timeout = 0;
                    SET idle_in_transaction_session_timeout = 0;
                    SET client_encoding = 'UTF8';
                    SET standard_conforming_strings = on;
                    SELECT pg_catalog.set_config('search_path', '', false);
                    SET default_toast_compression = 'pglz';
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


                    --
                    -- Name: ihp_user_id(); Type: FUNCTION; Schema: public; Owner: -
                    --

                    CREATE FUNCTION public.ihp_user_id() RETURNS uuid
                        LANGUAGE sql
                        AS $$ SELECT current_setting('rls.ihp_user_id')::uuid; $$;


                    SET default_tablespace = '';

                    SET default_table_access_method = heap;

                    --
                    -- Name: users; Type: TABLE; Schema: public; Owner: -
                    --

                    CREATE TABLE public.users (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        email text NOT NULL,
                        password_hash text NOT NULL,
                        locked_at timestamp with time zone,
                        failed_login_attempts integer DEFAULT 0 NOT NULL,
                        access_token text
                    );


                    --
                    -- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
                    --

                    ALTER TABLE ONLY public.users
                        ADD CONSTRAINT users_pkey PRIMARY KEY (id);


                    --
                    -- PostgreSQL database dump complete
                    --

                |]
                let migration = sql [i|
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        body TEXT NOT NULL
                    );
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            it "should generate statements in the right order" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        email TEXT NOT NULL,
                        password_hash TEXT NOT NULL,
                        locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                        failed_login_attempts INT DEFAULT 0 NOT NULL,
                        access_token TEXT DEFAULT NULL
                    );
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        body TEXT NOT NULL,
                        user_id UUID NOT NULL
                    );
                    CREATE INDEX posts_user_id_index ON posts (user_id);
                    ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]
                let actualSchema = sql [i|
                    --
                    -- PostgreSQL database dump
                    --

                    -- Dumped from database version 14.0 (Debian 14.0-1.pgdg110+1)
                    -- Dumped by pg_dump version 14beta1

                    SET statement_timeout = 0;
                    SET lock_timeout = 0;
                    SET idle_in_transaction_session_timeout = 0;
                    SET client_encoding = 'UTF8';
                    SET standard_conforming_strings = on;
                    SELECT pg_catalog.set_config('search_path', '', false);
                    SET default_toast_compression = 'pglz';
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


                    --
                    -- Name: ihp_user_id(); Type: FUNCTION; Schema: public; Owner: -
                    --

                    CREATE FUNCTION public.ihp_user_id() RETURNS uuid
                        LANGUAGE sql
                        AS $$ SELECT current_setting('rls.ihp_user_id')::uuid; $$;


                    SET default_tablespace = '';

                    SET default_table_access_method = heap;

                    --
                    -- Name: users; Type: TABLE; Schema: public; Owner: -
                    --

                    CREATE TABLE public.users (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        email text NOT NULL,
                        password_hash text NOT NULL,
                        locked_at timestamp with time zone,
                        failed_login_attempts integer DEFAULT 0 NOT NULL,
                        access_token text
                    );


                    --
                    -- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
                    --

                    ALTER TABLE ONLY public.users
                        ADD CONSTRAINT users_pkey PRIMARY KEY (id);

                |]
                let migration = sql [i|
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        body TEXT NOT NULL,
                        user_id UUID NOT NULL
                    );
                    CREATE INDEX posts_user_id_index ON posts (user_id);
                    ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration


            it "should normalize unique constraints on columns" do
                let targetSchema = sql [i|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        github_user_id INT DEFAULT NULL UNIQUE
                    );
                |]

                let actualSchema = sql [i|
                    CREATE TABLE public.users (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        github_user_id integer
                    );

                    ALTER TABLE ONLY public.users ADD CONSTRAINT users_github_user_id_key UNIQUE (github_user_id);
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []


            it "should normalize policy definitions" do
                let targetSchema = sql [i|
                    CREATE POLICY "Users can manage their project's migrations" ON migrations USING (EXISTS (SELECT 1 FROM projects WHERE projects.id = migrations.project_id)) WITH CHECK (EXISTS (SELECT 1 FROM projects WHERE projects.id = migrations.project_id));
                |]

                let actualSchema = sql [i|
                    CREATE POLICY "Users can manage their project's migrations" ON public.migrations USING ((EXISTS ( SELECT 1
                       FROM public.projects
                      WHERE (projects.id = migrations.project_id)))) WITH CHECK ((EXISTS ( SELECT 1
                       FROM public.projects
                      WHERE (projects.id = migrations.project_id))));
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []

            it "should normalize check constraints" do
                let targetSchema = sql [i|
                    CREATE TABLE public.a (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL
                    );
                    ALTER TABLE a ADD CONSTRAINT contact_email_or_url CHECK (contact_email IS NOT NULL OR source_url IS NOT NULL);
                |]

                let actualSchema = sql [i|
                    CREATE TABLE public.a (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        CONSTRAINT contact_email_or_url CHECK (((contact_email IS NOT NULL) OR (source_url IS NOT NULL)))
                    );
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []


sql :: Text -> [Statement]
sql code = case Megaparsec.runParser Parser.parseDDL "" code of
    Left parsingFailed -> error (tshow parsingFailed)
    Right r -> r