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
                    -- Commit the transaction previously started by IHP
                    COMMIT;
                    ALTER TYPE mood ADD VALUE IF NOT EXISTS 'happy';
                    -- Restart the connection as IHP will also try to run it's own COMMIT
                    BEGIN;
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

            it "should normalize Bigserials" do
                let targetSchema = sql [i|
                    CREATE TABLE testserial (
                        testcol BIGSERIAL NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE public.testserial (
                        testcol bigint NOT NULL
                    );

                    CREATE SEQUENCE public.testserial_testcol_seq
                        START WITH 1
                        INCREMENT BY 1
                        NO MINVALUE
                        NO MAXVALUE
                        CACHE 1;

                    ALTER SEQUENCE public.testserial_testcol_seq OWNED BY public.testserial.testcol;
                    ALTER TABLE ONLY public.testserial ALTER COLUMN testcol SET DEFAULT nextval('public.testserial_testcol_seq'::regclass);
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []
            
            it "should normalize Serials" do
                let targetSchema = sql [i|
                    CREATE TABLE testserial (
                        testcol SERIAL NOT NULL
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE public.testserial (
                        testcol int NOT NULL
                    );

                    CREATE SEQUENCE public.testserial_testcol_seq
                        START WITH 1
                        INCREMENT BY 1
                        NO MINVALUE
                        NO MAXVALUE
                        CACHE 1;

                    ALTER SEQUENCE public.testserial_testcol_seq OWNED BY public.testserial.testcol;
                    ALTER TABLE ONLY public.testserial ALTER COLUMN testcol SET DEFAULT nextval('public.testserial_testcol_seq'::regclass);
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []

            
            it "should normalize index expressions" do
                let targetSchema = sql [i|
                    CREATE INDEX users_email_index ON users (LOWER(email));
                |]
                let actualSchema = sql [i|
                    CREATE INDEX users_email_index ON public.users USING btree (lower(email));
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []

            it "should not detect a difference between two functions when the only difference is between 'CREATE' and 'CREATE OR REPLACE'" do
                let targetSchema = sql [i|
                    CREATE OR REPLACE FUNCTION notify_did_insert_webrtc_connection() RETURNS TRIGGER AS $$
                    BEGIN
                        PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text);
                        RETURN NEW;
                    END;
                    $$ language plpgsql;
                |]
                let actualSchema = sql [i|
                    CREATE FUNCTION public.notify_did_insert_webrtc_connection() RETURNS trigger
                        LANGUAGE plpgsql
                        AS $$
                    BEGIN
                        PERFORM pg_notify('did_insert_webrtc_connection', json_build_object('id', NEW.id, 'floor_id', NEW.floor_id, 'source_user_id', NEW.source_user_id, 'target_user_id', NEW.target_user_id)::text);
                        RETURN NEW;
                    END;
                    $$;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []
            
            it "should normalize aliases in policies" do
                let targetSchema = sql [i|
                    CREATE POLICY "Users can see other users in their company" ON users USING (company_id = (SELECT users.company_id FROM users WHERE users.id = ihp_user_id()));
                |]
                let actualSchema = sql [i|
                    CREATE POLICY "Users can see other users in their company" ON public.users USING ((company_id = ( SELECT users_1.company_id
                       FROM public.users users_1
                      WHERE (users_1.id = public.ihp_user_id()))));
                |]

                diffSchemas targetSchema actualSchema `shouldBe` []
            
            it "should handle implicitly deleted indexes and constraints" do
                let targetSchema = sql [i|
                |]
                let actualSchema = sql [i|
                    CREATE TABLE projects (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        user_id UUID NOT NULL
                    );
                    CREATE INDEX projects_name_index ON projects (name);
                    ALTER TABLE projects ADD CONSTRAINT projects_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
                |]
                let migration = sql [i|
                    DROP TABLE projects;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should run 'ALTER TYPE .. ADD VALUE ..' outside of a transaction" do
                let targetSchema = sql [i|
                    CREATE TABLE a();
                    CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
                    CREATE TABLE b();
                |]
                let actualSchema = sql [i|
                    CREATE TYPE mood AS ENUM ('sad', 'ok');
                |]
                let migration = sql [i|
                    -- Commit the transaction previously started by IHP
                    COMMIT;
                    ALTER TYPE mood ADD VALUE IF NOT EXISTS 'happy';
                    -- Restart the connection as IHP will also try to run it's own COMMIT
                    BEGIN;
                    CREATE TABLE a();
                    CREATE TABLE b();
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should run not generate a default value for a generated column" do
                let targetSchema = sql [i|
                    CREATE TABLE products (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        description TEXT NOT NULL,
                        sku TEXT NOT NULL,
                        text_search TSVECTOR GENERATED ALWAYS AS 
                            ( setweight(to_tsvector('english', sku), 'A') ||
                              setweight(to_tsvector('english', name), 'B') ||
                              setweight(to_tsvector('english', description), 'C')
                            ) STORED
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE products (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        description TEXT NOT NULL,
                        sku TEXT NOT NULL
                    );
                |]
                let migration = sql [i|
                    ALTER TABLE products ADD COLUMN text_search TSVECTOR GENERATED ALWAYS AS (setweight(to_tsvector('english', sku), 'A') || setweight(to_tsvector('english', name), 'B') || setweight(to_tsvector('english', description), 'C')) STORED;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should normalize generated columns" do
                let targetSchema = sql [i|
                    CREATE TABLE products (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        name TEXT NOT NULL,
                        description TEXT NOT NULL,
                        sku TEXT NOT NULL,
                        text_search TSVECTOR GENERATED ALWAYS AS 
                            ( setweight(to_tsvector('english', sku), 'A') ||
                              setweight(to_tsvector('english', name), 'B') ||
                              setweight(to_tsvector('english', description), 'C')
                            ) STORED
                    );
                |]
                let actualSchema = sql [i|
                    CREATE TABLE public.products (
                        id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
                        name text NOT NULL,
                        description text NOT NULL,
                        sku text NOT NULL,
                        text_search tsvector GENERATED ALWAYS AS (((setweight(to_tsvector('english'::regconfig, sku), 'A'::"char") || setweight(to_tsvector('english'::regconfig, name), 'B'::"char")) || setweight(to_tsvector('english'::regconfig, description), 'C'::"char"))) STORED
                    );
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should not detect changes if the LANGUAGE is in difference casing" do
                let targetSchema = sql [trimming|
                    CREATE FUNCTION ihp_user_id() RETURNS UUID AS $$$$
                        SELECT NULLIF(current_setting('rls.ihp_user_id'), '')::uuid;
                    $$$$ LANGUAGE SQL;

                    CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$$$
                    BEGIN
                        NEW.updated_at = NOW();
                        RETURN NEW;
                    END;
                    $$$$ language plpgsql;
                |]
                let actualSchema = sql [trimming|
                    --
                    -- Name: ihp_user_id(); Type: FUNCTION; Schema: public; Owner: -
                    --

                    CREATE FUNCTION public.ihp_user_id() RETURNS uuid
                        LANGUAGE sql
                        AS $$$$
                        SELECT NULLIF(current_setting('rls.ihp_user_id'), '')::uuid;
                    $$$$;

                    CREATE FUNCTION public.set_updated_at_to_now() RETURNS trigger
                        LANGUAGE plpgsql
                        AS $$$$
                    BEGIN
                        NEW.updated_at = NOW();
                        RETURN NEW;
                    END;
                    $$$$;
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should not try dropping an index after already droping a column" do
                let targetSchema = sql [trimming|
                    CREATE TABLE tasks (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        user_id UUID DEFAULT ihp_user_id() NOT NULL,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
                    );
                    CREATE INDEX tasks_updated_at_index ON tasks (updated_at);
                |]
                let actualSchema = sql [trimming|
                    CREATE TABLE tasks (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        user_id UUID DEFAULT ihp_user_id() NOT NULL,
                        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
                    );
                    CREATE INDEX tasks_created_at_index ON tasks (created_at);
                    CREATE INDEX tasks_updated_at_index ON tasks (updated_at);
                |]
                let migration = sql [i|
                    ALTER TABLE tasks DROP COLUMN created_at;
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "ignore auto generated 'notify_...' functions" do
                let targetSchema = sql [trimming|
                |]
                let actualSchema = sql [trimming|
                    CREATE FUNCTION public.notify_did_change_todos() RETURNS trigger
                        LANGUAGE plpgsql
                        AS $$$$
                            BEGIN
                                CASE TG_OP
                                WHEN ''UPDATE'' THEN
                                    PERFORM pg_notify(
                                        ''did_change_todos'',
                                        json_build_object(
                                          ''UPDATE'', NEW.id::text,
                                          ''CHANGESET'', (
                                                SELECT json_agg(row_to_json(t))
                                                FROM (
                                                      SELECT pre.key AS "col", post.value AS "new"
                                                      FROM jsonb_each(to_jsonb(OLD)) AS pre
                                                      CROSS JOIN jsonb_each(to_jsonb(NEW)) AS post
                                                      WHERE pre.key = post.key AND pre.value IS DISTINCT FROM post.value
                                                ) t
                                          )
                                        )::text
                                    );
                                WHEN ''DELETE'' THEN
                                    PERFORM pg_notify(
                                        ''did_change_todos'',
                                        (json_build_object(''DELETE'', OLD.id)::text)
                                    );
                                WHEN ''INSERT'' THEN
                                    PERFORM pg_notify(
                                        ''did_change_todos'',
                                        json_build_object(''INSERT'', NEW.id)::text
                                    );
                                END CASE;
                                RETURN new;
                            END;
                        $$$$;
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should normalize unique constraint names with multiple columns" do
                let targetSchema = sql $ cs [plain|
                    ALTER TABLE days ADD UNIQUE (category_id, date);
                |]
                let actualSchema = sql $ cs [plain|
                    ALTER TABLE ONLY public.days ADD CONSTRAINT days_category_id_date_key UNIQUE (category_id, date);
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should not detect changes between functions where only the whitespace is different" do
                let targetSchema = sql $ cs [plain|
CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language PLPGSQL;
                |]
                let actualSchema = sql $ cs [plain|
                    
CREATE FUNCTION public.set_updated_at_to_now() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        NEW.updated_at = NOW();
        RETURN NEW;
    END;
    $$;
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration

            it "should replace functions if the body has changed" do
                let targetSchema = sql $ cs [plain|
CREATE FUNCTION a() RETURNS TRIGGER AS $$
BEGIN
    hello_world();
END;
$$ language PLPGSQL;
                |]
                let actualSchema = sql $ cs [plain|
                    
CREATE FUNCTION public.a() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        RETURN NEW;
    END;
    $$;
                |]
                let migration = sql [i|
CREATE OR REPLACE FUNCTION a() RETURNS TRIGGER AS $$BEGIN
    hello_world();
END;$$ language PLPGSQL;|]

                diffSchemas targetSchema actualSchema `shouldBe` migration
            
            it "should normalize qualified identifiers in policy expressions" do
                -- https://github.com/digitallyinduced/ihp/issues/1480
                let targetSchema = sql $ cs [plain|
                    CREATE POLICY "Users can manage servers they have access to" ON servers USING (servers.user_id = ihp_user_id() OR (EXISTS (SELECT 1 FROM public.user_server_access WHERE user_server_access.user_id = ihp_user_id() AND user_server_access.server_id = servers.id)));
                |]
                let actualSchema = sql $ cs [plain|
                    --
                    -- Name: servers Users can manage servers they have access to; Type: POLICY; Schema: public; Owner: -
                    --

                    CREATE POLICY "Users can manage servers they have access to" ON public.servers USING (((user_id = public.ihp_user_id()) OR (EXISTS ( SELECT 1
                       FROM public.user_server_access
                      WHERE ((user_server_access.user_id = public.ihp_user_id()) AND (user_server_access.server_id = servers.id))))));
                |]
                let migration = sql [i|
                |]

                diffSchemas targetSchema actualSchema `shouldBe` migration


sql :: Text -> [Statement]
sql code = case Megaparsec.runParser Parser.parseDDL "" code of
    Left parsingFailed -> error (cs $ Megaparsec.errorBundlePretty parsingFailed)
    Right r -> r