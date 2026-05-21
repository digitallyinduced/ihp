module Main where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.TypeScript.Compiler

import qualified IHP.Postgres.Parser as Parser
import IHP.Postgres.Types
import qualified Text.Megaparsec as Megaparsec

main :: IO ()
main = hspec tests

tests = do
    describe "Test.Web.View.TypeDefinitions.TypeScript" do
        describe "generateTypeScriptTypeDefinitions" do
            it "should generate a valid typescript definition file" do
                let schema = parseSqlStatements $ cs [plain|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        email TEXT NOT NULL,
                        password_hash TEXT NOT NULL,
                        locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                        failed_login_attempts INT DEFAULT 0 NOT NULL,
                        access_token TEXT DEFAULT NULL
                    );
                    CREATE TYPE colors AS ENUM ('red', 'blue');
                    CREATE TABLE tasks (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
                        user_id UUID NOT NULL,
                        color colors NOT NULL,
                        color_arr colors[] NOT NULL
                    );
                    CREATE INDEX tasks_user_id_index ON tasks (user_id);
                    ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                    CREATE POLICY "Users can manage their tasks" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                    ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
                |]
                let expected = [trimming|
                    declare module 'ihp-datasync' {
                        type Color = 'red' | 'blue';
                        interface User {
                            id: UUID;
                            email: string;
                            passwordHash: string;
                            lockedAt: string | null;
                            failedLoginAttempts: number;
                            accessToken: string | null;
                        }
                        interface Task {
                            id: UUID;
                            title: string;
                            createdAt: string;
                            userId: UUID;
                            color: Color;
                            colorArr: Array<Color>;
                        }
                        /**
                         * A User object not yet inserted into the `users` table
                         */
                        interface NewUser {
                            id?: UUID;
                            email: string;
                            passwordHash: string;
                            lockedAt?: string | null;
                            failedLoginAttempts?: number;
                            accessToken?: string | null;
                        }
                        /**
                         * A Task object not yet inserted into the `tasks` table
                         */
                        interface NewTask {
                            id?: UUID;
                            title: string;
                            createdAt?: string;
                            userId: UUID;
                            color: Color;
                            colorArr: Array<Color>;
                        }

                        interface TableRegistry {
                            "users": User;
                            "tasks": Task;
                        }

                        interface NewRecordRegistry {
                            "users": NewUser;
                            "tasks": NewTask;
                        }

                        function getCurrentUserId(): string;
                        function getCurrentUser(): Promise<User | null>;

                        interface LogoutOptions {
                            redirect?: string;
                        }

                        function logout(options?: LogoutOptions): Promise<void>;

                        /**
                         * Useful to implement a login button. Redirects the user to the login page.
                         *
                         * The returned promise never resolves, as the browser is redirected to a different page.
                         *
                         * @example
                         * import { loginWithRedirect } from 'ihp-datasync';
                         * function LoginButton() {
                         *      const isLoading = useState(false);
                         *
                         *      const doLogin = async () => {
                         *          setLoading(true);
                         *          await loginWithRedirect();
                         *          setLoading(false);
                         *      }
                         *
                         *      return <button onClick={doLogin} disabled={isLoading}>Login</button>
                         * }
                         */
                        function loginWithRedirect(): Promise<void>;
                        function ensureIsUser(): Promise<void>;
                        function initAuth(): Promise<void>;

                        function initThinBackend(options: { host: string | undefined; }): void;
                    }

                    declare module 'ihp-datasync/react' {
                        function useCurrentUser(): User | null;

                        /**
                         * Returns true if there's a user logged in. Returns false if there's no logged in user. Returns null if loading.
                         *
                         * @example
                         * const isLoggedIn = useIsLoggedIn();
                         */
                        function useIsLoggedIn(): boolean | null;

                        interface ThinBackendProps {
                            requireLogin?: boolean;
                            children: JSX.Element[] | JSX.Element;
                        }
                        function ThinBackend(props: ThinBackendProps): JSX.Element;
                    }
                |]

                (generateTypeScriptTypeDefinitions schema) `shouldBe` expected

        describe "recordInterface" do
            it "should generate required fields for fields with default values" do
                let t = (table "tasks")
                        { columns =
                            [ (col "id" PUUID) { notNull = True }
                            , (col "title" PText) { defaultValue = Just (TextExpression "untitled"), notNull = True }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let expected = [trimming|
                    interface Task {
                        id: UUID;
                        title: string;
                    }
                |]
                (recordInterface t) `shouldBe` expected

        describe "newRcordInterface" do
            it "should generate optional fields for fields with default values" do
                let t = (table "tasks")
                        { columns =
                            [ (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                            , (col "title" PText) { defaultValue = Just (TextExpression "untitled"), notNull = True }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let expected = [trimming|
                    /**
                     * A Task object not yet inserted into the `tasks` table
                     */
                    interface NewTask {
                        id?: UUID;
                        title?: string;
                    }
                |]
                (newRecordInterface t) `shouldBe` expected

parseSqlStatements :: Text -> [Statement]
parseSqlStatements sql =
    case Megaparsec.runParser Parser.parseDDL "input" sql of
            Left parserError -> error (cs $ Megaparsec.errorBundlePretty parserError) -- For better error reporting in hspec
            Right statements -> statements
