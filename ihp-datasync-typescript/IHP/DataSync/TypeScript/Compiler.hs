module IHP.DataSync.TypeScript.Compiler where

import IHP.Prelude
import IHP.Postgres.Types

generateTypeScriptTypeDefinitions :: [Statement] -> Text
generateTypeScriptTypeDefinitions schema = [trimming|
declare module 'ihp-datasync' {
    ${enumTypes}
    ${recordInterfaces}
    ${newRecordInterfaces}

    interface TableRegistry {
        ${tableRegistryEntries}
    }

    interface NewRecordRegistry {
        ${newRecordRegistryEntries}
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

    ${initThinBackendTypeDef'}
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
    where
        tableNames :: [Text]
        tableNames = createTableStatements |> map (get #name)

        createTableStatements :: [CreateTable]
        createTableStatements =
                schema |> mapMaybe \case
                    StatementCreateTable { unsafeGetCreateTable = table } -> Just table
                    otherwise -> Nothing

        recordInterfaces :: Text
        recordInterfaces = createTableStatements
                |> map recordInterface
                |> intercalate "\n"

        newRecordInterfaces :: Text
        newRecordInterfaces = createTableStatements
                |> map newRecordInterface
                |> intercalate "\n"

        enumTypes :: Text
        enumTypes =
            schema
                |> mapMaybe \case
                    CreateEnumType { name, values } -> Just (generateEnumType name values)
                    otherwise -> Nothing
                |> intercalate "\n"

        tableRegistryEntries :: Text
        tableRegistryEntries = createTableStatements
                |> map (\table -> tshow (get #name table) <> ": " <> tableNameToModelName (get #name table) <> ";")
                |> intercalate "\n"

        newRecordRegistryEntries :: Text
        newRecordRegistryEntries = createTableStatements
                |> map (\table -> tshow (get #name table) <> ": New" <> tableNameToModelName (get #name table) <> ";")
                |> intercalate "\n"

        initThinBackendTypeDef' :: Text
        initThinBackendTypeDef' = initThinBackendTypeDef

recordInterface :: CreateTable -> Text
recordInterface CreateTable { name, columns } = "interface " <> tableNameToModelName name <> " {\n" <> fields <> "\n}"
    where
        fields = columns
                |> map columnToField
                |> intercalate "\n"
        columnToField Column { name, columnType, notNull } = "    " <> columnNameToFieldName name <> ": " <> columnTypeToTypeScript columnType notNull <> ";"

-- | Generates a record interface where fields with default values are optional
newRecordInterface :: CreateTable -> Text
newRecordInterface CreateTable { name, columns } = [trimming|
        /**
         * ${description}
         */
        interface New${modelName} {
            ${fields}
        }
    |]
    where
        fields = columns
                |> map columnToField
                |> intercalate "\n"
        columnToField Column { name, columnType, notNull, defaultValue } = columnNameToFieldName name <> (if isJust defaultValue then "?" else "") <> ": " <> columnTypeToTypeScript columnType notNull <> ";"

        modelName :: Text
        modelName = tableNameToModelName name

        description :: Text
        description = "A " <> modelName <> " object not yet inserted into the `" <> name <> "` table"

columnTypeToTypeScript :: PostgresType -> Bool -> Text
columnTypeToTypeScript sqlType notNull =
    if notNull
        then columnTypeToTypeScript' sqlType
        else columnTypeToTypeScript' sqlType <> " | null"

columnTypeToTypeScript' :: PostgresType -> Text
columnTypeToTypeScript' PText = "string"
columnTypeToTypeScript' PInt = "number"
columnTypeToTypeScript' PSmallInt = "number"
columnTypeToTypeScript' PDouble = "number"
columnTypeToTypeScript' PBoolean = "boolean"
columnTypeToTypeScript' PUUID = "UUID"
columnTypeToTypeScript' (PCustomType customType) = tableNameToModelName customType
columnTypeToTypeScript' (PArray inner) = "Array<" <> columnTypeToTypeScript' inner <> ">"
columnTypeToTypeScript' otherwise = "string"

initThinBackendTypeDef :: Text
initThinBackendTypeDef = [trimming|
    function initThinBackend(options: { host: string | undefined; }): void;
|]



packageJsonContent :: Text
packageJsonContent = [trimming|
{
  "name": "@types/ihp-datasync",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "",
  "license": "ISC",
  "types": "main.d.ts"
}
|]

generateEnumType :: Text -> [Text] -> Text
generateEnumType _ [] = ""
generateEnumType name values = "type " <> tableNameToModelName name <> " = " <> (intercalate " | " (map compileValue values)) <> ";"
    where
        compileValue :: Text -> Text
        compileValue value = "'" <> value <> "'"
