module IHP.IDE.CodeGen.Types where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types

data GeneratorAction
    = CreateFile { filePath :: Text, fileContent :: Text }
    | AppendToFile { filePath :: Text, fileContent :: Text }
    | AppendToMarker { marker :: Text, filePath :: Text, fileContent :: Text }
    | AddImport { filePath :: Text, fileContent :: Text }
    | AddAction { filePath :: Text, fileContent :: Text }
    | AddToDataConstructor { dataConstructor :: Text, filePath :: Text, fileContent :: Text }
    | AddMountToFrontController { filePath :: Text, applicationName :: Text }
    | EnsureDirectory { directory :: Text }
    | RunShellCommand { shellCommand :: Text }
    deriving (Show, Eq)



fieldsForTable :: [Statement] -> Text -> [Text]
fieldsForTable database name =
    case getTable database name of
        Just (CreateTable { columns }) -> columns
                |> filter columnRelevantForCreateOrEdit
                |> map (get #name)
                |> map columnNameToFieldName
        _ -> []

-- | Returns True when a column should be part of the generated controller or forms
--
-- Returrns @False@ for primary keys, or fields such as @created_at@
columnRelevantForCreateOrEdit :: Column -> Bool
columnRelevantForCreateOrEdit column
    | (get #columnType column == PTimestamp || get #columnType column == PTimestampWithTimezone)
    && (isJust (get #defaultValue column))
    = False
columnRelevantForCreateOrEdit column = not (get #primaryKey column)

getTable :: [Statement] -> Text -> Maybe Statement
getTable schema name = find isTable schema
    where
        isTable :: Statement -> Bool
        isTable table@(CreateTable { name = name' }) | name == name' = True
        isTable _ = False

