module IHP.IDE.CodeGen.Types where

import IHP.Prelude

data GeneratorAction
    = CreateFile { filePath :: Text, fileContent :: Text }
    | AppendToFile { filePath :: Text, fileContent :: Text }
    | AppendToMarker { marker :: Text, filePath :: Text, fileContent :: Text }
    | AddImport { filePath :: Text, fileContent :: Text }
    | AddAction { filePath :: Text, fileContent :: Text }
    | EnsureDirectory { directory :: Text }
    | RunShellCommand { shellCommand :: Text }
    deriving (Show, Eq)