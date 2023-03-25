module IHP.IDE.CodeGen.ScriptGenerator (buildPlan) where

import IHP.Prelude
import IHP.IDE.CodeGen.Types

buildPlan :: Text -> Either Text [GeneratorAction]
buildPlan scriptName =
    if null scriptName
        then Left "Script name cannot be empty"
        else do 
            let filePath = "Application/Script/" <> cs scriptName <> ".hs"
            let fileContent = renderScript scriptName
            Right [ CreateFile { filePath, fileContent }
                  , RunShellCommand { shellCommand = "chmod +x " <> filePath }
                  ]

renderScript :: Text -> Text
renderScript scriptName' = cs [plain|#!/usr/bin/env run-script
module Application.Script.#{taskName} where

import Application.Script.Prelude

run :: Script
run = do
    putStrLn "Hello World!"
|]
    where taskName :: String = cs scriptName'