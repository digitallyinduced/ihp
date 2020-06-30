module IHP.IDE.CodeGen.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.CodeGen.View.Generators
import IHP.IDE.CodeGen.View.NewController
import IHP.IDE.CodeGen.View.NewScript
import IHP.IDE.CodeGen.View.NewView
import IHP.IDE.CodeGen.View.NewAction
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.IDE.CodeGen.ScriptGenerator as ScriptGenerator
import IHP.IDE.CodeGen.ViewGenerator as ViewGenerator
import IHP.IDE.CodeGen.ActionGenerator as ActionGenerator
import IHP.IDE.ToolServer.Helper.Controller
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Inflections as Inflector
import Control.Exception
import System.Directory


instance Controller CodeGenController where
    action GeneratorsAction = do
        render GeneratorsView

    action NewControllerAction = do
        let controllerName = paramOrDefault "" "name"
        plan <- ControllerGenerator.buildPlan controllerName
        render NewControllerView { .. }

    action CreateControllerAction = do
        let controllerName = param "name"
        (Right plan) <- ControllerGenerator.buildPlan controllerName
        executePlan plan
        setSuccessMessage "Controller generated"
        redirectTo GeneratorsAction

    action NewScriptAction = do
        let scriptName = paramOrDefault "" "name"
        let plan = ScriptGenerator.buildPlan scriptName
        render NewScriptView { .. }

    action CreateScriptAction = do
        let scriptName = paramOrDefault "" "name"
        let (Right plan) = ScriptGenerator.buildPlan scriptName
        executePlan plan
        setSuccessMessage "Script generated"
        redirectTo GeneratorsAction

    action NewViewAction = do
        let viewName = paramOrDefault "" "name"
        let applicationName = "Web"
        let controllerName = paramOrDefault "" "controllerName"
        controllers <- listOfWebControllers
        plan <- ViewGenerator.buildPlan viewName applicationName controllerName
        render NewViewView { .. }

    action CreateViewAction = do
        let viewName = paramOrDefault "" "name"
        let applicationName = "Web"
        let controllerName = paramOrDefault "" "controllerName"
        (Right plan) <- ViewGenerator.buildPlan viewName applicationName controllerName
        executePlan plan
        setSuccessMessage "View generated"
        redirectTo GeneratorsAction

    action NewActionAction = do
        let actionName = paramOrDefault "" "name"
        let applicationName = "Web"
        let controllerName = paramOrDefault "" "controllerName"
        controllers <- listOfWebControllers
        plan <- ActionGenerator.buildPlan actionName applicationName controllerName
        render NewActionView { .. }

    action CreateActionAction = do
        let actionName = paramOrDefault "" "name"
        let applicationName = "Web"
        let controllerName = paramOrDefault "" "controllerName"
        (Right plan) <- ActionGenerator.buildPlan actionName applicationName controllerName
        executePlan plan
        setSuccessMessage "Action generated"
        redirectTo GeneratorsAction

    action OpenControllerAction = do
        let name = param "name"
        case name |> Inflector.toCamelCased True of
            Left error -> renderPlain "Failed to transform name to camel case"
            Right indexActionName-> redirectToUrl ("http://localhost:" <> tshow appPort <> "/" <> indexActionName)


executePlan :: [GeneratorAction] -> IO ()
executePlan actions = forEach actions evalAction
    where
        evalAction CreateFile { filePath, fileContent } = do
            Text.writeFile (cs filePath) (cs fileContent)
            putStrLn ("+ " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            Text.appendFile (cs filePath) fileContent
            putStrLn ("* " <> filePath)
        evalAction AppendToMarker { marker, filePath, fileContent } = do
            content <- Text.readFile (cs filePath)
            let newContent = Text.replace marker (marker <> "\n" <> cs fileContent) (cs content)
            Text.writeFile (cs filePath) (cs newContent)
            putStrLn ("* " <> filePath <> " (import)")
        evalAction AddImport { filePath, fileContent } = do
            addImport filePath [fileContent]
            putStrLn ("* " <> filePath <> " (import)")
        evalAction AddAction { filePath, fileContent } = do
            addAction filePath [fileContent]
            putStrLn ("* " <> filePath <> " (import)")
        evalAction AddToDataConstructor { dataConstructor, filePath, fileContent } = do
            content <- Text.readFile (cs filePath)
            case addToDataConstructor content dataConstructor fileContent of
                Just newContent -> do
                    Text.writeFile (cs filePath) (cs newContent)
                    putStrLn ("* " <> filePath <> " (AddToDataConstructor)")
                Nothing -> putStrLn ("Could not automatically add " <> tshow content <> " to " <> filePath)
        evalAction EnsureDirectory { directory } = do
            Directory.createDirectoryIfMissing True (cs directory)
        evalAction RunShellCommand { shellCommand } = do
            _ <- Process.system (cs shellCommand)
            putStrLn ("* " <> shellCommand)

undoPlan :: [GeneratorAction] -> IO()
undoPlan actions = forEach actions evalAction
    where
        evalAction CreateFile { filePath, fileContent } = do
            (Directory.removeFile (cs filePath)) `catch` handleError
            putStrLn ("- " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            deleteTextFromFile (cs filePath) fileContent `catch` handleError
            putStrLn ("* " <> filePath)
        evalAction AppendToMarker { marker, filePath, fileContent } = do
            (deleteTextFromFile (cs filePath) (fileContent <> "\n")) `catch` handleError
            putStrLn ("* " <> filePath <> " (import)")
        evalAction AddImport { filePath, fileContent } = do
            (deleteTextFromFile (cs filePath) (fileContent <> "\n")) `catch` handleError
            putStrLn ("* " <> filePath <> " (import)")
        evalAction AddToDataConstructor { dataConstructor, filePath, fileContent } = do
            (deleteTextFromFile (cs filePath) (fileContent <> "\n")) `catch` handleError
            putStrLn ("* " <> filePath <> " (RemoveFromDataConstructor)")
        evalAction EnsureDirectory { directory } = do
            (Directory.removeDirectory (cs directory)) `catch` handleError
        evalAction RunShellCommand { shellCommand } = pure ()
        handleError :: SomeException -> IO ()
        handleError ex = putStrLn (tshow ex)

deleteTextFromFile :: Text -> Text -> IO ()
deleteTextFromFile filePath lineContent = do
    fileContent <- Text.readFile (cs filePath)
    let replacedContent = Text.replace lineContent "" fileContent
    Text.writeFile (cs filePath) replacedContent

addImport :: Text -> [Text] -> IO ()
addImport file importStatements = do
    content :: Text <- Text.readFile (cs file)
    case addImport' content importStatements of
        Just newContent -> Text.writeFile (cs file) (cs newContent)
        Nothing -> putStrLn ("Could not automatically add " <> tshow importStatements <> " to " <> file)
    pure ()

addImport' :: Text -> [Text] -> Maybe Text
addImport' file = appendLineAfter file ("import" `isPrefixOf`)

addAction :: Text -> [Text] -> IO ()
addAction filePath fileContent = do
    content <- Text.readFile (cs filePath)
    case addAction' content fileContent of
        Just newContent -> Text.writeFile (cs filePath) (cs newContent)
        Nothing -> putStrLn ("Could not automatically add " <> tshow content <> " to " <> filePath)
    pure ()

addAction' :: Text -> [Text] -> Maybe Text
addAction' fileContent = appendLineAfter fileContent ("instance Controller" `isPrefixOf`)

-- | Gets content of a Types.hs, a existent data constructor and a type which should be added to it
--   and returns fileContent with the type in it.
addToDataConstructor :: Text -> Text -> Text -> Maybe Text
addToDataConstructor fileContent dataConstructor content = do
    lineOfDataConstructor <- lines fileContent
        |> zip [1..]
        |> filter (\(n, line) -> dataConstructor `isInfixOf` line)
        |> lastMay
        |> fmap fst
    lineOfDerivingStatement <- ((drop lineOfDataConstructor $ lines fileContent) :: [Text])
        |> zip [lineOfDataConstructor..]
        |> filter (\(n, line) -> "deriving" `isInfixOf` line)
        |> headMay
        |> fmap fst
    Just $ unlines $ ((take lineOfDerivingStatement $ lines fileContent) <> [content] <> (drop lineOfDerivingStatement $ lines fileContent)) 

appendLineAfter :: Text -> (Text -> Bool) -> [Text] -> Maybe Text
appendLineAfter file isRelevantLine newLines =
    let content :: [Text] = lines file
        lastImportLine = content
            |> zip [1..]
            |> filter (\(n, line) -> isRelevantLine line)
            |> lastMay
            |> fmap fst
    in fmap (\lastImportLine -> unlines $ (take lastImportLine content) <> newLines <> (drop lastImportLine content)) lastImportLine

listOfWebControllers :: IO [Text]
listOfWebControllers = do
    directoryFiles <-  listDirectory "Web/Controller"
    let controllerFiles :: [Text] =  filter (\x -> not $ "Prelude" `isInfixOf` x || "Context" `isInfixOf` x)  $ map cs directoryFiles
    pure $ map (Text.replace ".hs" "") controllerFiles
