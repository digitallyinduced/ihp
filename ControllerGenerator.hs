{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Foundation.ControllerGenerator where

import ClassyPrelude
import Foundation.NameSupport
import Model.Schema
import Foundation.SchemaSupport
import Data.String.Conversions (cs)
import Data.Text.IO (appendFile)

main :: IO ()
main = do
    args <- getArgs
    main' args

main' :: [Text] -> IO ()
main' args = do
    case headMay args of
        Just controllerName' -> do
            let controllerName = normalizeName controllerName'
            let generate =
                    [ CreateFile { filePath = "src/Apps/Web/Controller/" <> controllerName <> ".hs", fileContent = (generateController controllerName) }
                    , AppendToFile { filePath = "src/Apps/Web/Routes.hs", fileContent = (controllerInstance controllerName) }
                    , AppendToFile { filePath = "src/Apps/Web/Types.hs", fileContent = (generateControllerData controllerName) }
                    , AppendImportToFile { filePath = "src/Apps/Web/App.hs", fileContent = ("import Apps.Web.Controller." <> controllerName) }
                    ]
            evalActions generate
        Nothing -> usage





usage :: IO ()
usage = putStrLn "Usage: gen/controller RESOURCE_NAME"

controllerInstance :: Text -> Text
controllerInstance name = "instance RestfulController " <> name <> "Controller\n"

data GeneratorAction
    = CreateFile { filePath :: Text, fileContent :: Text }
    | AppendToFile { filePath :: Text, fileContent :: Text }
    | AppendImportToFile { filePath :: Text, fileContent :: Text }
    deriving (Show, Eq)

evalActions :: [GeneratorAction] -> IO ()
evalActions actions = forM_ actions evalAction
    where
        evalAction CreateFile { filePath, fileContent } = do
            writeFile (cs filePath) (cs fileContent)
            putStrLn ("+ " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            appendFile (cs filePath) fileContent
            putStrLn ("* " <> filePath)
        evalAction AppendImportToFile { filePath, fileContent } = do
            appendFile (cs filePath) fileContent
            putStrLn ("* " <> filePath <> " (import)")

describePlan :: [GeneratorAction] -> Text
describePlan actions = intercalate "\n" (map describePlan' actions)

describePlan' :: GeneratorAction -> Text
describePlan' CreateFile { filePath, fileContent } = "CREATE " <> filePath
describePlan' AppendToFile { filePath, fileContent } = "APPEND " <> filePath <> ": " <> fileContent
describePlan' AppendImportToFile { filePath, fileContent } = "IMPORT " <> filePath <> ": " <> fileContent

getTable :: Text -> Maybe Table
getTable name = find (\(Table n _) -> n == name) database

fieldsForTable :: Text -> [Text]
fieldsForTable name =
    case getTable name of
        Just (Table _ attributes) -> map (\(Field name _) -> columnNameToFieldName name) (fieldsWithoutDefaultValue $ fieldsOnly attributes)
        Nothing -> []

normalizeName name = ucfirst name


generateControllerData :: Text -> Text
generateControllerData name' =
    let
        name = normalizeName name'
        singularName = pluralToSingular name
        idFieldName = lcfirst singularName <> "Id"
        idType = singularName <> "Id"
    in 
        "\n"
        <> "data " <> name <> "Controller\n"
        <> "    = " <> name <> "Action\n"
        <> "    | New" <> singularName <> "Action\n"
        <> "    | Show" <> singularName <> "Action { " <> idFieldName <> " :: " <> idType <> " }\n"
        <> "    | Create" <> singularName <> "Action\n"
        <> "    | Delete" <> singularName <> "Action { " <> idFieldName <> " :: " <> idType <> " }\n"
        <> "    deriving (Eq, Show, Generic, Data)\n"

generateController :: Text -> Text
generateController name' =
    let
        name = normalizeName name'
        singularName = pluralToSingular name
        moduleName = "Apps.Web.Controller." <> name
        controllerName = name <> "Controller"

        importStatements =
            [ "import Foundation.ControllerPrelude"
            , "import Apps.Web.Types"
            , "import qualified Apps.Web.View." <> name <> ".Index as Index"
            , "import qualified Apps.Web.View." <> name <> ".New as New"
            , "import qualified Apps.Web.View." <> name <> ".Edit as Edit"
            , "import qualified Apps.Web.View." <> name <> ".Show as Show"
            , "import qualified Model." <> singularName <> " as " <> singularName

            ]

        modelVariablePlural = lcfirst name
        modelVariableSingular = lcfirst singularName
        idFieldName = lcfirst singularName <> "Id"
        model = ucfirst singularName
        indexAction =
            ""
            <> "    action " <> name <> "Action = do\n"
            <> "        " <> modelVariablePlural <> " <- query @" <> model <> " |> fetch\n"
            <> "        renderHtml $ Index.render " <> modelVariablePlural <> "\n"

        newAction =
            ""
            <> "    action New" <> singularName <> "Action = do\n"
            <> "        let " <> modelVariableSingular <> " :: New" <> model <> " = " <> model <> ".build {\n"
            <> (intercalate ",\n" (map (\f -> "                " <> f <> " = def" ) modelFields)) <> "\n"
            <> "            }\n"
            <> "        renderHtml $ New.render " <> modelVariableSingular <> "\n"

        showAction =
            ""
            <> "    action Show" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        renderHtml $ Show.render " <> modelVariableSingular <> "\n"

        editAction =
            ""
            <> "    action Edit" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        renderHtml $ Edit.render " <> modelVariableSingular <> "\n"

        modelFields :: [Text]
        modelFields = fieldsForTable modelVariablePlural

        updateAction =
            ""
            <> "    action Update" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> "' <- fetch " <> idFieldName <> "\n"
            <> "        let " <> modelVariableSingular <> " :: " <> model <> " = " <> model <> ".readParams (" <> model <> ".const " <> modelVariableSingular <> "') {\n"
            <> (intercalate ",\n" (map (\f -> "                " <> f <> " = param" ) modelFields)) <> "\n"
            <> "            }\n"
            <> "        isValid <- validateRecord2 " <> modelVariableSingular <> "\n"
            <> "        case isValid of\n"
            <> "            Left errors -> renderHtml $ Edit.render " <> modelVariableSingular <> "\n"
            <> "            Right " <> modelVariableSingular <> " -> do\n"
            <> "                " <> modelVariableSingular <> " <- createRecord " <> modelVariableSingular <> "\n"
            <> "                setSuccessMessage \"" <> model <> " updated\"\n"
            <> "                redirectTo Edit" <> singularName <> "Action { .. }\n"

        createAction =
            ""
            <> "    action Create" <> singularName <> "Action = do\n"
            <> "        let " <> modelVariableSingular <> " :: New" <> model <> " = " <> model <> ".readParams " <> model <> ".buildConst {\n"
            <> (intercalate ",\n" (map (\f -> "                " <> f <> " = param" ) modelFields)) <> "\n"
            <> "            }\n"
            <> "        isValid <- validateRecord2 " <> modelVariableSingular <> "\n"
            <> "        case isValid of\n"
            <> "            Left errors -> renderHtml $ New.render " <> modelVariableSingular <> "\n"
            <> "            Right " <> modelVariableSingular <> " -> do\n"
            <> "                " <> modelVariableSingular <> " <- updateRecord " <> modelVariableSingular <> "\n"
            <> "                setSuccessMessage \"" <> model <> " created\"\n"
            <> "                redirectTo " <> name <> "Action\n"

        deleteAction =
            ""
            <> "    action Delete" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        deleteRecord " <> modelVariableSingular <> "\n"
            <> "        setSuccessMessage \"" <> model <> " deleted\"\n"
            <> "        redirectTo " <> name <> "Action\n"
    in
        ""
        <> "module " <> moduleName <> " where" <> "\n"
        <> "\n"
        <> intercalate "\n" importStatements
        <> "\n\n"
        <> "instance Controller " <> controllerName <> " where\n"
        <> indexAction
        <> "\n"
        <> newAction
        <> "\n"
        <> showAction
        <> "\n"
        <> editAction
        <> "\n"
        <> updateAction
        <> "\n"
        <> createAction
        <> "\n"
        <> deleteAction