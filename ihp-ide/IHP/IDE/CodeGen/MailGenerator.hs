module IHP.IDE.CodeGen.MailGenerator (buildPlan, buildPlan', MailConfig (..)) where

import IHP.Prelude
import IHP.IDE.CodeGen.Types
import IHP.Postgres.Types
import Text.Countable (pluralize)

data MailConfig = MailConfig
    { controllerName :: Text
    , applicationName :: Text
    , modelName :: Text
    , mailName :: Text
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan mailName applicationName controllerName' =
    if (null mailName || null controllerName')
        then pure $ Left "Neither mail name nor controller name can be empty"
        else do
            schema <- loadAppSchema
            let modelName = tableNameToModelName controllerName'
            let controllerName = tableNameToControllerName controllerName'
            let viewConfig = MailConfig { .. }
            pure $ Right $ buildPlan' schema viewConfig

-- E.g. qualifiedMailModuleName config "Confirmation" == "Web.Mail.Users.Confirmation"
qualifiedMailModuleName :: MailConfig -> Text -> Text
qualifiedMailModuleName config mailName =
    qualifiedModuleName config.applicationName "Mail" config.controllerName (ucfirst mailName)

buildPlan' :: [Statement] -> MailConfig -> [GeneratorAction]
buildPlan' schema config =
        let
            controllerName = config.controllerName
            name = config.mailName
            singularName = config.modelName
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst controllerName
            (nameWithSuffix, nameWithoutSuffix) = ensureSuffix "Mail" name

            indexAction = pluralize singularName <> "Action"

            modelFields :: [Text]
            modelFields = [ modelNameToTableName pluralVariableName, pluralVariableName ]
                    |> mapMaybe (fieldsForTable schema)
                    |> head
                    |> fromMaybe []

            mail =
                ""
                <> "module " <> qualifiedMailModuleName config nameWithoutSuffix <> " where\n"
                <> "import " <> config.applicationName <> ".View.Prelude\n"
                <> "import IHP.MailPrelude\n"
                <> "\n"
                <> "data " <> nameWithSuffix <> " = " <> nameWithSuffix <> " { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance BuildMail " <> nameWithSuffix <> " where\n"
                <> "    subject = \"Subject\"\n"
                <> "    to " <> nameWithSuffix <> " { .. } = Address { addressName = Just \"Firstname Lastname\", addressEmail = \"fname.lname@example.com\" }\n"
                <> "    from = \"hi@example.com\"\n"
                <> "    html " <> nameWithSuffix <> " { .. } = [hsx|\n"
                <> "        Hello World\n"
                <> "    |]\n"
        in
            [ EnsureDirectory { directory = textToOsPath (config.applicationName <> "/Mail/" <> controllerName) }
            , CreateFile { filePath = textToOsPath (config.applicationName <> "/Mail/" <> controllerName <> "/" <> nameWithoutSuffix <> ".hs"), fileContent = mail }
            , AddImport { filePath = textToOsPath (config.applicationName <> "/Controller/" <> controllerName <> ".hs"), fileContent = "import " <> qualifiedMailModuleName config nameWithoutSuffix }
            ]
