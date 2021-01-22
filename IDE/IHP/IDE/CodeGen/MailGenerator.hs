module IHP.IDE.CodeGen.MailGenerator (buildPlan, buildPlan', MailConfig (..)) where

import IHP.Prelude
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ViewSupport
import qualified System.Process as Process
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import qualified Text.Countable as Countable

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
            schema <- SchemaDesigner.parseSchemaSql >>= \case
                Left parserError -> pure []
                Right statements -> pure statements
            let modelName = tableNameToModelName controllerName'
            let controllerName = tableNameToControllerName controllerName'
            let viewConfig = MailConfig { .. }
            pure $ Right $ buildPlan' schema viewConfig

-- E.g. qualifiedMailModuleName config "Confirmation" == "Web.Mail.Users.Confirmation"
qualifiedViewModuleName :: MailConfig -> Text -> Text
qualifiedViewModuleName config mailName =
    get #applicationName config <> ".Mail." <> get #controllerName config <> "." <> ucfirst mailName

buildPlan' :: [Statement] -> MailConfig -> [GeneratorAction]
buildPlan' schema config =
        let
            controllerName = get #controllerName config
            name = get #mailName config
            singularName = config |> get #modelName
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst controllerName
            nameWithSuffix = if "Mail" `isSuffixOf` name
                then name
                else name <> "Mail" --e.g. "Test" -> "TestMail"
            nameWithoutSuffix = if "Mail" `isSuffixOf` name
                then Text.replace "Mail" "" name
                else name --e.g. "TestMail" -> "Test"

            indexAction = Countable.pluralize singularName <> "Action"

            modelFields :: [Text]
            modelFields = fieldsForTable schema (modelNameToTableName pluralVariableName)

            mail =
                ""
                <> "module " <> qualifiedViewModuleName config nameWithoutSuffix <> " where\n"
                <> "import " <> get #applicationName config <> ".View.Prelude\n"
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
            [ EnsureDirectory { directory = get #applicationName config <> "/Mail/" <> controllerName }
            , CreateFile { filePath = get #applicationName config <> "/Mail/" <> controllerName <> "/" <> nameWithoutSuffix <> ".hs", fileContent = mail }
            , AddImport { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = "import " <> qualifiedViewModuleName config nameWithoutSuffix }
            ]
