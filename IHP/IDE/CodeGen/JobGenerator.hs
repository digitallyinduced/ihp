module IHP.IDE.CodeGen.JobGenerator (buildPlan, buildPlan', JobConfig (..)) where

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

data JobConfig = JobConfig
    { applicationName :: Text
    , tableName :: Text -- | E.g. create_container_jobs
    , modelName :: Text -- | E.g. CreateContainerJob
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan jobName applicationName =
    if null jobName
        then pure $ Left "Job name can be empty"
        else do
            let jobConfig = JobConfig
                    { applicationName
                    , tableName = jobName
                    , modelName = tableNameToModelName jobName
                    }
            pure $ Right $ buildPlan' jobConfig

-- E.g. qualifiedMailModuleName config "Confirmation" == "Web.Mail.Users.Confirmation"
qualifiedJobModuleName :: JobConfig -> Text
qualifiedJobModuleName config =
    get #applicationName config <> ".Job." <> unqualifiedJobModuleName config

unqualifiedJobModuleName :: JobConfig -> Text
unqualifiedJobModuleName config = Text.replace "Job" "" (get #modelName config)

buildPlan' :: JobConfig -> [GeneratorAction]
buildPlan' config =
        let
            name = get #modelName config
            tableName = modelNameToTableName name
            nameWithSuffix = if "Job" `isSuffixOf` name
                then name
                else name <> "Job" --e.g. "Test" -> "TestJob"
            nameWithoutSuffix = if "Job" `isSuffixOf` name
                then Text.replace "Job" "" name
                else name --e.g. "TestJob" -> "Test""

            job =
                ""
                <> "module " <> qualifiedJobModuleName config <> " where\n"
                <> "import " <> get #applicationName config <> ".Controller.Prelude\n"
                <> "\n"
                <> "instance Job " <> nameWithSuffix <> " where\n"
                <> "    perform " <> name <> " { .. } = do\n"
                <> "        putStrLn \"Hello World!\"\n"

            schemaSql =
                ""
                <> "CREATE TABLE " <> tableName <> " (\n"
                <> "    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n"
                <> "    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n"
                <> "    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n"
                <> "    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n"
                <> "    last_error TEXT DEFAULT NULL,\n"
                <> "    attempts_count INT DEFAULT 0 NOT NULL,\n"
                <> "    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n"
                <> "    locked_by UUID DEFAULT NULL\n"
                <> ");\n"
        in
            [ EnsureDirectory { directory = get #applicationName config <> "/Job" }
            , CreateFile { filePath = get #applicationName config <> "/Job/" <> nameWithoutSuffix <> ".hs", fileContent = job }
            , AppendToFile { filePath = "Application/Schema.sql", fileContent = schemaSql }
            , AppendToMarker { marker = "-- Job Imports", filePath = get #applicationName config <> "/Worker.hs", fileContent = ("import " <> qualifiedJobModuleName config) }
            --, AddImport { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = "import " <> qualifiedViewModuleName config nameWithoutSuffix }
            ]
