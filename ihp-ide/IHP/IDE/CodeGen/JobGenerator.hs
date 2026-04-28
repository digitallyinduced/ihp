module IHP.IDE.CodeGen.JobGenerator (buildPlan, buildPlan', JobConfig (..)) where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.IDE.CodeGen.Types
import qualified System.Directory.OsPath as Directory

data JobConfig = JobConfig
    { applicationName :: Text
    , tableName :: Text -- E.g. create_container_jobs
    , modelName :: Text -- E.g. CreateContainerJob
    , isFirstJobInApplication :: Bool -- If true, creates Worker.hs in application directory
    , isFirstJobInProject :: Bool -- If true, creates WorkerMain.hs at the project root
    , uuidFunction :: Text -- E.g. "uuid_generate_v4" or "uuidv7"
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan jobName applicationName = do
    let workerPath = textToOsPath (applicationName <> "/Worker.hs")
    let rootWorkerPath = textToOsPath "WorkerMain.hs"
    isFirstJobInApplication <- not <$> Directory.doesFileExist workerPath
    isFirstJobInProject <- not <$> Directory.doesFileExist rootWorkerPath
    uuidFunction <- defaultUuidFunction
    if null jobName
        then pure $ Left "Job name cannot be empty"
        else do
            let jobConfig = JobConfig
                    { applicationName
                    , tableName = jobName
                    , modelName = tableNameToModelName jobName
                    , isFirstJobInApplication
                    , isFirstJobInProject
                    , uuidFunction
                    }
            pure $ Right $ buildPlan' jobConfig

-- E.g. qualifiedJobModuleName config == "Web.Job.CreateContainer"
qualifiedJobModuleName :: JobConfig -> Text
qualifiedJobModuleName config =
    config.applicationName <> ".Job." <> unqualifiedJobModuleName config

unqualifiedJobModuleName :: JobConfig -> Text
unqualifiedJobModuleName config = Text.replace "Job" "" (config.modelName)

buildPlan' :: JobConfig -> [GeneratorAction]
buildPlan' config =
        let
            name = config.modelName
            tableName = modelNameToTableName nameWithSuffix
            (nameWithSuffix, nameWithoutSuffix) = ensureSuffix "Job" name

            job =
                ""
                <> "module " <> qualifiedJobModuleName config <> " where\n"
                <> "import " <> config.applicationName <> ".Controller.Prelude\n"
                <> "\n"
                <> "instance Job " <> nameWithSuffix <> " where\n"
                <> "    perform " <> nameWithSuffix <> " { .. } = do\n"
                <> "        putStrLn \"Hello World!\"\n"

            schemaSql =
                ""
                <> "CREATE TABLE " <> tableName <> " (\n"
                <> "    id UUID DEFAULT " <> config.uuidFunction <> "() PRIMARY KEY NOT NULL,\n"
                <> "    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n"
                <> "    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n"
                <> "    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n"
                <> "    last_error TEXT DEFAULT NULL,\n"
                <> "    attempts_count INT DEFAULT 0 NOT NULL,\n"
                <> "    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n"
                <> "    locked_by UUID DEFAULT NULL,\n"
                <> "    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n"
                <> ");\n"


            emptyWorkerHs :: Text
            emptyWorkerHs =
                        let
                            applicationName = config.applicationName
                        in cs [plain|module #{applicationName}.Worker where

import IHP.Prelude
import #{applicationName}.Types
import Generated.Types
import IHP.Job.Runner
import IHP.Job.Types

import #{qualifiedJobModuleName config}

instance Worker #{applicationName}Application where
    workers _ =
        [ worker @#{nameWithSuffix}
        -- Generator Marker
        ]
|]

            -- Composed Worker instance for RootApplication. Lives in
            -- WorkerMain.hs at the project root (parallel to Main.hs) so that:
            --   * Main.hs no longer needs to import Web.Worker (which would
            --     transitively pull every Job module into Main's dep graph).
            --   * The dev-mode worker GHCi can load only this file (via
            --     build/RunJobs.hs) without dragging in Web.FrontController
            --     and the Controller / View tree.
            workerMainHs :: Text
            workerMainHs =
                        let
                            applicationName = config.applicationName
                        in cs [plain|module WorkerMain () where

import IHP.Prelude
import IHP.FrameworkConfig (RootApplication (..))
import IHP.Job.Runner (Worker (..))
import #{applicationName}.Types (#{applicationName}Application (..))
import #{applicationName}.Worker ()

instance Worker RootApplication where
    workers _ = workers #{applicationName}Application
|]
        in
            [ EnsureDirectory { directory = textToOsPath (config.applicationName <> "/Job") }
            , AppendToFile { filePath = "Application/Schema.sql", fileContent = schemaSql }
            , CreateFile { filePath = textToOsPath (config.applicationName <> "/Job/" <> nameWithoutSuffix <> ".hs"), fileContent = job }
            ]
            <> (if config.isFirstJobInApplication
                    then [ CreateFile { filePath = textToOsPath (config.applicationName <> "/Worker.hs"), fileContent = emptyWorkerHs } ]
                    else
                        [ AddImport { filePath = textToOsPath (config.applicationName <> "/Worker.hs"), fileContent = "import " <> qualifiedJobModuleName config }
                        , AppendToMarker { marker = "-- Generator Marker", filePath = textToOsPath (config.applicationName <> "/Worker.hs"), fileContent = "        , worker @" <> nameWithSuffix }
                        ])
            <> (if config.isFirstJobInProject
                    then [ CreateFile { filePath = textToOsPath "WorkerMain.hs", fileContent = workerMainHs } ]
                    else [])
