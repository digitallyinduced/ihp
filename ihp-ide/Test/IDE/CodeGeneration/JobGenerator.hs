{-|
Module: IDE.CodeGeneration.JobGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module IDE.CodeGeneration.JobGenerator where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.JobGenerator as JobGenerator
import IHP.IDE.CodeGen.Types

tests = do
    describe "Job Generator" do
        it "should build a job with name \"CreateContainerJobs\"" do
            let applicationName = "Web"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = False
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , AddImport {filePath = "Web/Worker.hs", fileContent = "import Web.Job.CreateContainer"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/Worker.hs", fileContent = "        , worker @CreateContainerJob"}
                ]

        it "should build a job with suffix Job even when it's missing in the input" do
            let applicationName = "Web"
            let tableName = "create_container"
            let modelName = "CreateContainer"
            let isFirstJobInApplication = False
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , AddImport {filePath = "Web/Worker.hs", fileContent = "import Web.Job.CreateContainer"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/Worker.hs", fileContent = "        , worker @CreateContainerJob"}
                ]


        it "should create Web/Worker.hs if it doesn't exist" do
            let applicationName = "Web"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = True
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Web/Worker.hs", fileContent = "module Web.Worker where\n\nimport IHP.Prelude\nimport Web.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Web.Job.CreateContainer\n\ninstance Worker WebApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Web.Types (WebApplication (..))"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Web.Worker ()"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "WorkerMain.hs", fileContent = "        ++ workers WebApplication"}
                ]


        it "should support other applications" do
            let applicationName = "Admin"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = False
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Admin/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Admin/Job/CreateContainer.hs", fileContent = "module Admin.Job.CreateContainer where\nimport Admin.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , AddImport {filePath = "Admin/Worker.hs", fileContent = "import Admin.Job.CreateContainer"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Admin/Worker.hs", fileContent = "        , worker @CreateContainerJob"}
                ]

        it "should create appropriate Worker.hs for other applications if it doesn't exist" do
            let applicationName = "Admin"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = True
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Admin/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Admin/Job/CreateContainer.hs", fileContent = "module Admin.Job.CreateContainer where\nimport Admin.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Admin/Worker.hs", fileContent = "module Admin.Worker where\n\nimport IHP.Prelude\nimport Admin.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Admin.Job.CreateContainer\n\ninstance Worker AdminApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Admin.Types (AdminApplication (..))"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Admin.Worker ()"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "WorkerMain.hs", fileContent = "        ++ workers AdminApplication"}
                ]

        it "should create WorkerMain.hs on the very first job in the project" do
            let applicationName = "Web"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = True
            let isFirstJobInProject = True
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Web/Worker.hs", fileContent = "module Web.Worker where\n\nimport IHP.Prelude\nimport Web.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Web.Job.CreateContainer\n\ninstance Worker WebApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                , CreateFile {filePath = "WorkerMain.hs", fileContent = "module WorkerMain () where\n\nimport IHP.Prelude\nimport IHP.FrameworkConfig (RootApplication (..))\nimport IHP.Job.Types (Worker (..))\nimport Web.Types (WebApplication (..))\nimport Web.Worker ()\n\ninstance Worker RootApplication where\n    workers _ =\n        workers WebApplication\n        -- Generator Marker\n"}
                ]

        it "should amend WorkerMain.hs when a second application gains its first job" do
            -- WorkerMain.hs already exists (created by the project's first job
            -- in another application), so instead of re-creating it we compose
            -- this application's workers into the root instance via the marker.
            let applicationName = "Api"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = True
            let isFirstJobInProject = False
            let uuidFunction = "uuid_generate_v4"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Api/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Api/Job/CreateContainer.hs", fileContent = "module Api.Job.CreateContainer where\nimport Api.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Api/Worker.hs", fileContent = "module Api.Worker where\n\nimport IHP.Prelude\nimport Api.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Api.Job.CreateContainer\n\ninstance Worker ApiApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Api.Types (ApiApplication (..))"}
                , AddImport {filePath = "WorkerMain.hs", fileContent = "import Api.Worker ()"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "WorkerMain.hs", fileContent = "        ++ workers ApiApplication"}
                ]
