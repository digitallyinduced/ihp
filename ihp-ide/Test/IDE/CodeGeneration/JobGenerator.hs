{-|
Module: Test.IDE.CodeGeneration.JobGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.CodeGeneration.JobGenerator where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.JobGenerator as JobGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport
import qualified System.Directory as Directory

tests = do
    describe "Job Generator" do
        it "should build a job with name \"CreateContainerJobs\"" do
            let applicationName = "Web"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = False
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
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Web/Worker.hs", fileContent = "module Web.Worker where\n\nimport IHP.Prelude\nimport Web.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Web.Job.CreateContainer\n\ninstance Worker WebApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                ]


        it "should support other applications" do
            let applicationName = "Admin"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let isFirstJobInApplication = False
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
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Admin/Job"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL,\n    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL\n);\n"}
                , CreateFile {filePath = "Admin/Job/CreateContainer.hs", fileContent = "module Admin.Job.CreateContainer where\nimport Admin.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , CreateFile {filePath = "Admin/Worker.hs", fileContent = "module Admin.Worker where\n\nimport IHP.Prelude\nimport Admin.Types\nimport Generated.Types\nimport IHP.Job.Runner\nimport IHP.Job.Types\n\nimport Admin.Job.CreateContainer\n\ninstance Worker AdminApplication where\n    workers _ =\n        [ worker @CreateContainerJob\n        -- Generator Marker\n        ]\n"}
                ]
