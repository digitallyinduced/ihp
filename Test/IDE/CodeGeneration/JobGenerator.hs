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


tests = do
    describe "Job Generator" do
        it "should build a job with name \"CreateContainerJobs\"" do
            let applicationName = "Web"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Job"}
                , CreateFile {filePath = "Web/Job/CreateContainer.hs", fileContent = "module Web.Job.CreateContainer where\nimport Web.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL\n);\n"}
                , AppendToMarker {marker = "-- Job Imports", filePath = "Web/Worker.hs", fileContent = "import Web.Job.CreateContainer"}
                ]

        it "should support other applications" do
            let applicationName = "Admin"
            let tableName = "create_container_jobs"
            let modelName = "CreateContainerJob"
            let config = JobGenerator.JobConfig { .. }
            let builtPlan = JobGenerator.buildPlan' config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Admin/Job"}
                , CreateFile {filePath = "Admin/Job/CreateContainer.hs", fileContent = "module Admin.Job.CreateContainer where\nimport Admin.Controller.Prelude\n\ninstance Job CreateContainerJob where\n    perform CreateContainerJob { .. } = do\n        putStrLn \"Hello World!\"\n"}
                , AppendToFile {filePath = "Application/Schema.sql", fileContent = "CREATE TABLE create_container_jobs (\n    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,\n    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,\n    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,\n    last_error TEXT DEFAULT NULL,\n    attempts_count INT DEFAULT 0 NOT NULL,\n    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,\n    locked_by UUID DEFAULT NULL\n);\n"}
                , AppendToMarker {marker = "-- Job Imports", filePath = "Admin/Worker.hs", fileContent = "import Admin.Job.CreateContainer"}
                ]
