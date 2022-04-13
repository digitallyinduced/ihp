{-|
Module: Test.NameSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.NameSupportSpec where

import Test.Hspec
import IHP.Prelude
import IHP.NameSupport


tests = do
    describe "NameSupport" do
        describe "tableNameToModelName" do
            it "should deal with empty input" do
                tableNameToModelName "" `shouldBe` ""

            it "should transform table names to model names" do
                tableNameToModelName "users" `shouldBe` "User"
                tableNameToModelName "projects" `shouldBe` "Project"
                tableNameToModelName "user_projects" `shouldBe` "UserProject"
                tableNameToModelName "users_projects" `shouldBe` "UsersProject"
                tableNameToModelName "people" `shouldBe` "Person"

        describe "tableNameToControllerName" do
            it "should deal with empty input" do
                tableNameToControllerName "" `shouldBe` ""

            it "should transform table names to controller names" do
                tableNameToControllerName "users" `shouldBe` "Users"
                tableNameToControllerName "projects" `shouldBe` "Projects"
                tableNameToControllerName "user_projects" `shouldBe` "UserProjects"
                tableNameToControllerName "users_projects" `shouldBe` "UsersProjects"
                tableNameToControllerName "people" `shouldBe` "People"

        describe "enumValueToControllerName" do
            it "should handle spaces in table names" do
                enumValueToControllerName "very happy" `shouldBe` "VeryHappy"
                enumValueToControllerName "sad" `shouldBe` "Sad"
                enumValueToControllerName "very sad" `shouldBe` "VerySad"

            it "should deal with typical enum cases" do
                enumValueToControllerName "job_status_not_started" `shouldBe` "JobStatusNotStarted"
                enumValueToControllerName "job_status_running" `shouldBe` "JobStatusRunning"
                enumValueToControllerName "job_status_failed" `shouldBe` "JobStatusFailed"
                enumValueToControllerName "job_status_succeeded" `shouldBe` "JobStatusSucceeded"
                enumValueToControllerName "job_status_retry" `shouldBe` "JobStatusRetry"

        describe "modelNameToTableName" do
            it "should deal with empty input" do
                modelNameToTableName "" `shouldBe` ""

            it "should transform model names to table names" do
                modelNameToTableName "User" `shouldBe` "users"
                modelNameToTableName "Project" `shouldBe` "projects"
                modelNameToTableName "UserProject" `shouldBe` "user_projects"
                modelNameToTableName "UsersProjects" `shouldBe` "users_projects"
                modelNameToTableName "Person" `shouldBe` "people"

        describe "columnNameToFieldName" do
            it "should deal with empty input" do
                columnNameToFieldName "" `shouldBe` ""

            it "should transform column names to field names" do
                columnNameToFieldName "email" `shouldBe` "email"
                columnNameToFieldName "project_id" `shouldBe` "projectId"
                columnNameToFieldName "user_project_name" `shouldBe` "userProjectName"

        describe "fieldNameToColumnName" do
            it "should deal with empty input" do
                fieldNameToColumnName "" `shouldBe` ""

            it "should transform field names to column names" do
                fieldNameToColumnName "email" `shouldBe` "email"
                fieldNameToColumnName "projectId" `shouldBe` "project_id"
                fieldNameToColumnName "userProjectName" `shouldBe` "user_project_name"

        describe "lcfirst" do
            it "should deal with empty input" do
                lcfirst "" `shouldBe` ""

            it "should lowercase the first letter" do
                lcfirst "Hello" `shouldBe` "hello"
                lcfirst "Hello World" `shouldBe` "hello World"
                lcfirst "1337" `shouldBe` "1337"

        describe "ucfirst" do
            it "should deal with empty input" do
                ucfirst "" `shouldBe` ""

            it "should uppercase the first letter" do
                ucfirst "hello" `shouldBe` "Hello"
                ucfirst "hello world" `shouldBe` "Hello world"
                ucfirst "1337" `shouldBe` "1337"

        describe "escapeHaskellKeyword" do
            it "should deal with empty input" do
                escapeHaskellKeyword "" `shouldBe` ""

            it "should escape haskell keywords" do
                escapeHaskellKeyword "type" `shouldBe` "type_"
                escapeHaskellKeyword "DATA" `shouldBe` "DATA_"

            it "should ignore non-haskell keywords" do
                escapeHaskellKeyword "hello" `shouldBe` "hello"

        describe "toSlug" do
            it "should deal with empty input" do
                toSlug "" `shouldBe` ""

            it "should make a slug string from a string" do
                toSlug "IHP Release: 21.08.2020 (v21082020)" `shouldBe` "ihp-release-21-08-2020-v21082020"
                toSlug "Hallo! @ Welt" `shouldBe` "hallo-welt"
            
            it "should deal with umlaute" do
                toSlug "käuferpass" `shouldBe` "kauferpass"
                toSlug "äöü" `shouldBe` "aou"
