{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.SchemaMigrationSpec where

import Test.Hspec
import IHP.Prelude
import IHP.SchemaMigration
import qualified System.Directory as Directory

tests = do
    describe "IHP.SchemaMigration" do
        describe "findAllMigrations" do
            it "should find all migrations" do
                Directory.withCurrentDirectory "Test/TestApp" do
                    migrations <- findAllMigrations

                    migrations `shouldBe`
                            [ Migration { revision = 1605721927, migrationFile = "1605721927.sql"}
                            , Migration { revision = 1605721940, migrationFile = "1605721940-create-users.sql" }
                            ]
