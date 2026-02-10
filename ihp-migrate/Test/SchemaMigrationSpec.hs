{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import Test.Hspec
import Prelude
import IHP.SchemaMigration
import qualified System.Directory as Directory
import qualified Data.List as List
import System.FilePath ((</>))
import System.IO.Temp

main :: IO ()
main = hspec do
    tests

tests = do
    describe "IHP.SchemaMigration" do
        describe "findAllMigrations" do
            it "should find all migrations" do
                withTempApp do
                    migrations <- findAllMigrations

                    migrations `shouldBe`
                            [ Migration { revision = 1605721927, migrationFile = "1605721927.sql"}
                            , Migration { revision = 1605721940, migrationFile = "1605721940-create-users.sql" }
                            ]

        describe "splitStatements" do
            it "should split simple statements" do
                splitStatements "CREATE TABLE a (id INT); CREATE TABLE b (id INT);"
                    `shouldBe` ["CREATE TABLE a (id INT)", "CREATE TABLE b (id INT)"]

            it "should handle statements without trailing semicolon" do
                splitStatements "CREATE TABLE a (id INT); CREATE TABLE b (id INT)"
                    `shouldBe` ["CREATE TABLE a (id INT)", "CREATE TABLE b (id INT)"]

            it "should ignore semicolons inside single-quoted strings" do
                splitStatements "INSERT INTO t (v) VALUES ('hello; world'); SELECT 1"
                    `shouldBe` ["INSERT INTO t (v) VALUES ('hello; world')", "SELECT 1"]

            it "should handle escaped quotes in strings" do
                splitStatements "INSERT INTO t (v) VALUES ('it''s a test'); SELECT 1"
                    `shouldBe` ["INSERT INTO t (v) VALUES ('it''s a test')", "SELECT 1"]

            it "should ignore semicolons in line comments" do
                splitStatements "SELECT 1; -- this; is a comment\nSELECT 2"
                    `shouldBe` ["SELECT 1", "-- this; is a comment\nSELECT 2"]

            it "should handle empty input" do
                splitStatements "" `shouldBe` []

            it "should filter blank statements from extra semicolons" do
                splitStatements "SELECT 1;; SELECT 2;" `shouldBe` ["SELECT 1", "SELECT 2"]

            it "should handle a single statement" do
                splitStatements "CREATE TABLE users (id INT)" `shouldBe` ["CREATE TABLE users (id INT)"]

withTempApp :: IO a -> IO a
withTempApp action =
    withSystemTempDirectory "ihp-migrate-test" \tmp -> do
        let appRoot      = tmp
        let migrationDir = appRoot </> "Application" </> "Migration"

        -- Create the directory structure
        Directory.createDirectoryIfMissing True migrationDir

        -- Create two migration files and one non-migration file
        writeFile (migrationDir </> "1605721927.sql") ""
        writeFile (migrationDir </> "1605721940-create-users.sql") ""
        writeFile (migrationDir </> "not_a_migration") ""

        -- Now run findAllMigrations as if this were an IHP app root
        Directory.withCurrentDirectory appRoot action