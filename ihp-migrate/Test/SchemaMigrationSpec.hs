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
import System.FilePath.Posix
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