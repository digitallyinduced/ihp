{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import Test.Hspec
import Prelude
import IHP.SchemaMigration
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Temp.OsPath (withSystemTempDirectory)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.OsPath (encodeUtf, decodeUtf)

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

            it "should fail when multiple migrations use the same timestamp" do
                withTempMigrationFiles ["1605721927.sql", "1605721927-create-users.sql"] do
                    findAllMigrations `shouldThrow` \exception ->
                        let message = ioeGetErrorString exception
                        in isUserError exception
                            && "Multiple migrations use the same timestamp" `isInfixOf` message
                            && "1605721927.sql" `isInfixOf` message
                            && "1605721927-create-users.sql" `isInfixOf` message


withTempApp :: IO a -> IO a
withTempApp =
    withTempMigrationFiles
        [ "1605721927.sql"
        , "1605721940-create-users.sql"
        ]

withTempMigrationFiles :: [FilePath] -> IO a -> IO a
withTempMigrationFiles migrationFiles action = do
    template <- encodeUtf "ihp-migrate-test"
    withSystemTempDirectory template \tmpOsPath -> do
        tmp <- decodeUtf tmpOsPath
        let appRoot      = tmp
        let migrationDir = appRoot </> "Application" </> "Migration"

        -- Create the directory structure
        Directory.createDirectoryIfMissing True migrationDir

        -- Create migration files and one non-migration file
        forM_ migrationFiles \migrationFile -> do
            writeFile (migrationDir </> migrationFile) ""
        writeFile (migrationDir </> "not_a_migration") ""

        -- Now run findAllMigrations as if this were an IHP app root
        Directory.withCurrentDirectory appRoot action
