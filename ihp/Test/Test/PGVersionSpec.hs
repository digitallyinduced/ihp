{-|
Module: Test.PGVersionSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.PGVersionSpec where

import Test.Hspec
import IHP.Prelude
import qualified Control.Exception as Exception
import qualified System.Environment as Environment
import IHP.PGVersion (defaultUuidFunction)

tests = do
    describe "defaultUuidFunction" do
        it "uses uuidv7 when IHP_POSTGRES_VERSION is unset" do
            withPostgresVersion Nothing do
                defaultUuidFunction `shouldReturn` "uuidv7"

        it "uses uuidv7 for PostgreSQL 18 and newer" do
            withPostgresVersion (Just "18") do
                defaultUuidFunction `shouldReturn` "uuidv7"
            withPostgresVersion (Just "19") do
                defaultUuidFunction `shouldReturn` "uuidv7"

        it "uses uuid_generate_v4 below PostgreSQL 18" do
            withPostgresVersion (Just "17") do
                defaultUuidFunction `shouldReturn` "uuid_generate_v4"

        it "uses uuidv7 when IHP_POSTGRES_VERSION is not a number" do
            withPostgresVersion (Just "latest") do
                defaultUuidFunction `shouldReturn` "uuidv7"

withPostgresVersion :: Maybe String -> IO a -> IO a
withPostgresVersion version action = do
    previous <- Environment.lookupEnv "IHP_POSTGRES_VERSION"
    Exception.bracket_
        (apply version)
        (apply previous)
        action
    where
        apply Nothing = Environment.unsetEnv "IHP_POSTGRES_VERSION"
        apply (Just value) = Environment.setEnv "IHP_POSTGRES_VERSION" value
