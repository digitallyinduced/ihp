{-|
Module: IDE.PostgresSpec
-}
module IDE.PostgresSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.Postgres (isPostgresReady)

import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Temp as Temp

tests :: Spec
tests = do
    describe "IHP.IDE.Postgres.isPostgresReady" do
        it "reports ready when PGHOST is unset, as the database isn't managed by the dev environment" do
            withEnv [("PGHOST", Nothing), ("IHP_DEVENV", Just "1")] do
                isPostgresReady >>= (`shouldBe` True)

        it "reports ready when pg_isready is not on PATH, so a missing binary can't block the dev server" do
            withEnv [("PGHOST", Just "/tmp"), ("IHP_DEVENV", Just "0"), ("PATH", Just "/nonexistent")] do
                isPostgresReady >>= (`shouldBe` True)

        it "reports not ready while devenv is still importing the schema" do
            -- devenv's postgres accepts connections during the import, so pg_isready
            -- succeeding is not enough: without the marker the app would query a
            -- schema that is only half there.
            withPgIsReady ExitsSuccessfully \binDir ->
                Temp.withSystemTempDirectory "pgdata" \pgData ->
                    withEnv [("PGHOST", Just "/tmp"), ("IHP_DEVENV", Just "1"), ("PGDATA", Just pgData), ("PATH", Just binDir)] do
                        isPostgresReady >>= (`shouldBe` False)

        it "reports ready once devenv has written the initialization marker" do
            withPgIsReady ExitsSuccessfully \binDir ->
                Temp.withSystemTempDirectory "pgdata" \pgData -> do
                    IO.writeFile (pgData FilePath.</> ".devenv_initialized") ""
                    withEnv [("PGHOST", Just "/tmp"), ("IHP_DEVENV", Just "1"), ("PGDATA", Just pgData), ("PATH", Just binDir)] do
                        isPostgresReady >>= (`shouldBe` True)

        it "reports not ready while postgres is not accepting connections yet" do
            withPgIsReady Fails \binDir ->
                Temp.withSystemTempDirectory "pgdata" \pgData -> do
                    IO.writeFile (pgData FilePath.</> ".devenv_initialized") ""
                    withEnv [("PGHOST", Just "/tmp"), ("IHP_DEVENV", Just "1"), ("PGDATA", Just pgData), ("PATH", Just binDir)] do
                        isPostgresReady >>= (`shouldBe` False)

        it "ignores the devenv marker outside a devenv shell, where nobody writes it" do
            withPgIsReady ExitsSuccessfully \binDir ->
                Temp.withSystemTempDirectory "pgdata" \pgData ->
                    withEnv [("PGHOST", Just "/tmp"), ("IHP_DEVENV", Just "0"), ("PGDATA", Just pgData), ("PATH", Just binDir)] do
                        isPostgresReady >>= (`shouldBe` True)

data PgIsReadyBehaviour = ExitsSuccessfully | Fails

-- | Runs the callback with a directory containing a @pg_isready@ stub, so the specs
-- don't depend on a postgres being around.
withPgIsReady :: PgIsReadyBehaviour -> (FilePath -> IO a) -> IO a
withPgIsReady behaviour callback =
    Temp.withSystemTempDirectory "bin" \binDir -> do
        let stub = binDir FilePath.</> "pg_isready"
        let exitCode :: String = case behaviour of
                ExitsSuccessfully -> "0"
                Fails -> "1"
        IO.writeFile stub ("#!/bin/sh\nexit " <> exitCode <> "\n")
        Directory.setPermissions stub (Directory.setOwnerExecutable True Directory.emptyPermissions { Directory.readable = True })
        callback binDir

-- | Sets the given env vars for the duration of the callback, restoring them afterwards.
withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv vars callback = Exception.bracket saveAndSet restore (const callback)
    where
        saveAndSet = do
            previous <- forM vars \(name, value) -> do
                previousValue <- Env.lookupEnv name
                setOrUnset name value
                pure (name, previousValue)
            pure previous

        restore = mapM_ (uncurry setOrUnset)

        setOrUnset name = \case
            Just value -> Env.setEnv name value
            Nothing -> Env.unsetEnv name
