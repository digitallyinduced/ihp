{-|
Module: IDE.SplitModeSpec
-}
module IDE.SplitModeSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import qualified IHP.IDE.SplitMode as SplitMode
import Control.Exception (bracket_)
import qualified Prelude as P

tests :: Spec
tests = do
    describe "IHP.IDE.SplitMode.hasJobs" do
        it "returns False for an empty project" do
            withTempCwd \_ -> do
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` False

        it "returns False when there are .hs files but none under a Job/ directory" do
            withTempCwd \dir -> do
                Directory.createDirectoryIfMissing True (dir <> "/Web/Controller")
                P.writeFile (dir <> "/Web/Controller/Static.hs") "module Web.Controller.Static where\n"
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` False

        it "returns True when a .hs file lives under */Job/" do
            withTempCwd \dir -> do
                Directory.createDirectoryIfMissing True (dir <> "/Web/Job")
                P.writeFile (dir <> "/Web/Job/SendEmail.hs") "module Web.Job.SendEmail where\n"
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` True

        it "matches case-insensitively (Job, JOB, job)" do
            withTempCwd \dir -> do
                Directory.createDirectoryIfMissing True (dir <> "/admin/JOB")
                P.writeFile (dir <> "/admin/JOB/Cleanup.hs") "module X where\n"
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` True

        it "ignores .hs files in excluded directories like node_modules" do
            withTempCwd \dir -> do
                Directory.createDirectoryIfMissing True (dir <> "/node_modules/some-lib/Job")
                P.writeFile (dir <> "/node_modules/some-lib/Job/X.hs") "module X where\n"
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` False

        it "ignores .hs files in build/ (the dev-time RunJobs lives there but doesn't count)" do
            withTempCwd \dir -> do
                Directory.createDirectoryIfMissing True (dir <> "/build/Job")
                P.writeFile (dir <> "/build/Job/X.hs") "module X where\n"
                hasJobs <- SplitMode.hasJobs
                hasJobs `shouldBe` False

    describe "IHP.IDE.SplitMode.generateRunJobsModule" do
        it "writes build/RunJobs.hs with the expected contents" do
            withTempCwd \dir -> do
                SplitMode.generateRunJobsModule
                contents <- ByteString.readFile (dir <> "/build/RunJobs.hs")
                contents `shouldBe` SplitMode.runJobsModuleContents

        it "is a no-op (preserves mtime) when the file is already up to date" do
            withTempCwd \dir -> do
                SplitMode.generateRunJobsModule
                let path = dir <> "/build/RunJobs.hs"
                mtime1 <- Directory.getModificationTime path
                SplitMode.generateRunJobsModule
                mtime2 <- Directory.getModificationTime path
                mtime2 `shouldBe` mtime1

        it "imports WorkerMain rather than Main (for dev/prod parity)" do
            withTempCwd \_ -> do
                SplitMode.generateRunJobsModule
                contents <- ByteString.readFile "build/RunJobs.hs"
                ("WorkerMain" `ByteString.isInfixOf` contents) `shouldBe` True
                ("import Main" `ByteString.isInfixOf` contents) `shouldBe` False

    describe "IHP.IDE.SplitMode.workerSocketPath" do
        it "lives outside the project directory" do
            withTempCwd \dir -> do
                path <- SplitMode.workerSocketPath
                FilePath.isAbsolute path `shouldBe` True
                projectDir <- Directory.canonicalizePath dir
                path `shouldSatisfy` (not . (projectDir `List.isPrefixOf`))

        it "is the same for one project and different for another" do
            withTempCwd \_ -> do
                first <- SplitMode.workerSocketPath
                again <- SplitMode.workerSocketPath
                again `shouldBe` first
                other <- withTempCwd \_ -> SplitMode.workerSocketPath
                other `shouldNotBe` first

        it "uses XDG_RUNTIME_DIR when set" do
            withTempCwd \_ ->
                -- Under /tmp because the macOS temp dir is too long for a socket.
                Temp.withTempDirectory "/tmp" "ihp-rt" \runtimeDir ->
                    withEnv "XDG_RUNTIME_DIR" runtimeDir do
                        path <- SplitMode.workerSocketPath
                        FilePath.takeDirectory path `shouldBe` runtimeDir

        it "is short enough for a Unix socket" do
            withTempCwd \_ -> do
                path <- SplitMode.workerSocketPath
                length path `shouldSatisfy` (<= 100)

-- | Run an action with an env var set, then restore the old value.
withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
    previous <- Env.lookupEnv name
    bracket_
        (Env.setEnv name value)
        (maybe (Env.unsetEnv name) (Env.setEnv name) previous)
        action

-- | Run an action with a fresh temporary directory as the current working
-- directory. Restores the previous cwd on exit.
withTempCwd :: (FilePath -> IO a) -> IO a
withTempCwd action = do
    Temp.withSystemTempDirectory "ihp-splitmode-test" \dir -> do
        previous <- Directory.getCurrentDirectory
        bracket_
            (Directory.setCurrentDirectory dir)
            (Directory.setCurrentDirectory previous)
            (action dir)
