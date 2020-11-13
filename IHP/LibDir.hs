{-|
Module: IHP.LibDir
Description: Functions to access the IHP lib/ directory at runtime
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.LibDir (findLibDirectory, ensureSymlink) where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified System.Process as Process
import qualified System.Posix.Files as Files

-- | Finds the lib
--
-- The location depends on whether the framework is installed through nix
-- or checked out from git inside the current project directory.
--
-- When it's installed with nix, the lib dir is located at @lib/ihp@
-- while the dev server binary is located at @bin/RunDevServer@.
findLibDirectory :: IO Text
findLibDirectory = do
    frameworkMountedLocally <- Directory.doesDirectoryExist "IHP"
    ihpLibSymlinkAvailable <- Directory.doesDirectoryExist "build/ihp-lib"
    if frameworkMountedLocally
        then pure "IHP/lib/IHP/"
        else if ihpLibSymlinkAvailable
            then do
                pure "build/ihp-lib/"
            else do
                binDir <- cs <$> Process.readCreateProcess (Process.shell "dirname $(which RunDevServer)") ""
                pure (Text.strip binDir <> "/../lib/IHP/")

-- | Creates the build/ihp-lib symlink if missing
--
-- This is called in dev mode to make sure that build/ihp-lib points to the right IHP version.
-- Otherwise the CLI tools might not work as expected, e.g. @make db@ might fail.
--
-- If the symlink is missing, it tries to fix this by starting a new nix-shell and pinpointing the framework lib dir in there
ensureSymlink :: IO ()
ensureSymlink = do
    frameworkMountedLocally <- pure False
    ihpLibSymlinkAvailable <- Directory.doesDirectoryExist "build/ihp-lib"

    unless ihpLibSymlinkAvailable do
        -- Make sure the build directory exists, otherwise we cannot add the symlink
        Directory.createDirectoryIfMissing False "build"

        libDir <- if frameworkMountedLocally
            then pure "../IHP/lib/IHP/"
            else do
                putStrLn "Building build/ihp-lib. This might take a few seconds"
                binDir <- cs <$> Process.readCreateProcess (Process.shell "nix-shell --run 'dirname $(which RunDevServer)'") ""
                pure (Text.strip binDir <> "/../lib/IHP/")
        Files.createSymbolicLink (cs libDir) "build/ihp-lib"