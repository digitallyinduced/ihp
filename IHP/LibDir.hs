{-|
Module: IHP.LibDir
Description: Functions to access the IHP lib/ directory at runtime
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.LibDir (findLibDirectory) where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified System.Process as Process
import qualified System.Posix.Files as Files
import qualified Control.Exception as Exception
import qualified IHP.FrameworkConfig as Config

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
    if frameworkMountedLocally
        then pure "IHP/lib/IHP/"
        else do
            -- The IHP_LIB env var is set in flake-module.nix
            ihpLibVar <- Config.envOrNothing "IHP_LIB"
            case ihpLibVar of
                Just ihpLib -> pure ihpLib
                Nothing -> do
                    -- This branch deals with legacy IHP versions before v1.1
                    -- We can remove this in the future
                    ihpLibSymlinkAvailable <- Directory.doesDirectoryExist "build/ihp-lib"
                    if ihpLibSymlinkAvailable
                        then pure "build/ihp-lib/"
                        else do
                            binDir <- cs <$> Process.readCreateProcess (Process.shell "dirname $(which RunDevServer)") ""
                            pure (Text.strip binDir <> "/../lib/IHP/")