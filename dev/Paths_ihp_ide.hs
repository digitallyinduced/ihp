{-# OPTIONS_GHC -Wno-deprecations  #-}
module Paths_ihp_ide where

import Data.Version
import Prelude
import System.Directory (doesPathExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)

version = Version { versionBranch = [1, 3, 0], versionTags = [] }

getDataFileName path = do
    let directCandidate = path
    directExists <- doesPathExist directCandidate
    if directExists
        then pure directCandidate
        else do
            envCandidates <- ihpLibCandidates path
            firstExistingPath envCandidates directCandidate

ihpLibCandidates :: FilePath -> IO [FilePath]
ihpLibCandidates path = do
    ihpLib <- lookupEnv "IHP_LIB"
    pure case ihpLib of
        Nothing -> []
        Just ihpLibPath ->
            let ihpIdePath = takeDirectory (takeDirectory ihpLibPath)
             in
                [ ihpIdePath </> path
                , ihpIdePath </> "data" </> path
                ]


firstExistingPath :: [FilePath] -> FilePath -> IO FilePath
firstExistingPath [] defaultPath = pure defaultPath
firstExistingPath (candidate : rest) defaultPath = do
    exists <- doesPathExist candidate
    if exists
        then pure candidate
        else firstExistingPath rest defaultPath
