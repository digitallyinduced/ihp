{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Version
Description: Find out the current IHP version
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Version where

import IHP.Prelude
import GitHash

-- | Returns the git commit hash of https://github.com/digitallyinduced/ihp at which this IHP version was built
ihpCommit :: Text
ihpCommit = cs $! giHash ($$tGitInfoCwd)