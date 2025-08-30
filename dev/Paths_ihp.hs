{-# OPTIONS_GHC -Wno-deprecations  #-}
module Paths_ihp where

import Data.Version
import Prelude

version = Version { versionBranch = [1, 3, 0], versionTags = [] }

getDataFileName path = pure (prefix <> path)
    where
        prefix = "lib/IHP/"