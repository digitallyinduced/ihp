{-|
Module: IHP.Version
Description: Find out the current IHP version
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Version where

import IHP.Prelude
import qualified Paths_ihp
import Data.Version (showVersion)

data Edition = Basic | Pro | Business | Enterprise
    deriving (Eq, Show)

-- | Returns the git commit hash of https://github.com/digitallyinduced/ihp at which this IHP version was built
ihpVersion :: Text
ihpVersion = cs $! showVersion Paths_ihp.version

ihpEdition :: Edition
ihpEdition = Pro