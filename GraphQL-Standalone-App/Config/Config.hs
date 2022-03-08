module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")