module Main where
import IHP.Prelude

import Config
import qualified IHP.Server
import IHP.RouterSupport
import IHP.FrameworkConfig
import IHP.Job.Types
import Web.FrontController
import Web.Types

instance FrontController RootApplication where
    controllers = [
            mountFrontController WebApplication
        ]

instance Worker RootApplication where
    workers _ = []

main :: IO ()
main = IHP.Server.run config
