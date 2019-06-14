module Main where

import ClassyPrelude
import TurboHaskell.Environment
import TurboHaskell.FrameworkConfig
import qualified TurboHaskell.Server
import TurboHaskell.RouterSupport
import TurboHaskell.ControllerPrelude
import TurboHaskell.GenericController

data DemoController = DemoAction deriving (Eq, Show, Generic)

instance HasPath RootApplication where
	pathTo _ = ""
instance CanRoute RootApplication () where
    parseRoute = parseRoute @DemoController

instance HasPath DemoController where
	pathTo _ = ""
instance CanRoute DemoController () where
    parseRoute = return (renderPlain "test")

instance FrameworkConfig where 
	environment = Development
	baseUrl = "http://localhost:8000"

main :: IO ()
main = TurboHaskell.Server.run
