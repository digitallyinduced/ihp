module Main where

import TurboHaskell.Prelude
import TurboHaskell.Environment
import TurboHaskell.FrameworkConfig
import qualified TurboHaskell.Server
import TurboHaskell.RouterSupport
import TurboHaskell.ControllerPrelude
--import TurboHaskell.GenericController

data DemoController = DemoAction deriving (Eq, Show)

instance HasPath RootApplication where
	pathTo _ = ""
instance CanRoute RootApplication () where
    parseRoute = parseRoute @DemoController

instance HasPath DemoController where
	pathTo _ = ""
instance CanRoute DemoController () where
    parseRoute = pure (renderPlain "test")

instance FrameworkConfig where 
	environment = Development
	baseUrl = "http://localhost:8000"

main :: IO ()
main = TurboHaskell.Server.run
