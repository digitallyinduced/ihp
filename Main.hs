module Main where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.RouterSupport
import IHP.ControllerPrelude
--import IHP.GenericController

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
main = IHP.Server.run
