module Main where

import ClassyPrelude
import Foundation.Environment
import Foundation.FrameworkConfig
import qualified Foundation.Server
import Foundation.RouterSupport
import Foundation.ControllerPrelude
import Foundation.GenericController

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
main = Foundation.Server.run
