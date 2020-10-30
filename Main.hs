module Main where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.RouterSupport
import IHP.ControllerPrelude
--import IHP.GenericController

data DemoController = DemoAction deriving (Eq, Show, Data)

instance AutoRoute DemoController
instance InitControllerContext RootApplication
instance FrontController RootApplication where
    controllers =
        [ parseRoute @DemoController
        , startPage DemoAction
        ]

instance Controller DemoController where
    action DemoAction = renderPlain "Hello World!"

instance FrameworkConfig where 
	environment = Development
	baseUrl = "http://localhost:8000"

main :: IO ()
main = IHP.Server.run
