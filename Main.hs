{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.ControllerPrelude
import IHP.Mail

data DemoController = DemoAction deriving (Eq, Show, Data)

instance InitControllerContext RootApplication
data WebApplication = WebApplication deriving (Eq, Show)

instance InitControllerContext WebApplication where
    initContext = pure ()

$(pure [])

[routes|DemoController
GET / DemoAction
|]

instance FrontController WebApplication where
    controllers =
        [ parseRoute @DemoController
        ]

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

instance Controller DemoController where
    action DemoAction = renderPlain "Hello World!"

instance Worker RootApplication where
  workers _ = []

config :: ConfigBuilder
config = do
    option Development
    option $ AppHostname "localhost"

main :: IO ()
main = IHP.Server.run config
