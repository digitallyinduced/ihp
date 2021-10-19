module Main where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.RouterSupport
import IHP.ControllerPrelude
import IHP.Mail
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

-- @todo: Without this I got an error
--     * No instance for (Worker RootApplication)
--        arising from a use of `IHP.Server.run'
--     * In the expression: IHP.Server.run config
--       In an equation for `main': main = IHP.Server.run config
--    |
-- 31 | main = IHP.Server.run config
instance Worker RootApplication where
  workers _ = []

config :: ConfigBuilder
config = do
    option Development
    option $ AppHostname "localhost"

main :: IO ()
main = IHP.Server.run config
