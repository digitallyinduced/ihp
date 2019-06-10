module Main where
import ClassyPrelude

import Config
import qualified Foundation.Server
import Foundation.RouterSupport
import Apps.Web.App
import Apps.Web.Types
import Foundation.FrameworkConfig

instance HasPath RootApplication where
	pathTo _ = ""
instance CanRoute RootApplication () where
    parseRoute = parseRoute @WebApplication

main :: IO ()
main = Foundation.Server.run
