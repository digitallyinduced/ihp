module Main where

import IHP.Prelude
import qualified IHP.Server
import IHP.Job.Types
import IHP.RouterSupport

import Config
import Web.FrontController ()
import Web.SlowLoad ()

instance Worker RootApplication where
    workers _ = []

main :: IO ()
main = IHP.Server.run config
