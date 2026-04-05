module Main where

import IHP.Prelude
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.Job.Types

import Config
import Web.FrontController ()
import Web.SlowLoad ()

instance Worker RootApplication where
    workers _ = []

main :: IO ()
main = IHP.Server.run config
