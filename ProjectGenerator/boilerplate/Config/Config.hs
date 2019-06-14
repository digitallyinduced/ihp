module Config where

import ClassyPrelude
import TurboHaskell.Environment
import TurboHaskell.FrameworkConfig

instance FrameworkConfig where 
	environment = Development
	baseUrl = "http://localhost:8000"