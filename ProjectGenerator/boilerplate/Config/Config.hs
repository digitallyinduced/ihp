module Config where

import ClassyPrelude
import Foundation.Environment
import Foundation.FrameworkConfig

instance FrameworkConfig where 
	environment = Development
	baseUrl = "http://localhost:8000"