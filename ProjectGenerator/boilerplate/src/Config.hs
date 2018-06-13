module Config where

import ClassyPrelude
import Foundation.Environment

environment = Development

baseUrl :: Text
baseUrl = "http://localhost:8000"

postgreSQLUrl :: ByteString
postgreSQLUrl = "postgresql://localhost:8001/app"
