module Foundation.ModelSupport where

import Foundation.HaskellSupport
import ClassyPrelude
import Database.PostgreSQL.Simple (Connection)

data ModelContext = ModelContext Connection

