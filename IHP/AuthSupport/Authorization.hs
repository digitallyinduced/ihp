module IHP.AuthSupport.Authorization where

import IHP.Prelude

class CanView user model where
    canView :: (?modelContext :: ModelContext) => model -> user -> IO Bool