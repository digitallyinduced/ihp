module TurboHaskell.AuthSupport.Authorization where

import TurboHaskell.Prelude

class CanView user model where
    canView :: (?modelContext :: ModelContext) => model -> user -> IO Bool
