{-# LANGUAGE MultiParamTypeClasses #-}

module TurboHaskell.AuthSupport.Authorization where

import ClassyPrelude
import TurboHaskell.ModelSupport

class CanView user model where
    canView :: (?modelContext :: ModelContext) => model -> user -> IO Bool
