{-# LANGUAGE MultiParamTypeClasses #-}

module Foundation.AuthSupport.Authorization where

import ClassyPrelude
import Foundation.ModelSupport

class CanView user model where
    canView :: (?modelContext :: ModelContext) => model -> user -> IO Bool
