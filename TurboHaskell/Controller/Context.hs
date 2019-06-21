module TurboHaskell.Controller.Context where

import Prelude
import TurboHaskell.Controller.RequestContext
import TurboHaskell.ModelSupport

class Context context where
    createContext :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => IO context

instance Context () where
    {-# INLINE createContext #-}
    createContext = return ()