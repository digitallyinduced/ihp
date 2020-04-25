module TurboHaskell.Environment where
import TurboHaskell.Prelude

data Environment = Development | Production deriving (Eq, Show)

{-# INLINE isDevelopment #-}
isDevelopment :: Environment -> Bool
isDevelopment Development = True
isDevelopment _           = False

{-# INLINE isProduction #-}
isProduction :: Environment -> Bool
isProduction Production = True
isProduction _          = False
