{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TurboHaskell.ValidationSupport
    ( module TurboHaskell.ValidationSupport.Types
    , module TurboHaskell.ValidationSupport.ValidateCanView
    , module TurboHaskell.ValidationSupport.ValidateField
    , module TurboHaskell.ValidationSupport.ValidateIsUnique
    , module TurboHaskell.ValidationSupport.ValidateFieldIO
    , initValidationContext
    )
where

import ClassyPrelude
import TurboHaskell.ValidationSupport.Types
import TurboHaskell.ValidationSupport.ValidateCanView
import TurboHaskell.ValidationSupport.ValidateIsUnique
import TurboHaskell.ValidationSupport.ValidateField
import TurboHaskell.ValidationSupport.ValidateFieldIO
import qualified Data.TMap as TypeMap

{-# INLINE initValidationContext #-}
initValidationContext :: TypeMap.TMap -> IO TypeMap.TMap
initValidationContext context = do
    validations <- newIORef []
    return (TypeMap.insert (ValidationResults validations) context)