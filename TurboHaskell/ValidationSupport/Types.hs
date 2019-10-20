module TurboHaskell.ValidationSupport.Types where

import ClassyPrelude
import TurboHaskell.ModelSupport
import TurboHaskell.HaskellSupport
import qualified Data.Text as Text
import qualified GHC.Records as Records
import Data.Proxy
import GHC.TypeLits

data ValidatorResult = Success | Failure !Text deriving (Show, Eq)

{-# INLINE isSuccess #-}
isSuccess Success = True
isSuccess _       = False

{-# INLINE isFailure #-}
isFailure Failure {} = True
isFailure _  = False

{-# INLINE attachValidatorResult #-}
attachValidatorResult :: (KnownSymbol field, Records.HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> ValidatorResult -> model -> model
attachValidatorResult field Success record = record
attachValidatorResult field (Failure message) record = attachFailure field message record

{-# INLINE attachFailure #-}
attachFailure :: (KnownSymbol field, Records.HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> Text -> model -> model
attachFailure field !message = modify #meta prependAnnotation
    where
        prependAnnotation :: MetaBag -> MetaBag
        prependAnnotation = modify #annotations (\a -> annotation:a)
        annotation = (Text.pack (symbolVal field), message)