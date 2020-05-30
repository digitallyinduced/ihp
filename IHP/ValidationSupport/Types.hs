module IHP.ValidationSupport.Types where

import IHP.Prelude
import qualified Data.Text as Text

data ValidatorResult = Success | Failure !Text deriving (Show, Eq)

{-# INLINE isSuccess #-}
isSuccess Success = True
isSuccess _       = False

{-# INLINE isFailure #-}
isFailure Failure {} = True
isFailure _  = False

{-# INLINE attachValidatorResult #-}
attachValidatorResult :: (KnownSymbol field, HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> ValidatorResult -> model -> model
attachValidatorResult field Success record = record
attachValidatorResult field (Failure message) record = attachFailure field message record

{-# INLINE attachFailure #-}
attachFailure :: (KnownSymbol field, HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> Text -> model -> model
attachFailure field !message = modify #meta prependAnnotation
    where
        prependAnnotation :: MetaBag -> MetaBag
        prependAnnotation = modify #annotations (\a -> annotation:a)
        annotation = (Text.pack (symbolVal field), message)

{-# INLINE getValidationFailure #-}
getValidationFailure :: (KnownSymbol field, HasField "meta" model MetaBag) => Proxy field -> model -> Maybe Text
getValidationFailure field model =
        model
            |> get #meta
            |> get #annotations
            |> lookup fieldName
    where
        fieldName = Text.pack (symbolVal field)