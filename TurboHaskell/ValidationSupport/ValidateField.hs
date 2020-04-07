module TurboHaskell.ValidationSupport.ValidateField where

import ClassyPrelude
import Data.Proxy
import TurboHaskell.ValidationSupport.Types
import GHC.TypeLits (KnownSymbol, Symbol)
import GHC.Records
import TurboHaskell.ModelSupport
import TurboHaskell.HaskellSupport

type Validator valueType = valueType -> ValidatorResult

validateField :: forall field fieldValue validator model validationState. (
        KnownSymbol field
        , HasField field model fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
    ) => Proxy field -> Validator fieldValue -> model -> model
validateField field validator model = attachValidatorResult field (validator (getField @field model)) model
{-# INLINE validateField #-}

{-# INLINE nonEmpty #-}
nonEmpty :: MonoFoldable value => value -> ValidatorResult
nonEmpty value | null value = Failure "This field cannot be empty"
nonEmpty _ = Success

{-# INLINE isPhoneNumber #-}
isPhoneNumber :: Text -> ValidatorResult
isPhoneNumber text | "+" `isPrefixOf` text && length text > 5 = Success
isPhoneNumber text = Failure "is not a valid phone number (has to start with +, at least 5 characters)"

{-# INLINE isEmail #-}
isEmail :: Text -> ValidatorResult
isEmail text | "@" `isInfixOf` text = Success
isEmail text = Failure "is not a valid email"

{-# INLINE isInRange #-}
isInRange :: (Show value, Ord value) => (value, value) -> value -> ValidatorResult
isInRange (min, max) value | value >= min && value <= max = Success
isInRange (min, max) value = Failure (" has to be between " <> tshow min <> " and " <> tshow max)

{-# INLINE isColor #-}
isColor :: Text -> ValidatorResult
isColor text | ("#" `isPrefixOf` text) && (length text == 7) = Success
isColor text = Failure "is not a valid color"

{-# INLINE isUrl #-}
isUrl :: Text -> ValidatorResult
isUrl text | "http://" `isPrefixOf` text || "https://" `isPrefixOf` text = Success
isUrl text = Failure "is not a valid url. It needs to start with http:// or https://"