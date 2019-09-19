module TurboHaskell.ValidationSupport.ValidateField where

import           ClassyPrelude
import           Data.Proxy
import           TurboHaskell.NameSupport               (humanize)
import           TurboHaskell.ValidationSupport.Types
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import GHC.Records
import Control.Monad.State
import TurboHaskell.ModelSupport

type Validator2 value = ValidateFieldInner value -> ValidatorResult

type family ValidateFieldInner fieldValue where
    ValidateFieldInner (FieldWithDefault inner) = inner
    ValidateFieldInner fieldValue = fieldValue

class ValidateField fieldValue where
    validateField :: forall field validator model validationState. (
            KnownSymbol field
            , HasField field model fieldValue
        ) => Proxy field -> Validator2 fieldValue -> model -> StateT [(Text, Text)] IO model

instance ValidateField (FieldWithDefault inner) where
    {-# INLINE validateField #-}
    validateField :: forall field validator model validationState. (
            KnownSymbol field
            , HasField field model (FieldWithDefault inner)
        ) => Proxy field -> Validator2 (FieldWithDefault inner) -> model -> StateT [(Text, Text)] IO model
    validateField field validator model = do
        case (getField @field model) :: FieldWithDefault inner of
            Default -> return model
            NonDefault value -> do
                case validator value of
                    Failure message -> do attachFailure field message; return model
                    Success -> return model

instance {-# OVERLAPPABLE #-} (ValidateFieldInner fieldValue ~ fieldValue) => ValidateField fieldValue where
    {-# INLINE validateField #-}
    validateField :: forall field validator model validationState. (
            KnownSymbol field
            , HasField field model fieldValue
        ) => Proxy field -> Validator2 fieldValue -> model -> StateT [(Text, Text)] IO model
    validateField field validator model = do
        validationState <- get
        case validator (getField @field model) of
            Failure message -> do attachFailure field message; return model
            Success -> return model

{-# INLINE validateNothing #-}
validateNothing :: forall s. StateT s IO ()
validateNothing = return ()

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
