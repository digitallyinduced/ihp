module TurboHaskell.ValidationSupport.ValidateField where

import           ClassyPrelude
import           Control.Lens                         hiding ((|>))
import           Data.Generics.Product
import           Data.Generics.Product.Types
import           Data.Proxy
import           TurboHaskell.NameSupport               (humanize)
import           TurboHaskell.ValidationSupport.Types
import           GHC.Generics
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State

type Validator2 value = value -> ValidatorResult

{-# INLINE validateField #-}
validateField :: forall field validator model validationState fieldValue. (
        KnownSymbol field
        , HasField' field model fieldValue
        -- , HasField field (ValidatorResultFor model) (ValidatorResultFor model) ValidatorResult ValidatorResult
        , Generic (ValidatorResultFor model)
        , Generic model
        , HasField field (ValidatorResultFor model) (ValidatorResultFor model) ValidatorResult ValidatorResult
    ) => Proxy field -> Validator2 fieldValue -> model -> StateT (ValidatorResultFor model) IO model
validateField field validator model = do
    validationState :: ValidatorResultFor model <- get
    let value :: ValidatorResult = validator (getField @field model)
    attachValidatorResult field value
    return model

{-# INLINE validateNothing #-}
validateNothing :: forall s. StateT s IO ()
validateNothing = return ()

{-# INLINE nonEmpty #-}
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
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
