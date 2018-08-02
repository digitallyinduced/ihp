module Foundation.ValidationSupport.ValidateField where

import           ClassyPrelude
import           Control.Lens                         hiding ((|>))
import           Data.Generics.Product
import           Data.Generics.Product.Types
import           Data.Proxy
import           Foundation.NameSupport               (humanize)
import           Foundation.ValidationSupport.Types
import           GHC.Generics
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State

type Validator2 value = value -> ValidatorResult
validateField2 :: forall field validator model validationState fieldValue. (
        ?model :: model
        , KnownSymbol field
        , HasField' field model fieldValue
        , HasField field (ValidatorResultFor model) (ValidatorResultFor model) ValidatorResult ValidatorResult
    ) => Proxy field -> Validator2 fieldValue -> StateT (ValidatorResultFor model) IO ()
validateField2 _ validator = do
    validationState :: ValidatorResultFor model <- get
    let value :: ValidatorResult = validator $ getField @field ?model
    put (validationState & ((field @field) .~ value))
    return ()

validateNothing :: forall model. (?model :: model) => StateT (ValidatorResultFor model) IO ()
validateNothing = return ()

nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "cannot be empty"
nonEmpty _ = Success

isPhoneNumber :: Text -> ValidatorResult
isPhoneNumber text | "+" `isPrefixOf` text && length text > 5 = Success
isPhoneNumber text = Failure "is not a valid phone number (has to start with +, at least 5 characters)"

isEmail :: Text -> ValidatorResult
isEmail text | "@" `isInfixOf` text = Success
isEmail text = Failure "is not a valid email"

isInRange :: (Show value, Ord value) => (value, value) -> value -> ValidatorResult
isInRange (min, max) value | value >= min && value <= max = Success
isInRange (min, max) value = Failure (" has to be between " <> tshow min <> " and " <> tshow max)
