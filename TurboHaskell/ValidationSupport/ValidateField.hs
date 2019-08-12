module TurboHaskell.ValidationSupport.ValidateField where

import           ClassyPrelude
import           Data.Proxy
import           TurboHaskell.NameSupport               (humanize)
import           TurboHaskell.ValidationSupport.Types
import           GHC.Generics
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import GHC.Records
import Control.Monad.State

{-# INLINE nonEmpty #-}
nonEmpty :: Text -> Either Text Text
nonEmpty "" = Left "This field cannot be empty"
nonEmpty value = Right value

{-# INLINE isPhoneNumber #-}
isPhoneNumber :: Text -> Either Text Text
isPhoneNumber text | "+" `isPrefixOf` text && length text > 5 = Right text
isPhoneNumber text = Left "is not a valid phone number (has to start with +, at least 5 characters)"

{-# INLINE isEmail #-}
isEmail :: Text -> Either Text Text
isEmail text | "@" `isInfixOf` text = Right text
isEmail text = Left "is not a valid email"

{-# INLINE isInRange #-}
isInRange :: (Show value, Ord value) => (value, value) -> value -> Either Text value
isInRange (min, max) value | value >= min && value <= max = Right value
isInRange (min, max) value = Left (" has to be between " <> tshow min <> " and " <> tshow max)
