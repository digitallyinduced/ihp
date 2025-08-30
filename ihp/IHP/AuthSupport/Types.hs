module IHP.AuthSupport.Types where

import IHP.Prelude
import Data.Data
import IHP.ModelSupport

data PasswordReset = PasswordReset
    { id :: UUID
    , email :: Text
    , meta :: MetaBag
    } deriving (Eq)

type instance GetModelName PasswordReset = "PasswordReset"

instance SetField "email" PasswordReset Text where
    {-# INLINE setField #-}
    setField newValue PasswordReset { .. } = PasswordReset { id, email = newValue, meta = meta { touchedFields = "email" : touchedFields meta } }

instance SetField "meta" PasswordReset MetaBag where
    {-# INLINE setField #-}
    setField newValue PasswordReset { .. } = PasswordReset { id, email, meta = newValue }

instance Record PasswordReset where
    newRecord = PasswordReset { id = def, email = def, meta = def }