module TurboHaskell.AuthSupport.Authentication (verifyPassword, hashPassword, generateAuthenticationToken, Lockable (maxSignInAttemps)) where

import ClassyPrelude
import Data.String.Conversions (cs)
import qualified Crypto.PasswordStore
import qualified Test.RandomStrings
import GHC.Records

class Lockable entity where
    maxSignInAttemps :: entity -> Int

passwordStrength :: Int
passwordStrength = 17

{-# INLINE hashPassword #-}
hashPassword :: ByteString -> IO Text
hashPassword plainText = cs <$> Crypto.PasswordStore.makePassword plainText passwordStrength


class VerifiyPassword a where
    verifyPassword' :: a -> Text -> Bool

instance VerifiyPassword Text where
    verifyPassword' passwordHash plainText = Crypto.PasswordStore.verifyPassword (cs plainText) (cs passwordHash)

instance VerifiyPassword (Maybe Text) where
    verifyPassword' (Just passwordHash) plainText = verifyPassword' passwordHash plainText
    verifyPassword' Nothing _ = False

{-# INLINE verifyPassword #-}
verifyPassword :: (HasField "passwordHash" entity passwordField, VerifiyPassword passwordField) => entity -> Text -> Bool
verifyPassword entity plainText = verifyPassword' passwordHash plainText
    where
        passwordHash = getField @"passwordHash" entity



{-# INLINE generateAuthenticationToken #-}
generateAuthenticationToken :: IO Text
generateAuthenticationToken = cs <$> Test.RandomStrings.randomWord Test.RandomStrings.randomASCII 32