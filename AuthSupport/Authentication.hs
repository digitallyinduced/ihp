module Foundation.AuthSupport.Authentication (PasswordLogin (getPasswordHash), verifyPassword, hashPassword, generateAuthenticationToken, Lockable (maxSignInAttemps)) where

import ClassyPrelude
import Data.String.Conversions (cs)
import qualified Crypto.PasswordStore
import Data.String.Conversions (cs)
import qualified Test.RandomStrings

class PasswordLogin entity where
    getPasswordHash :: entity -> Text

class Lockable entity where
	maxSignInAttemps :: entity -> Int

passwordStrength :: Int
passwordStrength = 17

hashPassword :: ByteString -> IO Text
hashPassword plainText = (Crypto.PasswordStore.makePassword plainText passwordStrength) >>= return . cs

verifyPassword :: PasswordLogin entity => entity -> Text -> Bool
verifyPassword entity plainText = Crypto.PasswordStore.verifyPassword (cs plainText) (cs (getPasswordHash entity))

generateAuthenticationToken :: IO Text
generateAuthenticationToken = (Test.RandomStrings.randomWord Test.RandomStrings.randomASCII 32) >>= return . cs