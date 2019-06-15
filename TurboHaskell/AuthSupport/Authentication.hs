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

hashPassword :: ByteString -> IO Text
hashPassword plainText = (Crypto.PasswordStore.makePassword plainText passwordStrength) >>= return . cs

verifyPassword :: HasField "passwordHash" entity Text => entity -> Text -> Bool
verifyPassword entity plainText = Crypto.PasswordStore.verifyPassword (cs plainText) (cs passwordHash)
	where
		passwordHash = getField @"passwordHash" entity

generateAuthenticationToken :: IO Text
generateAuthenticationToken = (Test.RandomStrings.randomWord Test.RandomStrings.randomASCII 32) >>= return . cs