module IHP.EnvVar
( env
, envOrDefault
, envOrNothing
, hasEnvVar
, EnvVarReader (..)
)
where

import IHP.Prelude
import Data.String.Interpolate.IsString (i)
import qualified System.Posix.Env.ByteString as Posix
import Network.Socket (PortNumber)
import IHP.Mail.Types
import IHP.Environment

-- | Returns a env variable. The raw string
-- value is parsed before returning it. So the return value type depends on what
-- you expect (e.g. can be Text, Int some custom type).
--
-- When the parameter is missing or cannot be parsed, an error is raised and
-- the app startup is aborted. Use 'envOrDefault' when you want to get a
-- default value instead of an error, or 'paramOrNothing' to get @Nothing@
-- when the env variable is missing.
--
-- You can define a custom env variable parser by defining a 'EnvVarReader' instance.
--
-- __Example:__ Accessing a env var PORT.
--
-- Let's say an env var PORT is set to 1337
--
-- > export PORT=1337
--
-- We can read @PORT@ like this:
--
-- > port <- env @Int "PORT"
--
-- __Example:__ Missing env vars
--
-- Let's say the @PORT@ env var is not defined. In that case we'll get an
-- error when trying to access it:
--
-- >>> port <- env @Int "PORT"
-- "Env var 'PORT' not set, but it's required for the app to run"
--
env :: forall result monad. (MonadIO monad) => EnvVarReader result => ByteString -> monad result
env name = envOrDefault name (error [i|Env var '#{name}' not set, but it's required for the app to run|])
{-# INLINE env #-}

envOrDefault :: (MonadIO monad) => EnvVarReader result => ByteString -> result -> monad result
envOrDefault name defaultValue = fromMaybe defaultValue <$> envOrNothing name
{-# INLINE envOrDefault #-}

envOrNothing :: (MonadIO monad) => EnvVarReader result => ByteString -> monad (Maybe result)
envOrNothing name = liftIO $ fmap parseString <$> Posix.getEnv name
    where
        parseString string = case envStringToValue string of
            Left errorMessage -> error [i|Env var '#{name}' is invalid: #{errorMessage}|]
            Right value -> value
{-# INLINE envOrNothing #-}

hasEnvVar :: (MonadIO monad) => ByteString -> monad Bool
hasEnvVar name = liftIO do
    value :: Maybe ByteString <- envOrNothing name
    pure (isJust value)
{-# INLINE hasEnvVar #-}

class EnvVarReader valueType where
    envStringToValue :: ByteString -> Either Text valueType

instance EnvVarReader Environment where
    envStringToValue "Production"  = Right Production
    envStringToValue "Development" = Right Development
    envStringToValue otherwise     = Left "Should be set to 'Development' or 'Production'"

instance EnvVarReader Int where
    envStringToValue string = case textToInt (cs string) of
        Just integer -> Right integer
        Nothing -> Left [i|Expected integer, got #{string}|]

instance EnvVarReader Text where
    envStringToValue string = Right (cs string)

instance EnvVarReader String where
    envStringToValue string = Right (cs string)

instance EnvVarReader ByteString where
    envStringToValue string = Right string

instance EnvVarReader Bool where
    envStringToValue "1"       = Right True
    envStringToValue "0"       = Right False
    envStringToValue otherwise = Left "Should be set to '1' or '0'"

-- | Allow reading the env var of an SMTP port number.
instance EnvVarReader PortNumber where
    envStringToValue string = case textToInt (cs string) of
        Just integer -> Right $ convertIntToPortNumber integer
        Nothing -> Left [i|Expected integer to be used as a Port number, got #{string}|]

convertIntToPortNumber :: Int -> PortNumber
convertIntToPortNumber int = fromIntegral (int :: Int) :: PortNumber

-- | Allow reading the env var of an SMTP encryption method.
instance EnvVarReader SMTPEncryption where
    envStringToValue "Unencrypted" = Right Unencrypted
    envStringToValue "TLS"  = Right TLS
    envStringToValue "STARTTLS"  = Right STARTTLS
    envStringToValue otherwise = Left [i|Expected 'Unencrypted', 'TLS' or 'STARTTLS', got #{otherwise}|]