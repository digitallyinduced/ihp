module IHP.IDE.CodeGen.DefaultUuidFunction (defaultUuidFunction) where

import IHP.Prelude
import qualified System.Environment as Env
import Text.Read (readMaybe)

-- | Returns the default UUID function based on the @IHP_POSTGRES_VERSION@ env var.
-- When set to 18 or higher, returns @"uuidv7"@ (PostgreSQL 18+ native function).
-- Otherwise returns @"uuid_generate_v4"@ (requires uuid-ossp extension).
defaultUuidFunction :: IO Text
defaultUuidFunction = do
    pgVersion <- fromMaybe "17" <$> Env.lookupEnv "IHP_POSTGRES_VERSION"
    let version = fromMaybe 17 (readMaybe pgVersion :: Maybe Int)
    pure if version >= 18 then "uuidv7" else "uuid_generate_v4"
