module IHP.PGVersion (defaultUuidFunction) where

import IHP.Prelude
import qualified System.Environment as Env
import Text.Read (readMaybe)

-- | Returns the default UUID function for new tables, jobs, and DataSync triggers.
--
-- Reads @IHP_POSTGRES_VERSION@. The default is @18@, which selects the
-- built-in @"uuidv7"@ function. A value below 18 selects @"uuid_generate_v4"@
-- from the @uuid-ossp@ extension. An unparseable value uses the default.
defaultUuidFunction :: IO Text
defaultUuidFunction = do
    pgVersion <- fromMaybe "18" <$> Env.lookupEnv "IHP_POSTGRES_VERSION"
    let version = fromMaybe 18 (readMaybe pgVersion :: Maybe Int)
    pure if version >= 18 then "uuidv7" else "uuid_generate_v4"
