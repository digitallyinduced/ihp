module Network.Wai.Middleware.AssetPath
( assetPathMiddleware
, assetPathFromEnvMiddleware
, assetPath
, assetVersion
) where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Network.Wai
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import System.Environment
import Data.Maybe

-- | Middleware used for cache busting of static files
-- You need to provide an asset version and optionally a base url.
--
-- The base url is useful if you use a CDN to host your static files
assetPathMiddleware :: Text -> Maybe Text -> Middleware
assetPathMiddleware assetVersion assetBaseUrl next req respond =
    next req { vault = Vault.insert assetPathVaultKey (assetVersion, assetBaseUrl) req.vault } respond

-- | Reads the asset version and base url from env variables.
--
-- > assetPathFromEnvMiddleware "IHP_ASSET_VERSION" "IHP_ASSET_BASEURL"
--
-- If the asset version env var is undefined, it will fallback to the static string @"dev"@
--
-- The base url env variable is optional.
assetPathFromEnvMiddleware :: Text -> Text -> IO Middleware
assetPathFromEnvMiddleware versionEnv baseUrlEnv = do
    version <- maybe "dev" Text.pack <$> lookupEnv (Text.unpack versionEnv)
    baseUrl <- fmap Text.pack <$> lookupEnv (Text.unpack baseUrlEnv)
    pure (assetPathMiddleware version baseUrl)

assetPathVaultKey :: Vault.Key (Text, Maybe Text)
assetPathVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE assetPathVaultKey #-}

renderAssetPath :: Text -> Maybe Text -> Text -> Text
renderAssetPath version baseUrl path =
    Text.concat [ fromMaybe "" baseUrl, path, "?v=", version ]

-- | Adds a cache buster to a asset path
--
-- >>> assetPath request "/keyhandlers.js"
-- "/keyhandlers.js?v=9be8995c-7055-43d9-a1b2-43e05c210271"
assetPath :: Request -> Text -> Text
assetPath request path =
    case Vault.lookup assetPathVaultKey request.vault of
        Just (version, baseUrl) -> renderAssetPath version baseUrl path
        Nothing -> path

-- | Returns the assetVersion
--
-- >>> assetVersion request
-- "9be8995c-7055-43d9-a1b2-43e05c210271"
assetVersion :: Request -> Maybe Text
assetVersion request =
    fst <$> Vault.lookup assetPathVaultKey request.vault
