{-|
Module: IHP.Assets.ControllerFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Assets.ControllerFunctions
( initAssetVersion
) where

import IHP.Prelude
import IHP.Assets.Types
import IHP.Controller.Context
import qualified System.Environment as Env

-- | Initializes the AssetVersion used for cache busting
--
-- On IHP Cloud IHP automatically uses the @IHP_CLOUD_CONTAINER_ID@ env variable
-- as the asset version. So when running there, you don't need to do anything.
--
-- If you deploy IHP on your own, you should provide the IHP_ASSET_VERSION
-- env variable with e.g. the git commit hash of the production build.
--
-- If IHP cannot figure out an asset version, it will fallback to the static
-- string @"dev"@.
--
initAssetVersion :: (?context :: ControllerContext) => IO ()
initAssetVersion = do
    ihpCloudContainerId <- fmap cs <$> Env.lookupEnv "IHP_CLOUD_CONTAINER_ID"
    ihpAssetVersion <- fmap cs <$> Env.lookupEnv "IHP_ASSET_VERSION"
    let assetVersion = [ ihpCloudContainerId, ihpAssetVersion]
            |> catMaybes
            |> head
            |> fromMaybe "dev"
    putContext (AssetVersion assetVersion)
