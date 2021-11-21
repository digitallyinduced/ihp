{-|
Module: IHP.Assets.ViewFunctions
Description: Functions for dealing with CSS and JS files, e.g. 'assetPath'
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Assets.ViewFunctions
( assetPath
, assetVersion
) where

import IHP.Prelude
import IHP.Assets.Types
import IHP.Controller.Context
import qualified IHP.FrameworkConfig as Config

-- | Adds a cache buster to a asset path
--
-- >>> assetPath "/keyhandlers.js"
-- "/keyhandlers.js?v=9be8995c-7055-43d9-a1b2-43e05c210271"
--
-- The asset version can be configured using the
-- @IHP_ASSET_VERSION@ environment variable.
assetPath :: (?context :: ControllerContext) => Text -> Text
assetPath assetPath = assetPath <> "?v=" <> assetVersion
{-# INLINABLE assetPath #-}

-- | Returns the assetVersion
--
-- >>> assetVersion
-- "9be8995c-7055-43d9-a1b2-43e05c210271"
--
-- The asset version can be configured using the
-- @IHP_ASSET_VERSION@ environment variable.
assetVersion :: (?context :: ControllerContext) => Text
assetVersion = ?context
    |> Config.getFrameworkConfig
    |> get #assetVersion
{-# INLINABLE assetVersion #-}