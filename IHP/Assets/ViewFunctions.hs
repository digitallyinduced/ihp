{-|
Module: IHP.Assets.ViewFunctions
Description: Functions for dealing with CSS and JS files, e.g. 'assetPath'
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Assets.ViewFunctions
( assetPath
) where

import IHP.Prelude
import IHP.Assets.Types
import IHP.Controller.Context

-- | Adds a cache buster to a asset path
--
-- >>> assetPath "/keyhandlers.js"
-- "/keyhandlers.js?v=9be8995c-7055-43d9-a1b2-43e05c210271"
--
assetPath :: (?context :: ControllerContext) => Text -> Text
assetPath assetPath = assetPath <> "?v=" <> assetVersion
    where
        (AssetVersion assetVersion) = fromFrozenContext @AssetVersion
{-# INLINABLE assetPath #-}