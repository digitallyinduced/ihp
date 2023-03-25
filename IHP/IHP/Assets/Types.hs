{-|
Module: IHP.Assets.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Assets.Types
( AssetVersion (..)
) where

import IHP.Prelude

-- | Stores the version appended to CSS and JS files via the @?v=..@ query parameter
--
-- __Example:__ An asset path where the @AssetVersion@ was set to @"dev"@
--
-- > "/app.css?v=dev"
-- 
newtype AssetVersion = AssetVersion Text