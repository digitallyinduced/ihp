{-|
Module: IHP.Router.UrlGenerator
Description: Type class for URL path generation
Copyright: (c) digitally induced GmbH, 2020

Provides the 'HasPath' type class used for generating URL paths from controller actions.
This is a lightweight module with minimal dependencies, allowing modules that only need
path generation to avoid importing the full routing infrastructure.
-}
module IHP.Router.UrlGenerator
( HasPath (..)
) where

import Data.Text (Text)

-- | Type class for types that can be converted to URL paths.
--
-- This is used by IHP's routing system to generate URLs for controller actions.
--
-- __Example:__
--
-- >>> pathTo UsersAction
-- "/Users"
--
-- >>> pathTo ShowUserAction { userId = "a32913dd-ef80-4f3e-9a91-7879e17b2ece" }
-- "/ShowUser?userId=a32913dd-ef80-4f3e-9a91-7879e17b2ece"
--
class HasPath controller where
    pathTo :: controller -> Text
