{-|
Module: IHP.View.Classes
Description: Provides the classes view helper function
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.Classes where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import qualified Text.Blaze.Html5 as Blaze
import IHP.HtmlSupport.QQ (hsx)
import IHP.HtmlSupport.ToHtml
import IHP.View.Types

-- | Helper for dynamically generating the @class=".."@ attribute.
-- 
-- Given a list like
-- 
-- > [("a", True), ("b", False), ("c", True)]
-- 
-- builds a class name string for all parts where the second value is @True@.
--
-- E.g.
--
-- >>> classes [("a", True), ("b", False), ("c", True)]
-- "a c"
--
-- When setting @b@ to @True@:
--
-- >>> classes [("a", True), ("b", True), ("c", True)]
-- "a b c"
--
-- __Example:__
-- 
-- >>> <div class={classes [("is-active", False)]}>
-- <div class="">
--
-- >>> <div class={classes [("is-active", True)]}>
-- <div class="is-active">
--
-- >>> forEach projects \project -> [hsx|
-- >>>     <div class={classes [("project", True), ("active", get #active project)]}>
-- >>>         {project}
-- >>>     </div>
-- >>> |]
-- If project is active:                        <div class="project active">{project}</div>
-- Otherwise:                                   <div class="project">{project}</div>
classes :: [(Text, Bool)] -> Text
classes !classNameBoolPairs =
    classNameBoolPairs
    |> filter snd
    |> map fst
    |> unwords
{-# INLINE classes #-}

-- | Allows `("my-class", True)` to be written as `"my-class"`
--
-- Useful together with 'classes'
instance IsString (Text, Bool) where
    fromString string = (cs string, True)