{-# OPTIONS_HADDOCK not-home, hide #-}
-- | Minimal prelude for IDE controllers
--
-- This module provides a focused subset of 'IHP.ControllerPrelude' optimized
-- for faster compilation of IDE controllers. It includes only the commonly
-- needed imports for SchemaDesigner and other IDE controllers.
module IHP.IDE.Prelude
    ( module IHP.Prelude
    , module IHP.ControllerSupport
    , module IHP.Controller.Param
    , module IHP.Controller.Render
    , module IHP.Controller.Redirect
    , module IHP.Controller.Layout
    , module IHP.FlashMessages
    , module IHP.Modal.Types
    , module IHP.Modal.ControllerFunctions
    , module IHP.ValidationSupport
    ) where

import IHP.Prelude
import IHP.ControllerSupport
import IHP.Controller.Param
import IHP.Controller.Render
import IHP.Controller.Redirect
import IHP.Controller.Layout
import IHP.FlashMessages
import IHP.Modal.Types
import IHP.Modal.ControllerFunctions
import IHP.ValidationSupport
