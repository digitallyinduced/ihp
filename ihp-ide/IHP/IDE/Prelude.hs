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
    , setModal
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
import IHP.Modal.ControllerFunctions hiding (setModal)
import qualified IHP.Modal.ControllerFunctions as Modal
import IHP.ViewSupport (View)
import qualified IHP.ViewSupport as ViewSupport
import IHP.ValidationSupport
import qualified Network.Wai

-- | Renders a view and stores it as modal HTML in the context for later rendering.
--
-- > setModal MyModalView { .. }
--
setModal :: (?context :: ControllerContext, ?request :: Network.Wai.Request, View view) => view -> IO ()
setModal view = let ?view = view in Modal.setModal (ViewSupport.html view)
