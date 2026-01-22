{-|
Module: IHP.Modal.ControllerFunctions
Description: Controller Helper Functions to use modals
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.ControllerFunctions (setModal) where

import Prelude
import Text.Blaze.Html5 (Html)
import IHP.ControllerContext (ControllerContext, putContext)
import IHP.Modal.Types

-- | Store modal HTML in the context for later rendering.
--
-- In IHP, you typically use this with a rendered view:
--
-- > setModal (html MyModalView { .. })
--
setModal :: (?context :: ControllerContext) => Html -> IO ()
setModal modalHtml = putContext (ModalContainer modalHtml)
