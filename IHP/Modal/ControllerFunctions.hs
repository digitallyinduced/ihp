{-|
Module: IHP.Modal.ControllerFunctions
Description: Controller Helper Functions to use modals
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.ControllerFunctions (setModal) where

import IHP.Prelude
import IHP.Controller.Context
import IHP.Controller.Layout
import IHP.Modal.Types
import qualified IHP.ViewSupport as ViewSupport
import Text.Blaze.Html5 (Html)

setModal :: (?context :: ControllerContext, ViewSupport.View view) => view -> IO ()
setModal view = do
    html <- do
        context <- freeze ?context
        let ?context = context
        let ?view = view
        pure $ ViewSupport.html view
        
    putContext (ModalContainer html)