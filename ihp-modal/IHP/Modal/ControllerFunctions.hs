{-|
Module: IHP.Modal.ControllerFunctions
Description: Controller Helper Functions to use modals
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.ControllerFunctions (setModal) where

import Prelude
import Data.IORef (writeIORef)
import Text.Blaze.Html5 (Html)
import Network.Wai (Request)
import IHP.Modal.Types

-- | Store modal HTML in the context for later rendering.
--
-- In IHP, you typically use this with a rendered view:
--
-- > setModal (html MyModalView { .. })
--
setModal :: (?request :: Request) => Html -> IO ()
setModal modalHtml = writeIORef (lookupModalVault modalContainerVaultKey ?request) (Just (ModalContainer modalHtml))
