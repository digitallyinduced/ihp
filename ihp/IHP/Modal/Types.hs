{-|
Module: IHP.Modal.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.Types
( Modal (..)
, ModalContainer (..)
) where

import IHP.Prelude
import Text.Blaze.Html5 (Html)

data Modal = Modal
    { modalContent :: Html
    , modalFooter :: Maybe Html
    , modalCloseUrl :: Text
    , modalTitle :: Text
    }

-- | Stores the current modal inside @?context@
newtype ModalContainer = ModalContainer Html