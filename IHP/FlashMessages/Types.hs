{-|
Module: IHP.FlashMessages.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.FlashMessages.Types
( FlashMessage (..)
) where

import IHP.Prelude

data FlashMessage
    = SuccessFlashMessage !Text
    | ErrorFlashMessage !Text