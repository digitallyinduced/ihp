{-|
Module: IHP.View.Form.TypedAction
Description: Typed action form compatibility helpers

Typed GADT actions use the regular 'IHP.View.Form.FormFor.formFor' and
'IHP.View.Form.FormFor.formForAction' and
'IHP.View.Form.FormFor.formForActionWithOptions' functions:

@
formForAction UpdateProjectAction { projectId, returnTo } initialProjectInput [hsx|
    {textField #title}
    {submitButton}
|]
@
-}
module IHP.View.Form.TypedAction
    ( FormCompatibleBodyEncodings (..)
    , formForAction
    , formForActionWithOptions
    ) where

import IHP.View.Form.FormFor (FormCompatibleBodyEncodings (..), formForAction, formForActionWithOptions)
